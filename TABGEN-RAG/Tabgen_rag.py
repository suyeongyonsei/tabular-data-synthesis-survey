import os
import re
import io
import csv
import random
import time
import argparse
from typing import List, Tuple, Dict

import numpy as np
import pandas as pd
from openai import OpenAI, RateLimitError, APIError


MODEL_NAME = "gpt-4.1-mini"


def load_csv_with_schema(
    path: str,
) -> Tuple[pd.DataFrame, List[str], Dict[str, str], Dict[str, set]]:
    df = pd.read_csv(path)
    cols = list(df.columns)

    schema: Dict[str, str] = {}
    cat_allowed: Dict[str, set] = {}

    for c in cols:
        if pd.api.types.is_numeric_dtype(df[c]):
            schema[c] = "numeric"
        else:
            schema[c] = "categorical"
            vals = df[c].astype(str).fillna("").str.strip()
            cat_allowed[c] = set(vals.unique().tolist())

    return df, cols, schema, cat_allowed


def quantile_bin_codes(x: pd.Series, q: int) -> pd.Series:
    x_num = pd.to_numeric(x, errors="coerce")
    if x_num.dropna().nunique() < 2:
        return pd.Series([pd.NA] * len(x_num), index=x.index, dtype="Int64")

    try:
        binned = pd.qcut(x_num, q=q, labels=False, duplicates="drop")
        return pd.Series(binned, index=x.index).astype("Int64")
    except Exception:
        return pd.Series([pd.NA] * len(x_num), index=x.index, dtype="Int64")


def quantile_hist_counts(x: pd.Series, q: int) -> np.ndarray:
    codes = quantile_bin_codes(x, q=q).dropna()
    if codes.empty:
        return np.zeros(0, dtype=float)

    k = int(codes.max()) + 1
    counts = np.zeros(k, dtype=float)
    for i in range(k):
        counts[i] = float((codes == i).sum())
    return counts


def _safe_probs(counts: np.ndarray) -> np.ndarray:
    counts = np.asarray(counts, dtype=float)
    s = counts.sum()
    if s <= 0:
        return np.ones_like(counts, dtype=float) / max(len(counts), 1)
    return counts / s


def _jsd(p: np.ndarray, q: np.ndarray, eps: float = 1e-12) -> float:
    p = np.asarray(p, dtype=float)
    q = np.asarray(q, dtype=float)

    L = max(len(p), len(q))
    if len(p) < L:
        p = np.pad(p, (0, L - len(p)))
    if len(q) < L:
        q = np.pad(q, (0, L - len(q)))

    p = p / max(p.sum(), eps)
    q = q / max(q.sum(), eps)
    m = 0.5 * (p + q)

    def _kl(a, b):
        a = np.clip(a, eps, 1.0)
        b = np.clip(b, eps, 1.0)
        return float(np.sum(a * np.log(a / b)))

    return 0.5 * _kl(p, m) + 0.5 * _kl(q, m)


def _tvd(p: np.ndarray, q: np.ndarray) -> float:
    p = np.asarray(p, dtype=float)
    q = np.asarray(q, dtype=float)

    L = max(len(p), len(q))
    if len(p) < L:
        p = np.pad(p, (0, L - len(p)))
    if len(q) < L:
        q = np.pad(q, (0, L - len(q)))

    p = p / max(p.sum(), 1e-12)
    q = q / max(q.sum(), 1e-12)
    return float(0.5 * np.sum(np.abs(p - q)))


def _col_jsd_numeric_quantile(real_col: pd.Series, cand_col: pd.Series, num_bins: int = 10) -> float:
    hx = quantile_hist_counts(real_col, q=num_bins)
    hy = quantile_hist_counts(cand_col, q=num_bins)
    if len(hx) == 0 or len(hy) == 0:
        return 0.0
    return _jsd(_safe_probs(hx), _safe_probs(hy))


def _col_jsd_categorical(real_col: pd.Series, cand_col: pd.Series) -> float:
    xr = real_col.astype(str).fillna("").values
    yc = cand_col.astype(str).fillna("").values
    cats = pd.Index(pd.unique(xr)).union(pd.Index(pd.unique(yc)))
    if len(cats) == 0:
        return 0.0

    rc = pd.Series(xr).value_counts().reindex(cats, fill_value=0).values
    yc2 = pd.Series(yc).value_counts().reindex(cats, fill_value=0).values
    return _jsd(_safe_probs(rc), _safe_probs(yc2))


def _col_ks_numeric(real_col: pd.Series, cand_col: pd.Series) -> float:
    x = pd.to_numeric(real_col, errors="coerce").dropna().astype(float).values
    y = pd.to_numeric(cand_col, errors="coerce").dropna().astype(float).values

    if len(x) == 0 or len(y) == 0:
        return 0.0

    x = np.sort(x)
    y = np.sort(y)
    n = len(x)
    m = len(y)

    i = j = 0
    cdf_x = cdf_y = 0.0
    d = 0.0

    while i < n and j < m:
        if x[i] <= y[j]:
            v = x[i]
            while i < n and x[i] == v:
                i += 1
            cdf_x = i / n
        else:
            v = y[j]
            while j < m and y[j] == v:
                j += 1
            cdf_y = j / m
        d = max(d, abs(cdf_x - cdf_y))

    d = max(d, abs(1.0 - cdf_y))
    d = max(d, abs(cdf_x - 1.0))
    return float(d)


def _col_k_like_for_categorical(real_col: pd.Series, cand_col: pd.Series) -> float:
    xr = real_col.astype(str).fillna("").values
    yc = cand_col.astype(str).fillna("").values
    cats = pd.Index(pd.unique(xr)).union(pd.Index(pd.unique(yc)))
    if len(cats) == 0:
        return 0.0

    rc = pd.Series(xr).value_counts().reindex(cats, fill_value=0).values
    yc2 = pd.Series(yc).value_counts().reindex(cats, fill_value=0).values
    return _tvd(rc, yc2)


def distance_dataset(
    real_df: pd.DataFrame,
    cand_df: pd.DataFrame,
    cols: List[str],
    schema: Dict[str, str],
    metric: str,
    num_bins: int = 10,
) -> float:
    vals = []

    for c in cols:
        if schema.get(c) == "numeric":
            if metric == "JSD":
                vals.append(_col_jsd_numeric_quantile(real_df[c], cand_df[c], num_bins=num_bins))
            else:
                vals.append(_col_ks_numeric(real_df[c], cand_df[c]))
        else:
            if metric == "JSD":
                vals.append(_col_jsd_categorical(real_df[c], cand_df[c]))
            else:
                vals.append(_col_k_like_for_categorical(real_df[c], cand_df[c]))

    return float(np.mean(vals)) if vals else 0.0


def compute_residual_samples_tabgenrag_heuristic_fill(
    real_df: pd.DataFrame,
    synth_df: pd.DataFrame,
    cols: List[str],
    schema: Dict[str, str],
    n_residual: int = 100,
    num_bins: int = 10,
    iteration: int = 1,
    min_group_size: int = 1,
) -> pd.DataFrame:
    if len(real_df) == 0:
        return real_df.copy()

    if synth_df is None or len(synth_df) == 0:
        return real_df.sample(n=min(n_residual, len(real_df)), random_state=iteration)

    rng = np.random.default_rng(iteration)
    chosen_col = rng.choice(cols)
    metric = "JSD" if (iteration % 2 == 0) else "KSD"

    if schema.get(chosen_col) == "numeric":
        codes = quantile_bin_codes(real_df[chosen_col], q=num_bins)
        tmp = real_df.assign(_grp=codes).dropna(subset=["_grp"])
        grouped = list(tmp.groupby("_grp"))
    else:
        keys = real_df[chosen_col].astype(str).fillna("").str.strip()
        tmp = real_df.assign(_grp=keys)
        grouped = list(tmp.groupby("_grp"))

    group_scores = []
    for _, gdf in grouped:
        gdf2 = gdf[cols]
        if len(gdf2) < min_group_size:
            continue

        cand = pd.concat([synth_df[cols], gdf2], ignore_index=True)
        dval = distance_dataset(
            real_df=real_df[cols],
            cand_df=cand[cols],
            cols=cols,
            schema=schema,
            metric=metric,
            num_bins=num_bins,
        )
        group_scores.append((float(dval), gdf2))

    if not group_scores:
        return real_df.sample(n=min(n_residual, len(real_df)), random_state=iteration)

    group_scores.sort(key=lambda x: x[0])

    selected_parts = []
    remaining = min(n_residual, len(real_df))
    used_index = set()
    rs = iteration

    for _, gdf2 in group_scores:
        if remaining <= 0:
            break

        gdf_available = gdf2.loc[~gdf2.index.isin(used_index)]
        if gdf_available.empty:
            continue

        take = min(remaining, len(gdf_available))
        part = gdf_available.sample(n=take, random_state=rs)
        selected_parts.append(part)
        used_index.update(part.index.tolist())
        remaining -= take
        rs += 1

    if remaining > 0:
        rest_pool = real_df[cols].loc[~real_df.index.isin(used_index)]
        if not rest_pool.empty:
            take = min(remaining, len(rest_pool))
            part = rest_pool.sample(n=take, random_state=rs)
            selected_parts.append(part)

    if not selected_parts:
        return real_df.sample(n=min(n_residual, len(real_df)), random_state=iteration)

    out = pd.concat(selected_parts, axis=0).reset_index(drop=True)
    if len(out) > n_residual:
        out = out.iloc[:n_residual].copy()

    return out


def df_to_csv_no_header(df: pd.DataFrame, cols: List[str]) -> str:
    lines = []
    for _, row in df.iterrows():
        lines.append(",".join(str(row[c]) for c in cols))
    return "\n".join(lines)


def build_tabgen_rag_prompt(
    residual_samples: pd.DataFrame,
    cols: List[str],
    rows_to_generate: int,
) -> str:
    real_samples_block = df_to_csv_no_header(residual_samples, cols)
    cols_line = ",".join(cols)

    return f"""You are a synthetic data generator tasked with creating new tabular data samples that closely mirror the distribution and characteristics of the original dataset.

# Instruction
1. Analyze the provided real samples carefully.
2. Generate synthetic data that maintains the statistical properties of the real data.
3. Ensure all attributes cover their full expected ranges, including less common or extreme values.
4. Maintain the relationships and correlations between different attributes.
5. Preserve the overall distribution of the real data while introducing realistic variations.

# Key points to consider
- Replicate the data types of each column.
- Match the range and distribution of numerical attributes.
- Maintain the frequency distribution of categorical attributes.
- Reflect patterns and trends present in the original data.
- Introduce realistic variability to avoid exact duplication.

# Columns
{cols_line}

# Real samples (CSV format, no header)
{real_samples_block}

# Output format
- Return CSV only.
- Do not include headers.
- Keep the same column order.
- Generate {rows_to_generate} rows.
"""


def call_llm_with_retry(
    client: OpenAI,
    prompt: str,
    model: str,
    max_output_tokens: int = 100000,
    max_retries: int = 8,
) -> str:
    for attempt in range(1, max_retries + 1):
        try:
            response = client.responses.create(
                model=model,
                input=prompt,
                max_output_tokens=max_output_tokens,
            )
            return response.output_text

        except RateLimitError as e:
            msg = str(e)
            m = re.search(r"try again in (\d+)s", msg)
            sleep_seconds = int(m.group(1)) + 1 if m else 5 * attempt
            print(f"[RateLimitError] attempt {attempt}/{max_retries} -> sleep {sleep_seconds}s")
            time.sleep(sleep_seconds)

        except APIError as e:
            sleep_seconds = 5 * attempt
            print(f"[APIError] attempt {attempt}/{max_retries}: {e} -> sleep {sleep_seconds}s")
            time.sleep(sleep_seconds)

    raise RuntimeError("OpenAI API 호출이 반복적으로 실패했습니다.")


def parse_synthetic_csv(
    raw_text: str,
    cols: List[str],
    schema: Dict[str, str],
    cat_allowed: Dict[str, set],
) -> pd.DataFrame:
    text = raw_text.strip()
    text = re.sub(r"```[a-zA-Z]*", "", text)
    text = text.replace("```", "").strip()

    reader = csv.reader(io.StringIO(text))
    parsed_rows = []
    k = len(cols)

    for row in reader:
        if not row:
            continue

        cells = [c.strip() for c in row]
        if all(c == "" for c in cells):
            continue

        if len(cells) < k:
            continue
        if len(cells) > k:
            cells = cells[: k - 1] + [",".join(cells[k - 1 :]).strip()]

        parsed_rows.append(cells)

    if not parsed_rows:
        return pd.DataFrame(columns=cols)

    df = pd.DataFrame(parsed_rows, columns=cols)

    for c in cols:
        if schema.get(c) == "numeric":
            df[c] = pd.to_numeric(df[c], errors="coerce")
        else:
            df[c] = df[c].astype(str).fillna("").str.strip()

    numeric_cols = [c for c in cols if schema.get(c) == "numeric"]
    if numeric_cols:
        df = df.dropna(subset=numeric_cols).copy()

    cat_cols = [c for c in cols if schema.get(c) == "categorical"]
    for c in cat_cols:
        allowed = cat_allowed.get(c)
        if allowed is not None:
            df = df[df[c].isin(allowed)].copy()

    for c in numeric_cols:
        if len(df) > 0 and (df[c] % 1 == 0).mean() > 0.9:
            df[c] = df[c].round().astype(int)

    return df[cols]


def tabgen_rag_generate_one_dataset(
    real_df: pd.DataFrame,
    cols: List[str],
    schema: Dict[str, str],
    cat_allowed: Dict[str, set],
    num_synth: int,
    batch_size: int = 50,
    n_residual: int = 100,
    num_bins: int = 10,
    model: str = MODEL_NAME,
    max_output_tokens: int = 100000,
    max_add_per_iter: int = 100,
) -> pd.DataFrame:
    client = OpenAI()

    synthetic_df = pd.DataFrame(columns=cols)
    batches = []
    iteration = 0

    while len(synthetic_df) < num_synth:
        iteration += 1
        have = len(synthetic_df)
        need_total = num_synth - have

        print(f"\n=== Iteration {iteration} | {have}/{num_synth} rows generated ===")

        if iteration == 1 and have == 0:
            residual_samples = real_df.sample(
                n=min(n_residual, len(real_df)),
                random_state=iteration,
            )
        else:
            residual_samples = compute_residual_samples_tabgenrag_heuristic_fill(
                real_df=real_df,
                synth_df=synthetic_df,
                cols=cols,
                schema=schema,
                n_residual=n_residual,
                num_bins=num_bins,
                iteration=iteration,
            )

        rows_to_generate = min(batch_size, need_total)
        prompt = build_tabgen_rag_prompt(residual_samples, cols, rows_to_generate)

        raw_text = call_llm_with_retry(
            client=client,
            prompt=prompt,
            model=model,
            max_output_tokens=max_output_tokens,
        )

        batch_df = parse_synthetic_csv(raw_text, cols, schema, cat_allowed)

        if batch_df.empty:
            print("No valid rows parsed. Retrying in next iteration.")
            continue

        max_can_add = min(need_total, max_add_per_iter)
        if len(batch_df) > max_can_add:
            batch_df = batch_df.iloc[:max_can_add].copy()

        print(f"Added {len(batch_df)} valid rows.")

        synthetic_df = pd.concat([synthetic_df, batch_df], ignore_index=True)
        batches.append(batch_df)

    return pd.concat(batches, ignore_index=True).reset_index(drop=True)


def parse_args():
    parser = argparse.ArgumentParser(
        description="Generate synthetic tabular datasets using TABGEN-RAG style prompting."
    )

    parser.add_argument("--data_path", type=str, required=True, help="Path to input CSV file")
    parser.add_argument("--num_datasets", type=int, default=30, help="Number of synthetic datasets to generate")
    parser.add_argument("--n_residual", type=int, default=100, help="Number of real residual samples per call")
    parser.add_argument("--batch_size", type=int, default=100, help="Rows requested per LLM call")
    parser.add_argument("--num_bins", type=int, default=10, help="Number of quantile bins for numeric grouping")
    parser.add_argument("--max_add_per_iter", type=int, default=100, help="Maximum number of rows added per iteration")
    parser.add_argument("--model", type=str, default=MODEL_NAME, help="OpenAI model name")
    parser.add_argument("--max_output_tokens", type=int, default=100000, help="Maximum output tokens per API call")
    parser.add_argument("--seed_base", type=int, default=42, help="Base random seed")
    parser.add_argument("--output_dir", type=str, default=".", help="Directory to save generated CSV files")
    parser.add_argument("--file_start_index", type=int, default=1, help="Starting index for output file names")

    return parser.parse_args()


def main():
    args = parse_args()

    os.makedirs(args.output_dir, exist_ok=True)

    real_df, cols, schema, cat_allowed = load_csv_with_schema(args.data_path)
    n_real = len(real_df)
    dataset_stem = os.path.splitext(os.path.basename(args.data_path))[0]

    print(f"Loaded: {args.data_path}")
    print(f"Rows: {n_real}, Columns: {len(cols)}")
    print(f"Column names: {cols}")

    for i in range(args.num_datasets):
        dataset_idx = args.file_start_index + i
        seed = args.seed_base + dataset_idx

        print("\n" + "#" * 40)
        print(f"Generating dataset {i + 1}/{args.num_datasets} (file index={dataset_idx})")
        print("#" * 40)

        random.seed(seed)
        np.random.seed(seed)

        syn = tabgen_rag_generate_one_dataset(
            real_df=real_df,
            cols=cols,
            schema=schema,
            cat_allowed=cat_allowed,
            num_synth=n_real,
            batch_size=args.batch_size,
            n_residual=args.n_residual,
            num_bins=args.num_bins,
            model=args.model,
            max_output_tokens=args.max_output_tokens,
            max_add_per_iter=args.max_add_per_iter,
        )

        out_path = os.path.join(
            args.output_dir,
            f"{dataset_stem}_tabgenrag_{dataset_idx}.csv"
        )
        syn.to_csv(out_path, index=False, encoding="utf-8-sig")
        print(f"Saved: {out_path}")


if __name__ == "__main__":
    main()