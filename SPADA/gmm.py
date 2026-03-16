import numpy as np
import pandas as pd
from numba import jit
from scipy.stats import gaussian_kde
from sklearn.mixture import GaussianMixture
from sklearn.neighbors import NearestNeighbors
from copulas.bivariate import Clayton, Frank, Gumbel
from copulas.univariate import GaussianKDE


def safe_gmm(values, n_components=3):
    values = np.asarray(values, dtype=float).reshape(-1, 1)

    if len(values) < n_components:
        return None

    try:
        gmm = GaussianMixture(n_components=n_components)
        gmm.fit(values)
        return gmm
    except Exception as e:
        print(f"GMM fitting failed: {e}")
        return None


def safe_kde(values, bw_method="scott"):
    values = np.asarray(values, dtype=float)

    if len(values) < 3:
        return None

    values = values + np.random.normal(0, 1e-3, size=len(values))

    try:
        return gaussian_kde(values, bw_method=bw_method)
    except np.linalg.LinAlgError:
        return None


def adaptive_kde(values, n_neighbors=10):
    values = np.asarray(values, dtype=float).reshape(-1, 1)

    if len(values) < 3:
        return None

    neighbors = NearestNeighbors(
        n_neighbors=n_neighbors,
        algorithm="ball_tree",
    ).fit(values)
    distances, _ = neighbors.kneighbors(values)
    bandwidth = np.mean(distances[:, -1])

    try:
        return gaussian_kde(values.T, bw_method=bandwidth)
    except Exception as e:
        print(f"Adaptive KDE fitting failed: {e}")
        return None


def safe_copula(values, copula_type="clayton"):
    values = np.asarray(values, dtype=float)

    if len(values) < 3:
        return None

    values = values + np.random.normal(0, 1e-3, size=len(values))

    try:
        kde = GaussianKDE()
        kde.fit(values)
        transformed_values = kde.cdf(values)

        if copula_type == "clayton":
            copula = Clayton()
        elif copula_type == "gumbel":
            copula = Gumbel()
        elif copula_type == "frank":
            copula = Frank()
        else:
            raise ValueError(f"Unknown copula type: {copula_type}")

        copula.fit(transformed_values.reshape(-1, 1))

        return lambda x: copula.probability_density(
            kde.cdf(np.asarray(x)).reshape(-1, 1)
        )

    except Exception as e:
        print(f"Copula fitting failed: {e}")
        return None


@jit()
def uni(probabilities, discrete_values, precision):
    probabilities /= probabilities.sum()
    decimals = int(-np.log10(precision))
    return {
        round(val, decimals): prob
        for val, prob in zip(discrete_values, probabilities)
    }


def estimate_conditional_distribution_KDE(
    df,
    num_key,
    str_key,
    conditions,
    num_pre_dis,
    target_key,
    N=5,
    trees=None,
    method="kde",
    bw_method="scott",
):
    num_df_values = {key: df[key].to_numpy() for key in num_key}
    str_df_values = {
        key: df[key].astype(str).str.strip().to_numpy()
        for key in str_key
    }

    condition_mask = np.ones(df.shape[0], dtype=bool)

    for key, value in conditions.items():
        if key in num_key:
            df_values = num_df_values[key]
            precision = num_pre_dis[key]["precision"]
            value = float(value)

            if trees:
                _, indices = trees[key].query([[value]], k=50)
                condition_mask &= np.isin(np.arange(len(df)), indices[0])
            else:
                condition_mask &= np.abs(df_values - value) <= N * precision

        elif key in str_key:
            value = str(value).strip()
            condition_mask &= str_df_values[key] == value

        else:
            print(f"Invalid condition key: {key}")
            return False

    filtered_df = df[condition_mask]
    if len(filtered_df) == 0:
        return {}

    if target_key in str_key:
        return filtered_df[target_key].value_counts(normalize=True).to_dict()

    if target_key in num_key:
        params = num_pre_dis[target_key]
        min_val = params["min"]
        max_val = params["max"]
        precision = params["precision"]

        values = filtered_df[target_key].dropna().to_numpy()
        if len(values) == 0:
            return {}

        if method == "kde":
            estimator = safe_kde(values, bw_method=bw_method)
        elif method == "adaptive_kde":
            estimator = adaptive_kde(values)
        elif method == "gmm":
            estimator = safe_gmm(values)
        else:
            raise ValueError(f"Unknown method type: {method}")

        if estimator is None:
            return None

        num_samples = int((max_val - min_val) / precision) if precision > 0 else 500
        num_samples = max(50, min(500, num_samples))
        discrete_values = np.linspace(min_val, max_val, num_samples)

        probabilities = estimator(discrete_values)
        return uni(probabilities, discrete_values, precision)

    raise ValueError(f"'{target_key}' is not in num_key or str_key.")


def estimate_gmm_probabilities(
    samples,
    params,
    n_components=3,
    n_points=5000,
    q_low=0.0005,
    q_high=0.9995,
    ensure_endpoints=True,
):
    """
    Estimate a marginal distribution using a GMM and evaluate it on a
    quantile-based grid for better long-tail coverage.

    Returns
    -------
    dict
        Mapping from x values to normalized probabilities.
    """
    min_val = params["min"]
    max_val = params["max"]

    samples = np.asarray(samples, dtype=float).reshape(-1, 1)
    samples_1d = samples.ravel()

    gmm = GaussianMixture(n_components=n_components, random_state=42)
    gmm.fit(samples)

    n_points = int(max(50, n_points))
    quantiles = np.linspace(q_low, q_high, n_points)

    x_values = np.quantile(samples_1d, quantiles)
    x_values = np.clip(x_values, min_val, max_val)
    x_values = np.unique(x_values)

    if ensure_endpoints:
        x_values = np.unique(np.concatenate(([min_val], x_values, [max_val])))

    densities = np.exp(gmm.score_samples(x_values.reshape(-1, 1)))
    densities = np.nan_to_num(densities, nan=0.0, posinf=0.0, neginf=0.0)

    total = densities.sum()
    if total <= 0:
        probabilities = np.ones_like(densities) / len(densities)
    else:
        probabilities = densities / total

    return dict(zip(x_values.astype(float), probabilities.astype(float)))


def estimate_initiate_pro_distribution_for_num_features(
    df,
    num_keys,
    num_pre_dis,
    verbose=False,
    n_components=3,
):
    if isinstance(df, str):
        df = pd.read_csv(df)

    num_pro_dis = {}
    for key in num_keys:
        if verbose:
            print(f"Estimating distribution for: {key}")

        num_pro_dis[key] = estimate_gmm_probabilities(
            samples=df[key].astype(float).to_numpy(),
            params=num_pre_dis[key],
            n_components=n_components,
            n_points=5000,
        )

    return num_pro_dis
