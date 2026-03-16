import warnings
from pathlib import Path

import pandas as pd
from sklearn.exceptions import ConvergenceWarning

from SPADA import SPADA

warnings.filterwarnings("ignore", category=ConvergenceWarning)

DATA_PATH = Path("wilt.csv")
N_RUNS = 30
SAMPLE_METHOD = "nf"

DESCRIPTION = (
    "Synthetic data generation experiment for the UCI Wilt dataset "
    "using SPADA while preserving the original structural dependencies."
)

RESPONSE_WILT = """
GLCM_Pan: []
Mean_G: []
Mean_R: [Mean_G->Mean_R]
Mean_NIR: [Mean_R->Mean_NIR]
SD_Plan: [GLCM_Pan->SD_Plan, Mean_NIR->SD_Plan]
class: [GLCM_Pan->class, Mean_G->class, Mean_R->class, Mean_NIR->class, SD_Plan->class]
"""


def train_and_sample(run_idx: int, data_path: Path, n_samples: int) -> None:
    print(f"\n[{run_idx}/{N_RUNS}] Training SPADA model...")

    spada = SPADA()
    spada.fit(
        data=str(data_path),
        description=DESCRIPTION,
        response=RESPONSE_WILT,
    )

    synthetic_data = spada.sample(n_samples, method=SAMPLE_METHOD)
    synthetic_df = pd.DataFrame(synthetic_data)

    output_path = Path(f"wilt_SPADA_{run_idx}.csv")
    synthetic_df.to_csv(output_path, index=False)

    print(f"Saved: {output_path}")


def main() -> None:
    df = pd.read_csv(DATA_PATH)
    n_samples = len(df)

    for run_idx in range(1, N_RUNS + 1):
        train_and_sample(run_idx, DATA_PATH, n_samples)


if __name__ == "__main__":
    main()