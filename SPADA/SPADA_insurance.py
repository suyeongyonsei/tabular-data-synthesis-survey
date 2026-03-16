import warnings
from pathlib import Path

import pandas as pd
from sklearn.exceptions import ConvergenceWarning

from SPADA import SPADA

warnings.filterwarnings("ignore", category=ConvergenceWarning)

DATA_PATH = Path("insurance.csv")
N_RUNS = 30
SAMPLE_METHOD = "nf"

DESCRIPTION = (
    "Synthetic data generation experiment for the Kaggle insurance dataset "
    "using SPADA while preserving the original structural dependencies."
)

RESPONSE_INSURANCE = """
age: []
sex: []
bmi: [age->bmi, sex->bmi]
children: [age->children]
smoker: [age->smoker, sex->smoker, bmi->smoker]
region: []
charges: [age->charges, sex->charges, bmi->charges, children->charges, smoker->charges, region->charges]
"""


def train_and_sample(run_idx: int, data_path: Path, n_samples: int) -> None:
    print(f"\n[{run_idx}/{N_RUNS}] Training SPADA model...")

    spada = SPADA()
    spada.fit(
        data=str(data_path),
        description=DESCRIPTION,
        response=RESPONSE_INSURANCE,
    )

    synthetic_data = spada.sample(n_samples, method=SAMPLE_METHOD)
    synthetic_df = pd.DataFrame(synthetic_data)

    output_path = Path(f"insurance_SPADA_{run_idx}.csv")
    synthetic_df.to_csv(output_path, index=False)

    print(f"Saved: {output_path}")


def main() -> None:
    df = pd.read_csv(DATA_PATH)
    n_samples = len(df)

    for run_idx in range(1, N_RUNS + 1):
        train_and_sample(run_idx, DATA_PATH, n_samples)


if __name__ == "__main__":
    main()