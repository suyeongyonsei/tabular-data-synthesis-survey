import warnings
from pathlib import Path

import pandas as pd
from sklearn.exceptions import ConvergenceWarning

from SPADA import SPADA

warnings.filterwarnings("ignore", category=ConvergenceWarning)

DATA_PATH = Path("house_16H.csv")
N_RUNS = 30
SAMPLE_METHOD = "nf"

DESCRIPTION = (
    "Synthetic data generation experiment for the House_16H dataset "
    "using SPADA while preserving the original structural dependencies."
)

RESPONSE_HOUSE16H = """
P1: []
P5p1: []
P6p2: []
P11p4: []

P14p9: [P11p4->P14p9, P5p1->P14p9]

P15p3: [P11p4->P15p3]
P15p1: [P15p3->P15p1, P11p4->P15p1]

P16p2: [P15p1->P16p2, P11p4->P16p2]
P18p2: [P15p1->P18p2, P11p4->P18p2]
P27p4: [P15p1->P27p4, P11p4->P27p4]

H8p2: [P6p2->H8p2]
H10p1: []
H13p1: []
H2p2: [P1->H2p2, P15p1->H2p2]
H18pA: [P15p1->H18pA, P11p4->H18pA]
H40p4: [H2p2->H40p4]

price: [P1->price, P11p4->price, P6p2->price, P15p1->price, P15p3->price, P14p9->price, H13p1->price, H2p2->price, H18pA->price, H8p2->price]
"""


def train_and_sample(run_idx: int, data_path: Path, n_samples: int) -> None:
    print(f"\n[{run_idx}/{N_RUNS}] Training SPADA model...")

    spada = SPADA()
    spada.fit(
        data=str(data_path),
        description=DESCRIPTION,
        response=RESPONSE_HOUSE16H,
    )

    synthetic_data = spada.sample(n_samples, method=SAMPLE_METHOD)
    synthetic_df = pd.DataFrame(synthetic_data)

    output_path = Path(f"house_16H_SPADA_{run_idx}.csv")
    synthetic_df.to_csv(output_path, index=False)

    print(f"Saved: {output_path}")


def main() -> None:
    df = pd.read_csv(DATA_PATH)
    n_samples = len(df)

    for run_idx in range(1, N_RUNS + 1):
        train_and_sample(run_idx, DATA_PATH, n_samples)


if __name__ == "__main__":
    main()