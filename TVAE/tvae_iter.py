from pathlib import Path
import csv
import gc
import random
import subprocess
import sys
import threading
import time

import numpy as np
import pandas as pd
import torch
from tqdm.auto import tqdm

MODEL_DIR = Path("/mnt/data1/sykim27/ITRC_26/model")
sys.path.insert(0, str(MODEL_DIR))

from ctgan.synthesizers.tvae import TVAE


discrete_list = {}
discrete_list["adult"] = ["age", "workclass", "education", "marital-status", "occupation", "relationship", "race", "sex", "native-country", "income"]
discrete_list["wilt"] = ["class"]
discrete_list["insurance"] = ["age", "sex", "children", "smoker", "region"]
discrete_list["house_16H"] = []
discrete_list["intrusion_100k"] = ["protocol_type", "service", "flag", "land", "wrong_fragment", "urgent", "num_failed_logins", "logged_in", "root_shell", "su_attempted", "is_host_login", "is_guest_login", "label", "num_shells", "num_access_files", "num_outbound_cmds"]
discrete_list["credit_card"] = ["sex", "education", "marriage", "age", "pay_0", "pay_2", "pay_3", "pay_4", "pay_5", "pay_6", "default"]


INPUT_DIR = Path("/mnt/data1/sykim27/ITRC_26/splits")
OUTPUT_DIR = Path("/mnt/data1/sykim27/ITRC_26/synthetic_data/tvae")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
COMPUTE_INFO_PATH = OUTPUT_DIR / "computational_info_tvae_iter.csv"

MODEL_NAME = "tvae"
SEEDS = range(1, 31)
OVERWRITE = False

ITER_PARAMS = {
    "adult": {"epoch": 500, "batch_size": 256, "learning_rate": 1e-3},
    "wilt": {"epoch": 500, "batch_size": 256, "learning_rate": 1e-3},
    "insurance": {"epoch": 500, "batch_size": 256, "learning_rate": 1e-3},
    "house_16H": {"epoch": 500, "batch_size": 256, "learning_rate": 1e-3},
    "intrusion_100k": {"epoch": 500, "batch_size": 256, "learning_rate": 1e-3},
    "credit_card": {"epoch": 500, "batch_size": 256, "learning_rate": 1e-3},
}

files = [
    "adult",
    "wilt",
    "insurance",
    "house_16H",
    "intrusion_100k",
    "credit_card",
]


def set_all_seeds(seed):
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)

    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)

    if hasattr(torch.backends, "mps") and torch.backends.mps.is_available():
        torch.mps.manual_seed(seed)


def query_gpu_memory_mb() -> float:
    try:
        result = subprocess.run(
            [
                "nvidia-smi",
                "--query-gpu=memory.used",
                "--format=csv,noheader,nounits",
            ],
            check=True,
            capture_output=True,
            text=True,
        )
    except Exception:
        return np.nan

    values = []
    for line in result.stdout.splitlines():
        line = line.strip()
        if line:
            values.append(float(line))
    return float(sum(values)) if values else np.nan


class GPUMemoryMonitor:
    def __init__(self, interval_seconds=1.0):
        self.interval_seconds = interval_seconds
        self.baseline_mb = np.nan
        self.min_delta_mb = np.nan
        self.max_delta_mb = np.nan
        self._stop_event = threading.Event()
        self._thread = None

    def __enter__(self):
        self.baseline_mb = query_gpu_memory_mb()
        self._record()
        self._thread = threading.Thread(target=self._run, daemon=True)
        self._thread.start()
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self._stop_event.set()
        if self._thread is not None:
            self._thread.join()
        self._record()

    def _run(self):
        while not self._stop_event.wait(self.interval_seconds):
            self._record()

    def _record(self):
        current_mb = query_gpu_memory_mb()
        if np.isnan(current_mb) or np.isnan(self.baseline_mb):
            return

        delta_mb = max(current_mb - self.baseline_mb, 0.0)
        if np.isnan(self.min_delta_mb) or delta_mb < self.min_delta_mb:
            self.min_delta_mb = delta_mb
        if np.isnan(self.max_delta_mb) or delta_mb > self.max_delta_mb:
            self.max_delta_mb = delta_mb


def append_compute_info(row):
    COMPUTE_INFO_PATH.parent.mkdir(parents=True, exist_ok=True)
    file_exists = COMPUTE_INFO_PATH.exists()
    with COMPUTE_INFO_PATH.open("a", newline="") as file:
        writer = csv.DictWriter(file, fieldnames=row.keys())
        if not file_exists:
            writer.writeheader()
        writer.writerow(row)


total = len(files) * len(SEEDS)
progress = tqdm(total=total, desc="TVAE Iter")

for data_name in files:
    input_path = INPUT_DIR / f"{data_name}_train80.csv"

    if not input_path.exists():
        print(f"[건너뜀] 파일 없음: {input_path}")
        progress.update(len(SEEDS))
        continue

    original_data = pd.read_csv(input_path)
    discrete_columns = discrete_list[data_name]
    params = ITER_PARAMS[data_name]
    epoch = params["epoch"]
    batch_size = params["batch_size"]
    learning_rate = params["learning_rate"]
    lr_label = f"{learning_rate:.0e}"

    for seed in SEEDS:
        filename = (
            f"{data_name}_"
            f"{MODEL_NAME}_"
            f"{seed}_"
            f"{epoch}_"
            f"{batch_size}_"
            f"{lr_label}.csv"
        )
        output_path = OUTPUT_DIR / filename

        if output_path.exists() and not OVERWRITE:
            print(f"[존재함] {filename}")
            progress.update(1)
            continue

        set_all_seeds(seed)

        tvae_model = TVAE(
            epochs=epoch,
            batch_size=batch_size,
            learning_rate=learning_rate,
            verbose=True,
            enable_gpu=True,
        )
        tvae_model.set_random_state(seed)

        print(
            f"\n데이터={data_name} | "
            f"seed={seed} | "
            f"epoch={epoch} | "
            f"batch={batch_size} | "
            f"lr={lr_label} | "
            f"device={tvae_model._device}"
        )

        training_start = time.perf_counter()
        with GPUMemoryMonitor() as training_gpu:
            tvae_model.fit(
                original_data,
                discrete_columns=discrete_columns,
            )
        training_time_seconds = time.perf_counter() - training_start

        generation_start = time.perf_counter()
        with GPUMemoryMonitor() as generation_gpu:
            synthetic_data = tvae_model.sample(len(original_data))
        generation_time_seconds = time.perf_counter() - generation_start

        synthetic_data.to_csv(output_path, index=False)

        append_compute_info(
            {
                "dataset": data_name,
                "model": MODEL_NAME,
                "seed": seed,
                "epoch": epoch,
                "batch_size": batch_size,
                "learning_rate": learning_rate,
                "n_records": len(original_data),
                "output_file": str(output_path),
                "training_time_seconds": training_time_seconds,
                "training_gpu_min_memory_mb": training_gpu.min_delta_mb,
                "training_gpu_max_memory_mb": training_gpu.max_delta_mb,
                "generation_time_seconds": generation_time_seconds,
                "generation_gpu_min_memory_mb": generation_gpu.min_delta_mb,
                "generation_gpu_max_memory_mb": generation_gpu.max_delta_mb,
            }
        )

        del tvae_model, synthetic_data
        gc.collect()

        if torch.cuda.is_available():
            torch.cuda.empty_cache()

        if hasattr(torch, "mps") and torch.backends.mps.is_available():
            torch.mps.empty_cache()

        progress.update(1)

progress.close()

print(f"저장 위치: {OUTPUT_DIR}")
