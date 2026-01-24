import argparse
import os

os.environ["CUDA_DEVICE_ORDER"] = "PCI_BUS_ID"
os.environ["CUDA_VISIBLE_DEVICES"] = "2,3,4,5"

from be_great import GReaT
from sklearn.datasets import fetch_california_housing
import pandas as pd
import torch

if __name__ == "__main__":

    parser = argparse.ArgumentParser("Related tools")
    parser.add_argument("-device", "--device", type=str, help="Device info", default='5')
    parser.add_argument("-batch_size", "--batch_size", type=int, help="Number of batch info", default='50')
    parser.add_argument("-epochs", "--epochs", type=int, help="Number of epochs info", default='200')
    parser.add_argument("-data", "--data", type=str, help="Name of data", default='adult')

    args = parser.parse_args()
    device = args.device
    batch_size = args.batch_size
    epochs = args.epochs
    data = args.data

    ori_data = pd.read_csv(f'/data/sykim27/ITRC/original_data/{data}.csv')
    data_num = len(ori_data)
    model = GReaT(llm='distilgpt2', batch_size=batch_size, epochs=epochs, save_steps=10000)
    model.fit(ori_data)
    for i in range(1) : 
        synthetic_data = model.sample(n_samples=data_num, max_length=2000)
        synthetic_data.to_csv(f"/data/sykim27/synthetic data/GReaT/add_GReaT_house_16H.csv", index = False)
