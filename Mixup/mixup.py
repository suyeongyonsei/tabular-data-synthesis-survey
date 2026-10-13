import os
import argparse

from tqdm import tqdm

import numpy as np
import pandas as pd

import epbeta


def none_or_float(value):
    if value.lower() == 'none':
        return None
    try:
        return float(value)
    except ValueError:
        raise argparse.ArgumentTypeError(f"{value} is not a valid float or None value")


parser = argparse.ArgumentParser(description='Synthesize data using Mixup method with EpBeta distribution.')

parser.add_argument('path', help='path to original data')

parser.add_argument('--syn_folder_path', default=None, help='folder path for synthetic data')

parser.add_argument('--syn_prefix', default="", help='prefix for synthetic data.')
parser.add_argument('--syn_suffix', default="_", help='suffix for synthetic data (default: "_")')

parser.add_argument('--syn_instance', default=None, type=int, help='number of synthetic instances (default: same as original)')
parser.add_argument('--syn_n_data', default=5, type=int, help='number of synthetic datasets (default: 5)')

parser.add_argument('--seed', default=1234, type=int, help='seed for random synethesis (default: 1234)')

parser.add_argument('--epsilon_0', default=0.3, type=float, help='epsilon_0 parameter for EpBeta distribution (default: 0.3, non-negative)')
parser.add_argument('--epsilon_1', default=0.3, type=float, help='epsilon_1 parameter for EpBeta distribution (default: 0.3, non-negative)')
parser.add_argument('--delta', default=0.01, type=none_or_float, help='delta value for determining EpBeta parameters (default: 0.01)')

parser.add_argument('--alpha', default=None, type=float, help='alpha parameter for Beta distribution, delta must be None (default: None, non-negative)')
parser.add_argument('--beta', default=None, type=float, help='beta parameter for Beta distribution, delta must be None (default: None, non-negative)')


def mixup(df, weights, seed=None, fix_left_org=False):
    col_numeric = df.select_dtypes(include=['number']).columns

    size = len(weights)

    weight_matrix = np.repeat(weights.reshape((-1,1)), len(col_numeric), axis=1)

    if fix_left_org:
        mixed_x1 = df.copy()
    else:
        mixed_x1 = df.sample(n=size, replace=True, random_state=seed).reset_index(drop=True)
    mixed_x2 = df.sample(n=size, replace=True, random_state=seed).reset_index(drop=True)

    # Synthesize numeric columns
    syn_df_numeric = mixed_x1[col_numeric]*weight_matrix + mixed_x2[col_numeric]*(1-weight_matrix)

    # Synthesize categorical columns
    mixed_x1.drop(col_numeric, axis=1, inplace=True)
    mixed_x2.drop(col_numeric, axis=1, inplace=True)

    mixed_x1[weights<0.5] = mixed_x2[weights<0.5]

    return pd.concat([syn_df_numeric, mixed_x1], axis = 1)[df.columns]


def main():
    args = parser.parse_args()

    # path and fine name
    if args.syn_folder_path is None:
        syn_path = os.path.dirname(args.path)
    else:
        syn_path = args.syn_folder_path.rstrip('/')
        if not os.path.exists(syn_path):
            os.makedirs(syn_path)

    syn_path += "/" + args.syn_prefix + os.path.basename(args.path).rstrip('.csv') + args.syn_suffix

    # load original data
    df_org = pd.read_csv(args.path)

    if args.syn_instance is None:
        args.syn_instance = df_org.shape[0]

    # print paramters
    epbeta.generate_epbeta_weight(
            size=1,
            epsilon_0=args.epsilon_0, epsilon_1=args.epsilon_1,
            delta=args.delta,
            alpha=args.alpha, beta=args.beta,
            verbos=True
            )

    # synthesize
    np.random.seed(args.seed)
    for cnt in tqdm(range(args.syn_n_data), desc=f"Synthesize {os.path.basename(args.path)}"):
        weights = epbeta.generate_epbeta_weight(
            size=args.syn_instance,
            epsilon_0=args.epsilon_0, epsilon_1=args.epsilon_1,
            delta=args.delta,
            alpha=args.alpha, beta=args.beta
            )

        df_syn = mixup(df_org, weights, seed=None, fix_left_org=False)
        df_syn.to_csv(syn_path+str(cnt)+".csv", index=False)


if __name__ == "__main__":
    main()