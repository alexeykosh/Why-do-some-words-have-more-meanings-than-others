import numpy as np
import pandas as pd
from scipy.stats import spearmanr
import matplotlib.pyplot as plt
from tqdm.auto import tqdm, trange
from pingouin import partial_corr
import pickle
import os

plt.rcParams.update({'font.size': 20})

S = 600
N = 305

if __name__ == '__main__':
    # All possible pairs of parameters
    a = np.arange(0.01, 0.1, 0.02)
    b = np.arange(0.01, 0.1, 0.02)  
    pairs = np.array(np.meshgrid(a, b)).T.reshape(-1, 2)

    spearman_a_m = []
    spearman_f_m = []
    pcorr_f_m = []
    pcorr_a_m = []

    for pair in pairs:
        mu = pair[0]
        k = pair[1]
        # !python3.10 src/NeutralModel2.py --mu={mu} --k={k} --S=2000 --N=1670 --p=10000 --t=100
        # execute a shell command from python using os.system()
        os.system(f'python3 src/NeutralModel.py --mu={mu} --k={k} --S={S} --N={N} --p=10000 --t=100') # preserve the wordnet ratio
        # read the output from NeutralModel2.py
        data = pd.read_csv('data/model-data/model-N{N}_S{S}_p{p}.csv'.format(S=S, N=N, p=10000))
        data = data.rename(columns={'extinct ': 'extinct'})
        data99 = data.query('step == 99 and extinct != 1')
        data99['frequency'] = data99['count'] / data99['count'].sum()
        spearman_a_m.append(spearmanr(data99['age'], data99['meanings'])[0])
        spearman_f_m.append(spearmanr(data99['frequency'], data99['meanings'])[0])
        pcorr_f_m.append(partial_corr(data99, x='frequency', y='meanings', covar='age', method='spearman')['r'].values[0])
        pcorr_a_m.append(partial_corr(data99, x='frequency', y='age', covar='meanings', method='spearman')['r'].values[0])

    with open('data/heatmap-pkl/spearman_a_m.pkl', 'wb') as f:
        pickle.dump(spearman_a_m, f)

    with open('data/heatmap-pkl/spearman_f_m.pkl', 'wb') as f:
        pickle.dump(spearman_a_m, f)

    with open('data/heatmap-pkl/pcorr_f_m.pkl', 'wb') as f:
        pickle.dump(pcorr_f_m, f)

    with open('data/heatmap-pkl/pcorr_a_m.pkl', 'wb') as f:
        pickle.dump(pcorr_a_m, f)

    # with open('data/heatmap-pkl/spearman_a_m.pkl', 'rb') as f:
    #     spearman_a_m = pickle.load(f)

    # with open('data/heatmap-pkl/spearman_f_m.pkl', 'rb') as f:
    #     spearman_f_m = pickle.load(f)

    # with open('data/heatmap-pkl/pcorr_f_m.pkl', 'rb') as f:
    #     pcorr_f_m = pickle.load(f)

    # with open('data/heatmap-pkl/pcorr_a_m.pkl', 'rb') as f:
    #     pcorr_a_m = pickle.load(f)

    figure, ax = plt.subplots(2, 2, figsize=(7*2, 7*2))

    ax[0, 1].imshow(np.array(spearman_a_m).reshape(5, 5), cmap='RdBu_r', origin='lower', vmin=-0.05, vmax=1)
    ax[0, 0].imshow(np.array(spearman_f_m).reshape(5, 5), cmap='RdBu_r', origin='lower', vmin=-0.05, vmax=1)
    ax[1, 0].imshow(np.array(pcorr_f_m).reshape(5, 5), cmap='RdBu_r', origin='lower', vmin=-0.05, vmax=1)
    ax[1, 1].imshow(np.array(pcorr_a_m).reshape(5, 5), cmap='RdBu_r', origin='lower', vmin=-0.05, vmax=1)

    # change the x and y ticks to the range from 0.01 to 0.09 to match with previous ticks
    ax[0, 0].set_xticks(np.arange(0, 5, 1))
    ax[0, 0].set_xticklabels(np.arange(0.01, 0.1, 0.02).round(2))
    ax[0, 0].set_yticks(np.arange(0, 5, 1))
    ax[0, 0].set_yticklabels(np.arange(0.01, 0.1, 0.02).round(2))

    ax[0, 1].set_xticks(np.arange(0, 5, 1))
    ax[0, 1].set_xticklabels(np.arange(0.01, 0.1, 0.02).round(2))
    ax[0, 1].set_yticks(np.arange(0, 5, 1))
    ax[0, 1].set_yticklabels(np.arange(0.01, 0.1, 0.02).round(2))

    ax[1, 0].set_xticks(np.arange(0, 5, 1))
    ax[1, 0].set_xticklabels(np.arange(0.01, 0.1, 0.02).round(2))
    ax[1, 0].set_yticks(np.arange(0, 5, 1))
    ax[1, 0].set_yticklabels(np.arange(0.01, 0.1, 0.02).round(2))

    ax[1, 1].set_xticks(np.arange(0, 5, 1))
    ax[1, 1].set_xticklabels(np.arange(0.01, 0.1, 0.02).round(2))
    ax[1, 1].set_yticks(np.arange(0, 5, 1))
    ax[1, 1].set_yticklabels(np.arange(0.01, 0.1, 0.02).round(2))

    # add one single colorbar on the side
    # figure.subplots_adjust(right=0.95)
    cbar_ax = figure.add_axes([0.92, 0.15, 0.02, 0.7])
    figure.colorbar(ax[0, 0].get_images()[0], cax=cbar_ax)

    # add titles


    ax[0, 1].set_title('B', loc='left', fontweight="bold")
    ax[0, 0].set_title('A', loc='left', fontweight="bold")
    ax[1, 0].set_title('C', loc='left', fontweight="bold")
    ax[1, 1].set_title('D', loc='left', fontweight="bold")

    # add labels
    ax[0, 0].set_ylabel('$\mu$')
    ax[0, 0].set_xlabel('k')

    ax[0, 1].set_ylabel('$\mu$')
    ax[0, 1].set_xlabel('k')

    ax[1, 0].set_ylabel('$\mu$')
    ax[1, 0].set_xlabel('k')

    ax[1, 1].set_ylabel('$\mu$')
    ax[1, 1].set_xlabel('k')

    # save figure as pdf 
    # make the font bigger
    plt.savefig('figures/Fig4.pdf', 
                dpi=300, 
                bbox_inches='tight')