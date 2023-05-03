from collections import defaultdict
from uuid import uuid4
import argparse
import numpy as np
from tqdm.auto import trange
import ray

ray.init()

@ray.remote
class NeutralModel():
    def __init__(self, N=10, S=30, mu=0.3, k=0.2, p=100000, t=10) -> None:
        # lookup tables for the types
        # self.frequency_lookup = defaultdict(int)
        self.meaning_lookup = defaultdict(int)
        self.age_lookup = defaultdict(int)

        self.N = N # number of types
        self.S = S # number of meanings
        self.mu = mu # rate of invention/borrowing
        self.k = k # rate of reuse
        self.p = int(p) # size of sample of tokens
        self.t = t # number of time steps

        self.initialize()

    def initialize(self):
        # initialize the meanings and frequencies
        for _ in range(self.N):
            rt = uuid4()
            self.meaning_lookup[rt] = 1
            self.age_lookup[rt] = 0

        # initailize the pool
        self.pool = np.random.choice(list(self.meaning_lookup.keys()), size=self.p, replace=True)
        
        # assign meanings 
        while sum(self.meaning_lookup.values()) < self.S:
            self.meaning_lookup[np.random.choice(self.pool)] += 1

    def step(self, t):
        # clean frequency lookup
        frequency_lookup = defaultdict(int)

        # randomly sample from pool using numpy
        self.pool = np.random.choice(self.pool, size=self.p, replace=True)

        # NB: invention applies to the pool (i.e. to the list of size p)
        invent_index = np.where(np.random.binomial(1, self.mu, size=self.p) == 1)[0]

        # adding new forms
        for i in invent_index:
            rt = uuid4()
            self.meaning_lookup[rt] = 1
            self.age_lookup[rt] = 1
            self.pool[i] = rt
        
        # update frequency_lookup using numpy
        unique_types, counts = np.unique(self.pool, return_counts=True) 
        for rt, count in zip(unique_types, counts):
            frequency_lookup[rt] = count
        
        # NB: reuse applies to the list of types
        reuse_index = np.where(np.random.binomial(1, self.k, size=len(unique_types)) == 1)[0]  
        for i in reuse_index:
            self.meaning_lookup[unique_types[i]] += 1
        
        # remove types that are not in the pool from meaning_lookup
        for rt in list(self.meaning_lookup.keys()):
            if rt in unique_types:
                self.age_lookup[rt] += 1
                self.history.append({
                        'step': t,
                        'id': rt,
                        'age': self.age_lookup[rt],
                        'count': frequency_lookup[rt],
                        'meanings': self.meaning_lookup[rt],
                        'extinct': 0
                    })
            else:
                self.history.append({
                    'step': t,
                    'id': rt,
                    'age': self.age_lookup[rt],
                    'count': frequency_lookup[rt],
                    'meanings': self.meaning_lookup[rt],
                    'extinct': 1
                })
                del self.meaning_lookup[rt]
                del self.age_lookup[rt]
        
    def run(self):
        print(f'Initializing the model with N={self.N}, S={self.S}, mu={self.mu}, k={self.k}, p={self.p}, t={self.t} ...')
        self.history = []
        name = f'data/model-data/model-N{self.N}_S{self.S}_p{self.p}.csv'

        for t in trange(self.t, 
                bar_format='{l_bar}{bar:30}{r_bar}{bar:-10b}',
                desc='Running the model', 
                position=0, 
                leave=True):
            self.step(t)

        with open(name, 'w') as f:
            f.write('step,id,age,count,meanings,extinct \n')
            for row in self.history:
                f.write(f"{row['step']},{row['id']},{row['age']},{row['count']},{row['meanings']},{row['extinct']}\n")

if __name__ == '__main__':
    # parse arguments for the model from the command line 
    parser = argparse.ArgumentParser()
    parser.add_argument('--N', type=int, default=10, help='number of types')
    parser.add_argument('--S', type=int, default=30, help='number of meanings')
    parser.add_argument('--mu', type=float, default=0.3, help='rate of invention/borrowing')
    parser.add_argument('--k', type=float, default=0.04, help='rate of reuse')
    parser.add_argument('--p', type=float, default=100, help='size of sample of tokens')
    parser.add_argument('--t', type=int, default=10, help='number of time steps')
    args = parser.parse_args()
    # run the model with ray
    model = NeutralModel.remote(N=args.N, S=args.S, mu=args.mu, k=args.k, p=args.p, t=args.t)
    ray.get(model.run.remote())
    ray.shutdown()
