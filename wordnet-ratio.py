import nltk
from nltk.corpus import wordnet
import re

if __name__ == '__main__':
    nltk.download('wordnet')
    # get all words that do not have extra symbols in them
    all_words = [w for w in wordnet.words() if
     not bool(re.search(r'\d', w)) 
     and '-' not in w and '_' not in w]
    # collect the synsets for each word
    all_sunsets = [wordnet.synsets(w) for w in all_words]
    # flatten the list of synsets
    all_synsets = [item for sublist in all_sunsets for item in sublist]

    print(f'Number of words: {len(all_words)},'
        f'number of synsets: {len(all_synsets)},'
        f'ratio: {round(len(all_words) / len(all_synsets), 3)}')
