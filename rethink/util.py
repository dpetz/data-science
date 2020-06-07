from pandas import read_csv

def download_as_dataframe(dataset_name):
    """Fetches data from https://github.com/rmcelreath/rethinking/blob/master/data/.
    Datasets used so far: Howell1
    """
    github_url_raw = 'https://raw.githubusercontent.com/rmcelreath/rethinking/master/data/'
    return read_csv(github_url_raw + dataset_name + '.csv', sep=';')