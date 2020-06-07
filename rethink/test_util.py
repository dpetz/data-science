from util import download_as_dataframe

def test_download_Howell1():
    d = download_as_dataframe('Howell1')
    assert list(d.columns) == ["height","weight","age","male"]
    assert len(d) == 544