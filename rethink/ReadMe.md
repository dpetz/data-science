# Insights

According to [pipenv vs virtualenv vs conda], use `conda` for scientific work and `pipenv` for everything else. Create a conda

```sh
pip install conda
conda env create -f environment.yml
```
Other useful `conda`commands:
```
conda create --name rethink
conda create --name rethink python
conda env list
conda activate rethink
conda env export > environment.yml
conda env remove --name rethink
```
# Data

Export data from book's R package to local csv:
``` R
library(devtools)
devtools::install_github("rmcelreath/rethinking")
library(rethinking)
data(Howell1)
d <- Howell1
write.csv(t, "howell_1.csv", row.names=FALSE))
```


# Requirments

```sh
pip3 install pandas
```

# Project Structure

Pull into local folder such as
```sh
cd git/home/data-science/rethink/
```

[Pipenv over virtualenv]:https://medium.com/@krishnaregmi/pipenv-vs-virtualenv-vs-conda-environment-3dde3f6869ed