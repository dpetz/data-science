#!/usr/bin/env bash
curl -O http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/regression/E2006.train.bz2
bunzip2 E2006.train.bz2

curl -L -O http://files.grouplens.org/papers/ml-100k.zip
unzip ml-100k.zip

curl -L -O http://fimi.ua.ac.be/data/retail.dat.gz
