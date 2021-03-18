#!/bin/bash


set -e 


R CMD build .
R CMD check *.tar.gz
R CMD INSTALL *.tar.gz
