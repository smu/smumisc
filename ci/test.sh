#!/bin/bash


set -e 

R --vanilla <<EOF
install.packages(c('RCurl','ncdf','testthat'), repos='http://cran.us.r-project.org')
q()
EOF

R CMD build .
R CMD check *.tar.gz
R CMD INSTALL *.tar.gz
