#!/bin/bash


set -e 

sudo R --vanilla <<EOF
install.packages(c('RCurl', 
                   'stringr',
                   'ncdf4',
                   'testthat'), repos='http://cran.us.r-project.org')
q()
EOF

