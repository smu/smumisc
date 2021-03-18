#!/bin/bash


set -e 

sudo R --vanilla <<EOF
install.packages(c('RCurl', 
                   'stringr',
                   'testthat'), repos='http://cran.us.r-project.org')
q()
EOF

