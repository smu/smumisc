#!/bin/bash


set -e 

#sudo R --vanilla <<EOF
#install.packages(c('RCurl', 
                   #'stringr',
                   #'ncdf4',
                   #'testthat'), repos='http://cran.us.r-project.org')
#q()
#EOF

# install cdo

#wget https://code.mpimet.mpg.de/attachments/download/24638/cdo-1.9.10.tar.gz
#tar xvzf cdo-1.9.10.tar.gz
#cd cdo-1.9.10
#./configure 
#make
#sudo make install

export _R_CHECK_FORCE_SUGGESTS_=FALSE

R CMD build .
R CMD check *.tar.gz
R CMD INSTALL *.tar.gz
