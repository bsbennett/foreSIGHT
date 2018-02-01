INSTALLATION:
This package has a number of package dependencies. They should be installed on your computer before loading the foreSIGHT package:
install.packages(c("GA","doParallel","ggplot2","cowplot","directlabels","zoo"))


The foreSIGHT package can then be installed from a local zip file. If using RStudio, look for ‘Install Packages’ option on the ‘Tools’ menu and select ‘Package Archive File’. If using the standard R graphical interface, look for ‘Install packages from local files’ from the ‘Packages’ menu. Alternatively, the command below can be used. Make sure to give the full path to the file:

install.packages("C:\\pathtofile\\foreSIGHT_0.9.tar.gz")

You can then load the package with the following command:

library(foreSIGHT)

The package will generate a large amount of output files and diagnostic plots. It is recommended to have a dedicated working directory. This directory should be created in Windows Explorer, and then be pointed to from inside R with the following command:

setwd("C:\\path\\to\\working\\directory\\foreSIGHT_test")