#### Preamble ####
# Purpose: download data from opendatatoronto
# Author: Anna Li
# Data: February 6, 2022
# Contact:annadl.li@mail.utoronto.ca

#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)

### Data download
# From  https://open.toronto.ca/dataset/death-registry-statistics/
#Datasets are grouped into packages with multiple datasets.
# First step is to get package, which contains unique key obtained from the 
#dataset's webpage. 
#get package
package <- show_package("cba07a90-984b-42d2-9131-701c8c2a9788")
package

# get all resources for this package. There is only one dataset within package.
resources <- list_package_resources("cba07a90-984b-42d2-9131-701c8c2a9788")

# As there is only one resource, load the first dataset resource as the wanted
#dataset to be used in the paper. 
death_registry <- resources %>% get_resource()

###Save data in reference to project, in order to access in paper. 
write_csv(death_registry, "inputs/data/death_registry.csv")
         