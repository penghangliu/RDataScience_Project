library(tigris)
library(acs)
library(stringr) # to pad fips codes

# note that you can use county names in the tigris package but 
# not in the acs.fetch function from the acs package so I'm using
# fips numbers here.

# grab the spatial data (tigris)
#counties <- c(5, 47, 61, 81, 85)
tracts <- tracts(state = 'NY', cb=TRUE)

api.key.install(key="8a700f7c765b4c0445729df339185e229e632210")
# create a geographic set to grab tabular data (acs)
geo<-geo.make(state=c("NY"),
              county=c(5, 47, 61, 81, 85), tract="*")

Gini <- acs.fetch(endyear = 2016, span = 1, geography = geo, table.number = "B19001", col.names = "pretty")
