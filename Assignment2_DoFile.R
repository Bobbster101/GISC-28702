library(sf)
library(tmap)
library(RColorBrewer)
library(tidyverse)
library(tidycensus)

#Load Data using TidyCensus
CA_county_MHI_raw.sp <- get_acs(state = "CA", geography = "county", year = 2021, variables = "B19013_001", geometry = TRUE)
CA_tract_MHI_raw.sp <- get_acs(state = "CA", geography = "tract", year = 2021, variables = "B19013_001", geometry = TRUE)

#Remove NA Values
CA_tract_MHI.sp <- CA_tract_MHI_raw.sp[!is.na(CA_tract_MHI_raw.sp$estimate), ]
CA_county_MHI.sp <- CA_county_MHI_raw.sp[!is.na(CA_county_MHI_raw.sp$estimate), ]

#Export Shp Files
st_write(CA_county_MHI.sp,"CA_county_MHI.shp")
st_write(CA_tract_MHI.sp,"CA_tract_MHI.shp")




