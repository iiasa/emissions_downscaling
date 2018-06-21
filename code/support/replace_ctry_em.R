setwd("C:/Users/brau074/Documents/emissions_downscaling/input/reference_emissions/CEDS_CMIP6_to_2015")
library(magrittr)
library(dplyr)
library(tidyr)

ceds.old <- "CEDS_by_country_by_CEDS_sector_with_luc_all_em.csv" %>%
  read.csv()

chn.SO2.new <- "chn_emissions_extended.csv" %>%
  read.csv() %>%
  filter(em == "SO2") %>%
  gather(key=year, value=E, -em, -sector) %>%
  filter(year == "X2015") %>%
  # filter(! year %in% c("X2016", "X2017") ) %>%
  mutate(em = "Sulfur")

sector.map <- "CEDS_sector_mapping.csv" %>%
  read.csv() %>%
  select(CEDS_working_sector, CEDS16)

chn.SO2.new.mapped <- chn.SO2.new %>%
  left_join(sector.map, by=c("sector"="CEDS_working_sector")) %>%
  select(-sector) %>%
  dplyr::rename(sector=CEDS16) %>%
  filter(sector != "") %>%
  group_by(em, sector, year) %>%
  summarise(E=sum(E)/1000) %>%
  ungroup() %>%
  mutate(unit = "Mt")

chn.so2.new.mapped.wide <- chn.SO2.new.mapped %>%
  spread(key=year, value=E)

# CHINA SO2 data remains
chn.so2.awb.pb <- ceds.old %>%
  filter(iso == "chn" &
         em == "Sulfur" &
         sector %in% c("Agricultural Waste Burning",
                       "Peat Burning",
                       "Grassland Burning",
                       "Forest Burning"))

ceds.new <- ceds.old %>%
  filter(! (iso == "chn" & em == "Sulfur")) %>%
  rbind(chn.so2.awb.pb) %>%
  rbind(chn.so2.new.mapped.wide)
