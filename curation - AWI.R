# Ewan McHenry

# woodland cover gis data curation
##------ Mon Jan 31 12:46:04 2022 ------##

# libraries ----
library(tidyverse)
library(sf) # for gis
library(raster)
library(rgdal)
library(units) # for set_units()
library(nngeo) # remove holes
library(lwgeom) # snap to grid
library(U.utilities)

# load data ----
awi0 = st_read(paste0(gis.wd, "\\Data\\ancient woodland\\original\\AWI_joined_v4.02.shp"))
scot.awi0 = st_read(paste0(gis.wd, "\\Data\\ancient woodland\\original\\Ancient_Woodland_Inventory\\Ancient_Woodland_Inventory.shp"))
load(paste0(gis.wd, "\\Data\\CARSBoundary\\CARSBoundary_v03.RData")) # cars - regions

# curation ----

## first curation and colum selection ----
scot.awi <- scot.awi0 %>% 
  dplyr::select(SITE_NAME , SN_OTH, ANTIQUITY_, ANTIQUITY , MAP_ORIGIN) %>% 
  rename(Site_Name = SITE_NAME,
         Category  = SN_OTH) %>% 
  mutate(Country = "Scot") %>% 
  st_first.spatial.curation(smallest.hole = 100)

awi <- awi0 %>% 
  filter(Country != "Scot") %>% 
  dplyr::select(Site_Name, Category, Country, 
                ASNW_PAWS, # northern ireland
                Map_code # northern ireland
                ) %>% 
  st_first.spatial.curation(smallest.hole = 100) %>% 
  bind_rows(scot.awi)
awi$ha.awi_site = st_area(awi) %>% set_units("ha")

## Categorisation ----------------------------------------------------------


### AWI nucat - sensible, comparable categorisation ----
## nu_cat02 - "ASNW" ,"PAWS", "RAWS", "Unknown AW", SN LEPO, OTH LEPO, SN ROY, OTH ROY ----
# object storing unknown areas
awi$nu_cat02 = NA#awi$nu_cat
#eng and wales categories
awi$nu_cat02[awi$Category == "ASNW"] = "ASNW" 
awi$nu_cat02[awi$Category == "PAWS"] = "PAWS"
awi$nu_cat02[awi$Category == "Restored Ancient Woodland Site"] = "RAWS"
# unknown only a cat in wales
awi$nu_cat02[awi$Category == "Ancient Woodland Site of Unknown Category"] = "Unknown AW"

#Map is only in scotland
awi$nu_cat02[(awi$ANTIQUITY_ == "1a" | awi$ANTIQUITY_ == "2a") & awi$Category == "SN" ] = "ASNW" # woodland of natural  on either 1750 or 1860s maps
awi$nu_cat02[(awi$ANTIQUITY_ == "1a" | awi$ANTIQUITY_ == "2a") & awi$Category == "OTH" ] = "PAWS" # woodland of natural  on either 1750 or 1860s maps
awi$nu_cat02[awi$ANTIQUITY_ == "1b" | awi$ANTIQUITY_ == "2b"  & awi$Category == "SN" ] = "SN LEPO" # LEPO semi natural 
awi$nu_cat02[awi$ANTIQUITY_ == "1b" | awi$ANTIQUITY_ == "2b"  & awi$Category == "OTH" ] = "OTH LEPO" # LEPO other
awi$nu_cat02[awi$ANTIQUITY_ == "3" & awi$Category == "SN" ] = "SN ROY" # other
awi$nu_cat02[awi$ANTIQUITY_ == "3" & awi$Category == "OTH" ] = "OTH ROY" # other

# SORTing OUT NI STUFF TO (somewhat) MATCH SOWT
# AW - ASNW and PAWS
awi$nu_cat02[awi$Category %in% paste0("Ancient Woodland (",1:3,")") & 
               awi$ASNW_PAWS %in% c("Parkland", "Scrub", "Semi-natural broadleaved", 
                                    "Semi-natural mixed", "Semi-natural conifer")] = "ASNW" 
awi$nu_cat02[awi$Category %in% paste0("Ancient Woodland (",1:3,")") & 
               awi$ASNW_PAWS %in% c("Planted broadleaved", "Planted conifer",  "Planted mixed"
               )] = "PAWS" 
# long established SN and planted
awi$nu_cat02[awi$Category == "Long-established woodland" & 
                awi$ASNW_PAWS %in% c("Parkland", "Scrub", "Semi-natural broadleaved", 
                                      "Semi-natural mixed", "Semi-natural conifer")] = "SN LEPO" 
awi$nu_cat02[awi$Category == "Long-established woodland" & 
               awi$ASNW_PAWS %in% c("Planted broadleaved", "Planted conifer",  "Planted mixed"
                                    )] = "OTH LEPO" 
# Unknown - AW and long established
awi$nu_cat02[awi$Category == "Long-established woodland" &
               awi$Country == "NI" &
               is.na(awi$nu_cat02) ] <-  "Unknown LE"
awi$nu_cat02[awi$Category %in% paste0("Ancient Woodland (",1:3,")") & 
               awi$Country == "NI" &
               is.na(awi$nu_cat02) ] <-  "Unknown AW"
awi <- awi %>% rename(nu_cat = nu_cat02)

### AWI highlvl_paws_SN - very high level 2 category----
awi$highlvl_SN_OTH = NA
awi$highlvl_SN_OTH[awi$nu_cat %in% c("ASNW", "SN LEPO", "SN ROY")] = "SN"
awi$highlvl_SN_OTH[awi$nu_cat %in% c("PAWS", "OTH LEPO", "RAWS", "OTH ROY")] = "OTH"
awi$highlvl_SN_OTH[awi$nu_cat %in% c("Unknown AW", "Unknown LE") ] = "Unknown"

## grouping by type -----
#group by awi category
aw_grouped = awi %>%
  group_by(nu_cat) %>% 
  st_make_valid()  %>% 
  st_first.spatial.curation(., tolerance = 10, tiny.buff = 0.0001, smallest.hole = 100)%>%
  summarize(geometry = st_union(geometry))
aw_grouped$ha.awi_group = st_area(aw_grouped) %>% set_units("ha")

# intersect with CARs ----
awi_grouped_cars <- aw_grouped %>% 
  st_intersection( cars )
awi_grouped_cars$ha.car_awi_group = st_area(awi_grouped_cars) %>% set_units("ha")


#########

save(awi,file = "ancient woodland\\curated\\awi.RData" )
save(aw_grouped,file = "ancient woodland\\curated\\aw_grouped.RData" )
save(awi_grouped_cars,file = "ancient woodland\\curated\\awi_grouped_cars.RData" )
load("ancient woodland\\curated\\awi_grouped_cars.RData" ) # awi_grouped_cars

st_write(awi_grouped_cars, "ancient woodland\\curated\\awi_type_cars01.shp")

# # # # # # # # # # # # # #


## retaining individ-site info another option ----------------------------------------------------------
# fairly hesitant about moving this code here from the CARs factfile, but htink here is wher it belongs. replicate similar funcitonalityt o above
# bit more maybe --- needs to eb tidied
# it usese this pseudo ijtneraction hack, think it was to save cpu time... or there was corruption int eh result..
### [ ] investigate if thats needed
load("ancient woodland\\curated\\awi.RData") 

### AWI-CAR pseudo-intersection-hack and save ----
# becasue tehre ewas some issue wih polygons on boarders not being dealt with right? # later comment
# 1 Give individual id for each polygon - QGIS
# 2 join one to many - QGIS
# 3 isolate awi polys on cars boarder (they will be duplicated because the spatial join that made them was one-to-many) 
awi.carboarder = awi[duplicated(awi$awi_id),]# awi[awi$awi_id %in% awi$awi_id[duplicated(awi$awi_id)],]
awi.carNOTboarder = awi[!(awi$awi_id %in% awi$awi_id[duplicated(awi$awi_id)]),]
# 4 slice them up by their car
awi.carboarder.inter = st_intersection(awi.carboarder, cars )
awi.carboarder.inter$Name = awi.carboarder.inter$Name.1
awi.carboarder.inter = awi.carboarder.inter[,!names(awi.carboarder.inter) %in% c("Name.1", "area.ha")]
# 
awi.CAR.inter = rbind(awi.carNOTboarder, awi.carboarder.inter)
awi.CAR.inter$area.ha = st_area(awi.CAR.inter) %>% 
  set_units(value = "ha") %>% 
  as.numeric()

## AWI -car-category areas

# save
save(awi.CAR.inter, file = paste0(gis.wd, "Data\\AWI\\joined countries\\AWI-CAR-Inter01.RData"))



####################-------------------------------------------------------
# BUFFER WITHIN 50M -------------------------------------------------------
####################-------------------------------------------------------


# how much within 50m buffer??
# buffer by 50m and union ----
awi.roughbuff = aw_grouped %>% st_buffer(50) %>% st_union() 
sum(st_area(awi.roughbuff)) - (st_area(aw_grouped %>% st_simplify(dTolerance = 20)) %>% sum())
#st_write(awi.roughbuff, "ancient woodland\\curated\\awi_50mbuff01.shp")
save(awi.roughbuff,file = "ancient woodland\\curated\\awi_50mbuff01.RData" )

## intersect buffer with cars and remove core ----
load("ancient woodland\\curated\\awi_fixed01.RData")
load("ancient woodland\\curated\\awi_50mbuff01.RData")

awi.roughbuff_cars = st_intersection(awi.roughbuff, cars %>% st_transform(27700))
awi.roughbuff_cars2 = st_sym_difference(awi.roughbuff_cars, aw_grouped)# take out aw core
st_write(awi.roughbuff_cars2, "ancient woodland\\curated\\awi_50mbuff_cars_justbuff04.shp")

save(awi.roughbuff_cars, awi.roughbuff_cars2,file = "ancient woodland\\curated\\awi.roughbuff_cars2.RData" )

## cut LCM by buffer ----

