#loading in packages
pacman::p_load(tidyverse,
               tidycensus,
               tigris,
               sp,
               sf,
               tmap,
               spatstat,
               sparr,
               maptools,
               raster,
               dplyr, 
               readxl,
               DCluster,
               spdep,
               SpatialEpi,
               CARBayes,
               data.table,
               coda)
#####Setting up NAICS Catagories and getting only GA poi###########
files =  list.files(path = "/Users/Erica/Library/CloudStorage/OneDrive-SharedLibraries-EmoryUniversity/Liu, Carol - Erica thesis/poi_info/", pattern = "*.csv.gz") 

# this is too large to do at once so we will break up the files into 5 parts
# df <-  files %>%  map_df(~fread(.)) %>% dplyr::filter(region== "GA" & iso_country_code== "US")

df_1 <- files[1:10]   %>%  map_df(~fread(.)) %>% dplyr::filter(region== "GA" & iso_country_code== "US")
df_2 <- files[11:20]  %>%  map_df(~fread(.)) %>% dplyr::filter(region== "GA" & iso_country_code== "US")
df_3 <- files[21:30]  %>%  map_df(~fread(.)) %>% dplyr::filter(region== "GA" & iso_country_code== "US")
df_4 <- files[31:40]  %>%  map_df(~fread(.)) %>% dplyr::filter(region== "GA" & iso_country_code== "US")
df_5 <- files[41:49]  %>%  map_df(~fread(.)) %>% dplyr::filter(region== "GA" & iso_country_code== "US")

NAICS_list = do.call("rbind", list(df_1,df_2,df_3,df_4,df_5))
write.csv(NAICS_list, "/Users/Erica/Library/CloudStorage/OneDrive-SharedLibraries-EmoryUniversity/Liu, Carol - Erica thesis/NAICS_list.csv")    

NAICS_list= read.csv("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/NAICS_list.csv", header = T)
names(NAICS_list)

NAICS_CAT = readxl::read_xlsx("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/NAICS_list_cat.xlsx")


NAICS_list = NAICS_list[, c("placekey","location_name", "naics_code")]


NAICS_list$Catagory = ifelse(NAICS_list$naics_code %in% NAICS_CAT$Code, 1, NA) 

NAICS_list_catagory = NAICS_list[!is.na(NAICS_list$Catagory),]
NAICS_CAT_edu = NAICS_CAT[NAICS_CAT$category == "education",]
NAICS_CAT_meal = NAICS_CAT[NAICS_CAT$category == "meal",]
NAICS_CAT_transportation = NAICS_CAT[NAICS_CAT$category == "transportation",]
NAICS_CAT_grocery = NAICS_CAT[NAICS_CAT$category == "grocery",]

NAICS_list_catagory$Catagory = ifelse(NAICS_list_catagory$naics_code %in% NAICS_CAT_edu$Code, 
                                      "education", ifelse(NAICS_list_catagory$naics_code %in% NAICS_CAT_meal$Code, 
                                                          "meal", ifelse(NAICS_list_catagory$naics_code %in% NAICS_CAT_transportation$Code, 
                                                                         "transportation",ifelse(NAICS_list_catagory$naics_code %in% NAICS_CAT_grocery$Code, 
                                                                                                 "grocery",NA))))


write.csv(NAICS_list_catagory, "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/NAICS_list_catagory.csv")

######Adding NAICS categories to poi files and removing NA########

NAICS = read.csv("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/NAICS_list_catagory.csv", header = T)

NAICS=NAICS[,c(-1,-3)]

setwd("~/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/GA_Safegraph_Weeklydata/")

# Grabbing all needed poi files
poi_files = list.files(path = "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/GA_Safegraph_Weeklydata/", pattern = "*.RDS")
inital = "~/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/GA_Safegraph_Weeklydata/"

# week one data
df_1 <- poi_files[1] %>%  map_df(~readRDS(.)) %>% dplyr::filter(placekey %in% NAICS$placekey) %>%
  mutate( week = 1) %>% left_join(.,NAICS, by = "placekey" )

# looping to rbind all poi data
for (i in 1:length(poi_files)){
  df <- poi_files[i] %>%  map_df(~readRDS(.)) %>% dplyr::filter(placekey %in% NAICS$placekey) %>%
    mutate( week = i) %>% left_join(.,NAICS, by = "placekey" )
  weeks_poi_data = rbind(df,df_1)
  df_1 = weeks_poi_data
}
saveRDS(object=weeks_poi_data, file="~/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/weeks_poi_data.RDS")

names(weeks_poi_data)

weeks_poi_data[1,"iso_country_code"]

# Write up the potential different analysis 
# 
# Wrangle data 
# Figure out what the visit patterens look like of the first day of the week 
# Give a few maps
# 
# Look at some discriptives
# 
# Treat this as a time seres




