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

# Grabbing all needed poi files
poi_files = list.files(path = "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/GA_Safegraph_Weeklydata/", pattern = "*.RDS")

poi_files = data_frame(poi_files) %>% mutate(filepath = paste0(inital, filename))


for (i in 1:length(poi_files)){
  poi_files %>% rowwise() %>%
    do(., readRDS(file=.$filepath)) %>% 
    dplyr::filter(placekey %in% NAICS_list_catagory$placekey)
}



# Trying one file to add NAICS codes and categories
inital = "~/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/GA_Safegraph_Weeklydata/"

Week1.2020_03_23 = readRDS(paste(inital,poi_files[[1]], sep = "")) %>% 
  dplyr::filter(placekey %in% NAICS_list_catagory$placekey)

Week1.2020_03_23 = left_join(Week1.2020_03_23,NAICS_list_catagory, by = "placekey" )


dat <- gsub("comb.RDS","",poi_files[[1]])
x = paste("Week1",dat, sep = ".") 

names(Week1.2020_03_23)

dat <- gsub("comb.RDS","",poi_files[[1]])
paste("Week1",dat, sep = ".") 



### In loop form for all the files
for (i in 1:length(poi_files)){
  
  dat <- gsub("comb.RDS","",filenames[[i]])
  df <- readRDS(paste("Safegraph/data/comb/",filenames[[i]], sep = ""))
  df1<- expand_cat_json(df, 'visitor_home_cbgs', by="placekey")%>% 
    arrange(placekey) %>%
    left_join(df %>% select(placekey, poi_cbg) %>%
                arrange(placekey))
  
  df2 <- df1 %>% 
    mutate(home_county = substr(index, 3, 5),   ## Extract county fips from the home CBG
           poi_county = substr(poi_cbg, 3,5)) %>%     
    
    group_by(home_county, poi_county) %>%                 
    summarise(n=sum(visitor_home_cbgs)) %>% 
    arrange(home_county) %>% 
    left_join(home_panel_county[home_panel_county$week_start==dates[[i]],] %>% 
                arrange(county_fips),
              by = c("home_county"="county_fips"), 
              keep=F) %>%
    filter(!is.na(week_start)) %>%
    mutate(pop_flow = n/sampling_frac)
  
  saveRDS(df2, paste("Safegraph/data/od_trips/", dat, ".RDS",sep = "")) 
}




# Write up the potential different analysis 
# 
# Wrangle data 
# Figure out what the visit patterens look like of the first day of the week 
# Give a few maps
# 
# Look at some discriptives
# 
# Treat this as a time seres




