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
               coda,
               ggplot2, xlsx)
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

######loading in the data#####
data = readRDS("~/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/weeks_poi_data.RDS")

names(data)

# checking catagories with a quick look through
meal = data[data$Catagory == "meal",]

# GETTING THE COUNTY FIPS
data = data %>% mutate(countyFIPS= substr(poi_cbg, 3,5 ),
                stateFIPS = substr(poi_cbg,1,2))
  # checking
head(data[,c("poi_cbg", "countyFIPS", "stateFIPS")])
table(data$countyFIPS)

  # saving
saveRDS(object=data, file="~/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/weeks_poi_data.RDS")

# Merging Data for all time independent
  # importing GA county name to fips
      county_fips = read_xlsx("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/ga_county_fips.xlsx")%>% mutate(
        countyFIPS= substr(fips, 3,5 ),
        stateFIPS = substr(fips,1,2),
        county_name_all_upper = toupper(name)
        ) %>% rename(
        county_name = name
      )

  # county median age
      age = read_xlsx("~/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/median_age_county_2015_to_2019.xlsx") %>%
       dplyr::filter(state == "Georgia") %>% mutate(
         countyFIPS= substr(fips, 3,5 ),
         stateFIPS = substr(fips,1,2)
          )
  # county household size
      house_size = read_xlsx("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/_Persons per household 2014-2018 - (Average).xlsx")
        
      house_size = right_join(house_size, county_fips, by = c("County" = "county_name")) %>%
        dplyr::select("County", "Persons per household", "countyFIPS")
  # Urbanicity
      urbanicity = read_xlsx("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/urbanicity.xlsx")%>%
        dplyr::filter(`State Abr.` == "GA") %>% mutate(
          countyFIPS= substr("FIPS code", 3,5 ),
          stateFIPS = substr("FIPS code",1,2)
        )
      urban_level_fun = function(x) {
        ifelse(x == 1, return ("Large central metro"),
               ifelse(x == 2, return("large fringe metro"),
                      ifelse(x == 3, return("Medium metro"),
                             ifelse(x == 4,return("Small metro"),
                                    ifelse(x == 5, return("Micropolitan"),
                                           ifelse(x == 6, return("Noncore"), return(NA)))))))
      }
      
      urbanicity$level_of_urban = apply(urbanicity[,"2013 code"], 1,urban_level_fun)
      urbanicity$countyFIPS= substr(urbanicity$`FIPS code`, 3,5 )
      urbanicity$stateFIPS= substr(urbanicity$`FIPS code`, 1,2 )
  # Income
      income = read_xls("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/GA_COUNTY_personal income per capita_2020_prep.xls") %>%
        dplyr::filter(LineCode == 3) %>% mutate(
          countyFIPS= substr(GeoFips, 3,5 ),
          stateFIPS = substr(GeoFips,1,2)
        )
  # Pop % 65 and older
      pop_65_plus = read_xlsx("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/ga_county_pop_2020.xlsx") %>%
        dplyr::select("County", "2020 Population 65 and Over, Percent") 
      pop_65_plus = right_join(pop_65_plus, county_fips, by = c("County" = "county_name_all_upper")) %>%
        dplyr::select("County", "2020 Population 65 and Over, Percent", "countyFIPS") %>% rename(
          pop_65_plus_ratio = `2020 Population 65 and Over, Percent`
        )
  # political affiliation
      politics = read_xlsx("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/Georgia 2020 RLA Report.xlsx") %>%
        right_join( county_fips, by = c("County" = "county_name_all_upper")) %>%
        dplyr::select("County", "Trump", "Biden", "Jorgensen","Total", "countyFIPS")
  
  # MERGING
      time_indepentent_data = right_join(house_size, income, by = "countyFIPS") %>% rename(
        percap_personal_income_2020 = "2020",
        household_size = `Persons per household`) %>%
        dplyr::select("percap_personal_income_2020","countyFIPS", "household_size")
      time_indepentent_data = right_join(time_indepentent_data, age, by = "countyFIPS") %>%
        dplyr::select("countyFIPS", "percap_personal_income_2020", "household_size", "median_age_2019")
      time_indepentent_data = right_join(time_indepentent_data, politics, by = "countyFIPS") %>% rename(
        trump_votes = Trump,
        biden_votes = Biden,
        jorgensen_votes = Jorgensen,
        total_votes = Total
      ) %>% dplyr::select("countyFIPS", "percap_personal_income_2020", "household_size", "median_age_2019", "trump_votes","biden_votes","jorgensen_votes","total_votes")      
      time_indepentent_data = right_join(time_indepentent_data, pop_65_plus, by = "countyFIPS") %>% 
        dplyr::select("countyFIPS", "percap_personal_income_2020", "household_size", "median_age_2019", "trump_votes","biden_votes","jorgensen_votes","total_votes", "pop_65_plus_ratio")      
       time_indepentent_data = right_join(time_indepentent_data, urbanicity, by = "countyFIPS") %>% rename(urban_code_2013 = `2013 code`) %>% 
        dplyr::select("countyFIPS", "percap_personal_income_2020", "household_size", "median_age_2019", "trump_votes","biden_votes", "jorgensen_votes","total_votes", "pop_65_plus_ratio", "urban_code_2013", "level_of_urban")
  # Saving
       install.packages("xlsx")
       library("xlsx")
      xlsx::write.xlsx(time_indepentent_data,"/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/time_indepentent_data.xlsx" )
       write.csv(time_indepentent_data, "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/time_indepentent_data.csv")
# Merging Data for all time dependent
  # COVID Deaths
       covid_deaths = read.csv("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/covid_confirmed_usafacts.csv")  %>%
         dplyr::filter(State == "GA") %>% mutate(
           countyFIPS= substr(countyFIPS, 3,5 ),
           stateFIPS = substr(countyFIPS,1,2)
         )
       covid_deaths$week = rownames(covid_deaths)
       
           write.csv(covid_deaths, "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/Deaths_GA.csv")
    # importing the COVID Deaths per week
       covid_deaths = read.csv("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/COVID_Deaths_GA.csv")  %>%
         mutate(
           countyFIPS= substr(FIPS, 3,5 )
         )
       covid_deaths_time_ind = covid_deaths[,c(2,109,110)]
    # adding total deaths to time_independent data
       time_indepentent_data = right_join(time_indepentent_data, covid_deaths_time_ind, by = "countyFIPS")
# Graphing 
    plot(time_indepentent_data$percap_personal_income_2020, time_indepentent_data$total_deaths)
    plot(time_indepentent_data$household_size, time_indepentent_data$total_deaths)
    ggplot(time_indepentent_data, aes(x = factor(median_age_2019), y = total_deaths)) + 
      geom_bar(stat = "identity")
    plot(time_indepentent_data$trump_votes/time_indepentent_data$total_votes, time_indepentent_data$total_deaths)
    plot(time_indepentent_data$percap_personal_income_2020, time_indepentent_data$household_size)
    plot(time_indepentent_data$percap_personal_income_2020, time_indepentent_data$household_size)
    plot(time_indepentent_data$pop_65_plus_ratio, time_indepentent_data$total_deaths)
    ggplot(time_indepentent_data, aes(x = factor(level_of_urban), y = total_deaths)) + 
      geom_bar(stat = "identity")
    plot(time_indepentent_data$pop_65_plus_ratio,time_indepentent_data$trump_votes/time_indepentent_data$total_votes)
    ggplot(time_indepentent_data, aes(x = factor(level_of_urban), y = percap_personal_income_2020)) + 
      geom_bar(stat = "identity")
    plot(time_indepentent_data$median_age_2019, time_indepentent_data$percap_personal_income_2020)
       
# Grouping safe_graph data by catatory
    grouped_data = data %>% group_by(countyFIPS, week, Catagory)  %>% summarise(
      mean_normalized_visits_by_state_scaling = mean(normalized_visits_by_state_scaling),
      mean_normalized_visits_by_total_visits = mean(normalized_visits_by_total_visits),
      mean_normalized_visits_by_total_visitors = mean(normalized_visits_by_total_visitors),
      mean_normalized_visits_by_region_naics_visits = mean(normalized_visits_by_region_naics_visits),
      mean_normalized_visits_by_region_naics_visitors = mean(normalized_visits_by_region_naics_visitors)
    )
    cat_trans_1= grouped_data[grouped_data$Catagory == "meal",]
    plot(cat_trans_1$mean_normalized_visits_by_state_scaling, cat_trans_1$week)
    names(cat_trans_1)
    
# breaking up covid death data into multiple sets to add to other time dependent data
    #df %>% pivot_longer(cols=week1:week2, names_to= "week", values_to="vax_val)
    # need to remove everything but county and weeks data
  temp = read.csv("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/COVID_Deaths_GA_toLong.csv")  %>%
    pivot_longer(!FIPS, names_to = "Week", values_to = "Deaths")  %>% mutate(
      countyFIPS= substr(FIPS, 3,5 ),
      Week =as.numeric(gsub("X","",temp$Week) ))
  # Merging
  time_depentent_data = grouped_data %>% inner_join(temp, by=c("countyFIPS" = "countyFIPS",  "week" = "Week") ) %>% 
    dplyr::select(-"FIPS")
  
  
  # Saving
  write.csv(time_depentent_data, "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/time_depentent_data.csv")
  
  
  # Vac importing
  vac = read.csv("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/COVID-19_Vaccinations_in_the_United_States_County.csv") %>%
    dplyr::filter(Recip_State == "GA")
  
  # mutate for what week each date corresponds to - calander week
  # group by week and county
  # then how many new vac for that time period
  

# Merging all data but vaccination into one data set
  All_data = time_depentent_data %>% inner_join(time_indepentent_data, by=("countyFIPS" = "countyFIPS")) 
  
  # Adding in spatical data
  ga_space = st_read('/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/ga_mvc.gpkg') %>% mutate(
    countyFIPS= substr(GEOID, 3,5 )) %>% dplyr::select("countyFIPS")
  
  All_data = All_data %>% left_join(ga_space, by=("countyFIPS" = "countyFIPS")) 
  st_geometry(All_data) <- All_data$geom
  
  # saving all data together
  st_write(All_data,'/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/all_data.gpkg',append=FALSE)
  
  All_data_meal = All_data[All_data$Catagory == "meal", ]
  st_geometry(All_data_meal) <- All_data_meal$geom
  All_data_edu = All_data[All_data$Catagory == "education", ]
  All_data_trans = All_data[All_data$Catagory == "transportation", ]
  All_data_grocery = All_data[All_data$Catagory == "grocery", ]     
  
  names(All_data_meal)
  
  
  # percentage change in movement to these place
# Plotting
  base_map <- tm_shape(All_data_meal[All_data_meal$week == 1,]) +
    tm_borders(alpha = 0.2) 
    coord_quickmap() +
    theme_void() 
  base_map
  
  map_with_data <- base_map +
    geom_point(data = All_data_meal[All_data_meal$week == 1,], aes(mean_normalized_visits_by_total_visits, Deaths, group=week))
  map_with_data
  
  
  ggplot(All_data_meal) + geom_point(aes(mean_normalized_visits_by_total_visits, Deaths))
  
  map_with_animation <-ggplot(st_geometry(ga_space)) +
    geom_point(data = All_data_meal, aes(mean_normalized_visits_by_total_visits, Deaths, group=week)) +
    transition_time(week) +
    ggtitle('Week: {frame_time}',
            subtitle = 'Frame {frame} of {nframes}')
  num_years <- max(All_data_meal$week) - min(All_data_meal$week) + 1
  animate(map_with_animation, nframes = num_years)
  
# use ggplots facit raping (facet_wrap(~week)) -> variable you want to split it up by
    # looking at a random sample
        # might be better to look at the epidemic and see graphs for up weeks and low weeks
        # or other "special" weeks like christmas/holiday/change in stay at home orders
    # county map -> mobility from and to the county
    
  
# PPT FOR TOMORROW
# Potential next steps:
    # get all of the time dependent data into one data set
    # determine if any of the cofounders are overally corelated
    # determine which variables 
    
    # get facebook data by mid feburary
    
# small write up on why perdiction might be better then assiciation using machine learning to get that
  
  #abstract submission for epidemics conference -> June submission

