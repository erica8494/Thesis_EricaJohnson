# loading in packages
pacman::p_load(tidyverse, tidycensus, tigris, sp, sf, tmap, spatstat, sparr, maptools, raster, dplyr, 
               readxl, DCluster, spdep, SpatialEpi, CARBayes, data.table, coda, ggplot2, xlsx, plm,
               reticulate, gplots, ComplexHeatmap, reshape2, av, gifski, paletteer, cartography, GGally, splm,
               purrr, broom)


# data to get together
  # time dependent - deaths per 1000 people in county, vaccines per 1000 people in county,TRAVEL
  # time independent - overall deaths per 1000 people in county, overall vaccines per 1000 people in county
                      # % of black in county pop, % of 65+ in county pop, income per house, household size,
                      # % of county votes for trump over all votes, urban code, 
  # All data for modeling - has all of the above not including overall deaths/vaccines

  # Importing the fixed death data
    temp = read_xlsx("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/old/Correct_Inc_Deaths_GA.xlsx")  
    names(temp)
    
    deaths = temp%>% pivot_longer(!countyFIPS, names_to = "Week", values_to = "Deaths")
    total_deaths = deaths %>% group_by(countyFIPS)  %>% summarise (
      all_deaths = sum(Deaths)
    )

  # merging this total deaths to the time independent data
    temp = race_pop_data %>% inner_join(total_deaths, by="countyFIPS")
    temp$total_deaths_per_1000 = (temp$all_deaths/temp$pop)*1000
    temp = temp[, -which(names(temp) %in% c("GEOID","NAME","B01003_001", "B02009_001", "stateFIPS"))]
    time_indepentent_data$countyFIPS = as.integer(time_indepentent_data$countyFIPS)
    
    temp = time_indepentent_data %>% inner_join(temp, by="countyFIPS")
    temp$perc_trump = (temp$trump_votes/temp$total_votes)*100
    
    names(temp)
    
    complete_ind = temp[, -which(names(temp) %in% c("X","trump_votes","biden_votes", "jorgensen_votes", "level_of_urban","County.Name"))]
    complete_ind$perc_voted = (complete_ind$total_votes/complete_ind$pop)*100
    
    race_pop_data$countyFIPS = as.integer(race_pop_data$countyFIPS)
    # Merging
    time_depentent_data = grouped_data %>% inner_join(complete_ind, by=c("countyFIPS" = "countyFIPS") )
    complete_ind = time_depentent_data %>% inner_join(race_pop_data, by="countyFIPS")
    complete_ind = complete_ind[!duplicated(complete_ind$countyFIPS),]
    
    
    # Saving
    write.csv(temp, "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/complete_ind.csv")
    temp = temp[!duplicated(complete_ind$countyFIPS),]
    
  # Time dependent travel by cat  = grouped data
    # pull in vac data and get sorted
    temp = read.csv("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/old/counties.timeseries_VAC.csv")
      names(temp)    
      head(temp)    
      temp = temp %>% dplyr::filter(state == "GA")
      temp$Week <- 1+ as.numeric(as.Date(temp$date) - as.Date("2020-03-23")) %/% 7
      temp = temp[, which(names(temp) %in% c("date","state","county","fips","actuals.deaths","actuals.vaccinesDistributed",
                                             "actuals.vaccinationsInitiated","actuals.vaccinationsCompleted", "actuals.newDeaths",
                                             "actuals.vaccinesAdministered", "actuals.vaccinationsAdditionalDose","Week")) ]
      Vac = temp[0<temp$Week & temp$Week<107,]
      
      Vac = Vac %>% mutate(
        countyFIPS= substr(fips, 3,5 ),
        stateFIPS = substr(fips,1,2)
      )
      
      Vac = Vac %>% group_by(countyFIPS, Week) %>% summarise(
        total_deaaths = sum(actuals.deaths),
        total_vaccinesDistributed = sum(actuals.vaccinesDistributed),
        total_vaccinationsInitiated = sum(actuals.vaccinationsInitiated),
        total_vaccinationsCompleted = sum(actuals.vaccinationsCompleted),
        total_newDeaths = sum(actuals.newDeaths),
        total_vaccinesAdministered = sum(actuals.vaccinesAdministered),
        total_vaccinationsAdditionalDose = sum(actuals.vaccinationsAdditionalDose)
      )
      
      # Saving cleaned vac data
      write.csv(Vac, "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/complete_VAC.csv")
      
      # Adding Deaths and Vac to travel by cat  = grouped data
      temp = Vac  %>%
        dplyr::select("countyFIPS", "Week", "total_vaccinesDistributed", "total_vaccinationsInitiated", "total_vaccinationsCompleted",
                      "total_vaccinesAdministered", "total_vaccinationsAdditionalDose")
      temp$countyFIPS = as.integer(temp$countyFIPS)
      
      temp = grouped_data %>% inner_join(temp, by=c("countyFIPS" = "countyFIPS",  "week" = "Week") )
      deaths$countyFIPS = as.integer(deaths$countyFIPS)
      temp = temp %>% inner_join(deaths, by=c("countyFIPS" = "countyFIPS",  "week" = "Week") )  
      
      # Saving cleaned dependent data
      write.csv(temp, "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/complete_dep.csv")
      
    # Merging all data together into one dataset
      temp$countyFIPS = as.integer(temp$countyFIPS)
      complete_data = temp %>% inner_join(complete_ind, by = "countyFIPS")
      complete_data = complete_data %>% mutate(
        new_deaths_per_1000 = (Deaths/pop)*1000,
        new_vac_per_1000 = (total_vaccinationsInitiated/pop)*1000,
        total_vac_completed_per_1000 = (total_vaccinationsCompleted/pop)*1000
      )
      
      # Saving cleaned data
      write.csv(complete_data, "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/complete_data.csv")
      
      # Adding in spatical data
      ga_space = st_read('/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/old/ga_mvc.gpkg') %>% mutate(
        countyFIPS= substr(GEOID, 3,5 )) %>% dplyr::select("countyFIPS")
      
      ga_space$countyFIPS = as.integer(ga_space$countyFIPS)
      complete_data_geo = complete_data %>% inner_join(ga_space, by="countyFIPS")
      
      # saving the geo with data
      st_write(complete_data_geo, "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/complete_data_geo.gpkg")
      
     
      
# GRAPHING 
    # heat maping
      temp = county_fips %>% dplyr::select("countyFIPS", "name_with_county")
      temp$countyFIPS = as.integer(temp$countyFIPS)
      graphing_indep_data = temp %>% inner_join(complete_ind, by = "countyFIPS")    
    rownames(graphing_indep_data) = graphing_indep_data$name_with_county      
      names(graphing_indep_data)
      graphing_indep_data = graphing_indep_data %>% dplyr::select(-"name_with_county", -"pop", -"all_deaths")
      
      write.csv(graphing_indep_data, "/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/graphing_indep_data.csv") 
      
      df <- scale(graphing_indep_data)      
      heatmap(df, scale = "none")
      
      heatmap.2(df, scale = "none", col = bluered(100), 
                trace = "none", density.info = "none")
      
      Heatmap(df, 
              name = "Color Key", #title of legend
              column_title = "Variables", row_title = "Samples",
              row_names_gp = gpar(fontsize = 2) # Text size for row names
      )
      
      Heatmap(df, 
              name = "mtcars", #title of legend
              column_title = "Variables", row_title = "Samples",
              row_names_gp = gpar(fontsize = 7) # Text size for row names
      )
      densityHeatmap(df) 
      
      
      # looking at correlation
      # code from: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
      cormat <- round(cor(graphing_indep_data),2)
      melted_cormat <- melt(cormat)
      ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile()
      
      ggcorr(graphing_indep_data,
             nbreaks = NULL,
             label = TRUE,
             label_size = 3,
             color = "grey50")
      
      
      
      ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
        geom_tile(color = "white")+
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Pearson\nCorrelation") +
        theme_minimal()+ # minimal theme
        theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 12, hjust = 1))+
        coord_fixed()
      # Print the heatmap
      ggheatmap + 
        geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.justification = c(1, 0),
          #legend.position = c(0.6, 0.7),
          legend.direction = "horizontal")+
        guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                     title.position = "top", title.hjust = 0.5))      
      
      # looking at changes over time
      complete_data_geo = st_read("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/complete_data_geo.gpkg")
      All_data_meal = complete_data_geo[complete_data_geo$Catagory == "meal", ]
        st_geometry(All_data_meal) <- All_data_meal$geom
      
      
      All_data_edu = complete_data_geo[complete_data_geo$Catagory == "education", ]
        st_geometry(All_data_edu) <- All_data_edu$geom
      
      
      All_data_trans = complete_data_geo[complete_data_geo$Catagory == "transportation", ]
        st_geometry(All_data_trans) <- All_data_trans$geom
      
      All_data_grocery = complete_data_geo[complete_data_geo$Catagory == "grocery", ]     
        st_geometry(All_data_grocery) <- All_data_grocery$geom
      
      # looking just at week 1 for meal catagory
      All_data_meal_w1 = All_data_meal[All_data_meal$week == 1,]
      
      base_map <- tm_shape(All_data_meal_w1) +
        tm_borders(alpha = 0.2) 
      base_map
      
      names(All_data_meal_w1)
      
      map_with_data <- base_map +
        tm_shape(All_data_meal_w1) +
        tm_bubbles(size = "mean_normalized_visits_by_state_scaling", col = "new_deaths_per_1000",
                   border.col = "black", border.alpha = .5,
                   style="equal",
                   palette="-RdYlBu", contrast=1, 
                   title.size="Mean Visits by State Scaling",
                   title.col = "COVID Deaths per 1,000",
                   sizes.legend=seq(0, 2000, by=200)) +
        tm_borders() +
        tm_layout(legend.outside = TRUE)
      # Animating for all weeks
      # need to determine size breaks
      median(All_data_meal$mean_normalized_visits_by_state_scaling)
      min(All_data_meal$mean_normalized_visits_by_state_scaling)
      max(All_data_meal$mean_normalized_visits_by_state_scaling)
      quantile(All_data_meal$mean_normalized_visits_by_state_scaling)
      size_needed = c(seq(0, 2000, by=200), 2500, 3000, 3500, 4000, 4500, 5000, 5250)
      
      # need to determine color breaks
      median(All_data_meal$new_deaths_per_1000)
      min(All_data_meal$new_deaths_per_1000)
      
      max(All_data_edu$new_deaths_per_1000)
      max(All_data_grocery$new_deaths_per_1000)
      max(All_data_trans$new_deaths_per_1000)
      max(All_data_meal$new_deaths)
      
      quantile(All_data_meal$new_deaths)
      color_needed = c(seq(0,33,2), 35)
      
      # making the map
      map_with_data <- base_map +
        tm_shape(All_data_meal) +
        tm_bubbles(size = "mean_normalized_visits_by_state_scaling", col = "new_deaths_per_1000",
                   border.col = "black", border.alpha = .5,
                   style="fixed", breaks = color_needed,
                   palette=paletteer_dynamic("cartography::turquoise.pal", n=20),
                   contrast=1, 
                   title.size="Mean Visits by State Scaling",
                   title.col = "COVID Deaths per 1,000",
                   sizes.legend=size_needed) +
        tm_facets(along = "week", free.coords = FALSE) +
        tm_layout(legend.outside = TRUE)
      setwd("/Users/Erica/Desktop/APE_Thesis/Thesis")
      tmap_animation(map_with_data, "meal_cat.gif" , delay=100, outer.margins = 0)
      
      
      # Animating for all weeks for edu
      
      # making the map
      map_with_data <- base_map +
        tm_shape(All_data_edu) +
        tm_bubbles(size = "mean_normalized_visits_by_state_scaling", col = "new_deaths_per_1000",
                   border.col = "black", border.alpha = .5,
                   style="fixed", breaks = color_needed,
                   palette=paletteer_dynamic("cartography::turquoise.pal", 20),
                   contrast=1, 
                   title.size="Mean Visits by State Scaling",
                   title.col = "COVID Deaths per 1,000",
                   sizes.legend=size_needed) +
        tm_facets(along = "week", free.coords = FALSE) +
        tm_layout(legend.outside = TRUE)
      tmap_animation(map_with_data, "edu_cat.gif" , delay=100, outer.margins = 0)            

      # Animating for all weeks for transportation
      
      # making the map
      map_with_data <- base_map +
        tm_shape(All_data_trans) +
        tm_bubbles(size = "mean_normalized_visits_by_state_scaling", col = "new_deaths_per_1000",
                   border.col = "black", border.alpha = .5,
                   style="fixed", breaks = color_needed,
                   palette=paletteer_dynamic("cartography::turquoise.pal", 20),
                   contrast=1, 
                   title.size="Mean Visits by State Scaling",
                   title.col = "COVID Deaths per 1,000",
                   sizes.legend=size_needed) +
        tm_facets(along = "week", free.coords = FALSE) +
        tm_layout(legend.outside = TRUE)
      tmap_animation(map_with_data, "trans_cat.gif" , delay=100, outer.margins = 0)   
      
      
      # Animating for all weeks for grocery
      # making the map
      map_with_data <- base_map +
        tm_shape(All_data_grocery) +
        tm_bubbles(size = "mean_normalized_visits_by_state_scaling", col = "new_deaths_per_1000",
                   border.col = "black", border.alpha = .5,
                   style="fixed", breaks = color_needed,
                   palette=paletteer_dynamic("cartography::turquoise.pal", 20),
                   contrast=1, 
                   title.size="Mean Visits by State Scaling",
                   title.col = "COVID Deaths per 1,000",
                   sizes.legend=size_needed) +
        tm_facets(along = "week", free.coords = FALSE) +
        tm_layout(legend.outside = TRUE)
      
      tmap_animation(map_with_data, "grocery_cat.gif" , delay=100, outer.margins = 0) 
      
########---------------Regression---------------##############
  # Starting by looking at week 1 and a basic linear regression

  # re-organizing data
      # loading in the movememnt long data
      long_movement = read_xlsx("/Users/Erica/Desktop/APE_Thesis/Thesis/movement_long.xlsx")
      new_complete = complete_data_geo %>% dplyr::select(-"mean_normalized_visits_by_state_scaling",-"mean_normalized_visits_by_total_visits",
                                                     -"mean_normalized_visits_by_total_visitors", -"mean_normalized_visits_by_region_naics_visits",
                                                     -"mean_normalized_visits_by_region_naics_visitors", -"Catagory")
     
      lined_data = new_complete %>% inner_join(long_movement, by=c("countyFIPS" = "countyFIPS",  "week" = "week") )
      
      lined_data = unique(lined_data)
      
      names(complete_data_geo)
      write_csv(lined_data,"/Users/Erica/Desktop/APE_Thesis/Thesis/complete_lined_data.csv" )
 
  # seperating out week 1
      Data_Week1 = lined_data[lined_data$week == 1, ]
      st_geometry(Data_Week1) <- Data_Week1$geom
  
  # looking at the linear regression
      raw_linmod = lm(total_deaths_per_1000 ~ education_mean_normalized_visits_by_state_scaling + grocery_mean_normalized_visits_by_state_scaling 
                      + meal_mean_normalized_visits_by_state_scaling + transportation_mean_normalized_visits_by_state_scaling, 
                      data = Data_Week1)
      
      raw_linmod = lm(total_deaths_per_1000 ~ education_mean_normalized_visits_by_state_scaling + grocery_mean_normalized_visits_by_state_scaling 
                      + meal_mean_normalized_visits_by_state_scaling + transportation_mean_normalized_visits_by_state_scaling, 
                      data = Data_Week1)
      
      summary(raw_linmod)
      
  # looking at each week
      fit_model <- function(df) lm(total_deaths_per_1000 ~ education_mean_normalized_visits_by_state_scaling + grocery_mean_normalized_visits_by_state_scaling 
                                   + meal_mean_normalized_visits_by_state_scaling + transportation_mean_normalized_visits_by_state_scaling, 
                                   data =df)
      get_slope <- function(mod) tidy(mod)$estimate[2]
      get_p_value <- function(mod) tidy(mod)$p.value[2]
      
      temp = lined_data %>%  nest_by(countyFIPS, week) %>% 
       mutate(model = list(lm(total_deaths_per_1000 ~ education_mean_normalized_visits_by_state_scaling + grocery_mean_normalized_visits_by_state_scaling 
                              + meal_mean_normalized_visits_by_state_scaling + transportation_mean_normalized_visits_by_state_scaling, 
                              data, na.action = na.exclude)))
      %>% 
       summarise(tidy(model)) %>% 
       ungroup()
      
      
      temp = not_geo_lined_data %>%  nest_by(countyFIPS) %>% mutate(model = map(data, fit_model),
             slope = map_dbl(model, get_slope),
             p_value = map_dbl(model, get_p_value))
      not_geo_lined_data = st_drop_geometry(lined_data)
      
      # look at the different models with only ONE predictor
        # need to add back in the map function
      class(not_geo_lined_data)
      
      # run n regressions
      n = length(lined_data)
      my_lms <- lapply(1:n, function(x) lm(lined_data$total_deaths_per_1000[x] ~ lined_data$education_mean_normalized_visits_by_state_scaling[x]))
      # extract information
      sapply(my_lms, coef)
      summaries <- lapply(my_lms, summary)
      # coefficents with p values:
      lapply(summaries, function(x) x$coefficients[, c(1,4)])
      # or r-squared values
      sapply(summaries, function(x) c(r_sq = x$r.squared, 
                                      adj_r_sq = x$adj.r.squared))
      
      
      NA_data = lined_data[rowSums(is.na(lined_data)) > 0, ]
      zeroed_data = lined_data[is.na(lined_data)] = 0

      
      
      
    
      