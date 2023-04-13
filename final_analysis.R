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
      
      write_csv(not_geo_lined_data,"/Users/Erica/Desktop/APE_Thesis/Thesis/not_geo_lined_data.csv" )
      
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
      
      # MODELS to run just a linear regression
      outcome_data = lined_data$new_deaths_per_1000
      variable_data = lined_data %>% dplyr::select(countyFIPS,week,percap_personal_income_2020,household_size,
                                                   pop_65_plus_ratio,urban_code_2013,perc_b,perc_trump,new_vac_per_1000,
                                                   education_mean_normalized_visits_by_state_scaling,
                                                   grocery_mean_normalized_visits_by_state_scaling,
                                                   meal_mean_normalized_visits_by_state_scaling,
                                                   transportation_mean_normalized_visits_by_state_scaling)
      variable_data = st_drop_geometry(variable_data)
      full_data_lm=variable_data
      full_data_lm$new_deaths_per_1000 = outcome_data
      
       #Model 1 Simple - all variablesFit the multiple linear regression model
        model <- lm(new_deaths_per_1000~., data=full_data_lm)
      
         # Print the summary of the model results
           summary(model)      
      
      #Model 2 Simple + interaction - all variablesFit the multiple linear regression model
       model2 <- lm(new_deaths_per_1000~education_mean_normalized_visits_by_state_scaling*countyFIPS + 
                     grocery_mean_normalized_visits_by_state_scaling*countyFIPS +
                     meal_mean_normalized_visits_by_state_scaling*countyFIPS+
                     transportation_mean_normalized_visits_by_state_scaling*countyFIPS+countyFIPS+
                     week+percap_personal_income_2020+household_size+pop_65_plus_ratio+
                     urban_code_2013+perc_b+perc_trump+new_vac_per_1000+ education_mean_normalized_visits_by_state_scaling+
                   grocery_mean_normalized_visits_by_state_scaling+meal_mean_normalized_visits_by_state_scaling+
                   transportation_mean_normalized_visits_by_state_scaling, data=full_data_lm)
       
        # Print the summary of the model results
           summary(model2)      
  
    # Machine Leaning part
           # splitting the data into train and test
              set.seed(5)
              training = sample.split(full_data_lm$new_deaths_per_1000, SplitRatio = 0.8)  
              train = subset(full_data_lm, training == TRUE)
              test = subset(full_data_lm, training == FALSE)
          # Train and build the model 1
              train_model = lm(new_deaths_per_1000~., data=train)
              # Print the summary of the model results
              summary(train_model) 
              # plotting the residuals
              res = as.data.frame(residuals(train_model))
              head(res)
              res$mod = unlist(res)
              ggplot(res, aes(res$mod)) + geom_histogram(bins = 100)
            #look at model
              plot(train_model)
          # Looking at the prediction
              death_predict = predict(train_model,test)
              results = cbind(death_predict, test$new_deaths_per_1000)
              colnames(results) = c("predicted", "actual")
              results = as.data.frame(results)    
              head(results)
          # looking at the mean squared error  
            mse = mean( (results$actual - results$predicted)^2 )
            print(mse)
            # root mse
            print(mse^0.5)
          # SSE sum of squared error
            SSE = sum((results$predicted - results$actual)^2)
          # SST sum of squared total
            SST = sum ( (mean(full_data_lm$new_deaths_per_1000) - results$actual)^2 )
          # R^2 is 1 - SSE/SST
            r2 = 1 - SSE/SST
           print(r2)
    # model 2
           # Train and build the model 2
           train_model2 = lm(new_deaths_per_1000~education_mean_normalized_visits_by_state_scaling*countyFIPS + 
                              grocery_mean_normalized_visits_by_state_scaling*countyFIPS +
                              meal_mean_normalized_visits_by_state_scaling*countyFIPS+
                              transportation_mean_normalized_visits_by_state_scaling*countyFIPS+countyFIPS+
                              week+percap_personal_income_2020+household_size+pop_65_plus_ratio+
                              urban_code_2013+perc_b+perc_trump+new_vac_per_1000+ education_mean_normalized_visits_by_state_scaling+
                              grocery_mean_normalized_visits_by_state_scaling+meal_mean_normalized_visits_by_state_scaling+
                              transportation_mean_normalized_visits_by_state_scaling, data=train)
           # Print the summary of the model results
           summary(train_model2) 
           # plotting the residuals
           res = as.data.frame(residuals(train_model2))
           head(res)
           res$mod = unlist(res)
           ggplot(res, aes(res$mod)) + geom_histogram(bins = 100)
           #look at model
           plot(train_model2)
           # Looking at the prediction
           death_predict = predict(train_model2,test)
           results = cbind(death_predict, test$new_deaths_per_1000)
           colnames(results) = c("predicted", "actual")
           results = as.data.frame(results)    
           head(results)
           # looking at the mean squared error  
           mse = mean( (results$actual - results$predicted)^2 )
           print(mse)
           # root mse
           print(mse^0.5)
           # SSE sum of squared error
           SSE = sum((results$predicted - results$actual)^2)
           # SST sum of squared total
           SST = sum ( (mean(full_data_lm$new_deaths_per_1000) - results$actual)^2 )
           # R^2 is 1 - SSE/SST
           r2 = 1 - SSE/SST
           print(r2)

# I think the issue is with scaling so I am going to normalize the predictor variables
    names(train)
    # need to just rescale the predictors
    train[c(1:13)] = lapply(train[c(1:13)], function(x) c(scale(x)))
    # re-Train and build the model 1
        train_model = lm(new_deaths_per_1000~., data=train)
        # Print the summary of the model results
        summary(train_model) 
        # plotting the residuals
        res = as.data.frame(residuals(train_model))
        head(res)
        res$mod = unlist(res)
        ggplot(res, aes(res$mod)) + geom_histogram(bins = 100)
        #look at model
        plot(train_model)
        # Looking at the prediction
        test[c(1:13)] = lapply(test[c(1:13)], function(x) c(scale(x)))
        death_predict = predict(train_model,test)
        results = cbind(death_predict, test$new_deaths_per_1000)
        colnames(results) = c("predicted", "actual")
        results = as.data.frame(results)    
        head(results)
        # looking at the mean squared error  
        mse = mean( (results$actual - results$predicted)^2 )
        print(mse)
        # root mse
        print(mse^0.5)
        # SSE sum of squared error
        SSE = sum((results$predicted - results$actual)^2)
        # SST sum of squared total
        SST = sum ( (mean(full_data_lm$new_deaths_per_1000) - results$actual)^2 )
        # R^2 is 1 - SSE/SST
        r2 = 1 - SSE/SST
        print(r2)
# we are still getting a low r2 so I am going to reduce the number of cofounders
        # Train and build the model 3
        train_model3 = lm(new_deaths_per_1000~week+ education_mean_normalized_visits_by_state_scaling+
                            grocery_mean_normalized_visits_by_state_scaling+pop_65_plus_ratio+
                            perc_b+perc_trump+new_vac_per_1000
                            , data=train)
        # Print the summary of the model results
        summary(train_model3) 
        # plotting the residuals
        res = as.data.frame(residuals(train_model3))
        head(res)
        res$mod = unlist(res)
        ggplot(res, aes(res$mod)) + geom_histogram(bins = 100)
# still getting a very low r2 we might need to consider doing a non-linear regression
        # trying per_b and per_trump as quadrtic terms
        # Train and build the model 3
        train_model3 = lm(new_deaths_per_1000~countyFIPS+
                            week+percap_personal_income_2020+household_size+pop_65_plus_ratio+
                            urban_code_2013+perc_b^2+perc_trump^2+new_vac_per_1000+ education_mean_normalized_visits_by_state_scaling+
                            grocery_mean_normalized_visits_by_state_scaling+meal_mean_normalized_visits_by_state_scaling+
                            transportation_mean_normalized_visits_by_state_scaling, data=train)
        # Print the summary of the model results
        summary(train_model3) 
        # plotting the residuals
        res = as.data.frame(residuals(train_model3))
        head(res)
        res$mod = unlist(res)
        ggplot(res, aes(res$mod)) + geom_histogram(bins = 100)

# maybe we need to look at a logistic regression           

                               


###------------Changing variables into catagories to simplify--------------------------###  

    # adding in dichtomous variables for both movement and covid deaths
  
  names(full_data_lm)
  
  # # OUTCOME
  # # creating two potential new outcomes that are dichotomous
  # full_data_lm$new_deaths_increase =  c(FALSE, diff(full_data_lm$new_deaths_per_1000) >0)
  # full_data_lm$deaths_greater_mean = full_data_lm$new_deaths_per_1000 > mean(full_data_lm$new_deaths_per_1000)
  
  # PREDICTORS
  # creating new catagorical predictor variables to simplify data
  full_data_lm$new_vac_increase =  c(FALSE, diff(full_data_lm$new_vac_per_1000) >0)
  full_data_lm$new_vac_greater_mean = full_data_lm$new_vac_per_1000 > mean(full_data_lm$new_vac_per_1000)
  
    # for movement doing both increase dichotomous variable and a 3 catagory (low, medium, high movement)
  full_data_lm$edu_movement_increase =  c(FALSE, diff(full_data_lm$education_mean_normalized_visits_by_state_scaling) >0)
  full_data_lm$education_mean_normalized_visits_by_state_scaling = as.numeric(full_data_lm$education_mean_normalized_visits_by_state_scaling)
  full_data_lm$edu_movement_cat = cut(full_data_lm$education_mean_normalized_visits_by_state_scaling, 
                                      breaks = c(-Inf, quantile(full_data_lm$education_mean_normalized_visits_by_state_scaling, probs = c(1/3, 2/3)), Inf), 
                                      labels = c(0, 1, 2),
                                      include.lowest = TRUE)
  class(full_data_lm$education_mean_normalized_visits_by_state_scaling)
  full_data_lm$grocery_movement_increase =  c(FALSE, diff(full_data_lm$grocery_mean_normalized_visits_by_state_scaling) >0)
  full_data_lm$grocery_movement_cat = cut(full_data_lm$grocery_mean_normalized_visits_by_state_scaling, 
                                      breaks = c(-Inf, quantile(full_data_lm$grocery_mean_normalized_visits_by_state_scaling, probs = c(1/3, 2/3)), Inf), 
                                      labels = c(0, 1, 2))
  
  full_data_lm$meal_movement_increase =  c(FALSE, diff(full_data_lm$meal_mean_normalized_visits_by_state_scaling) >0)
  full_data_lm$meal_movement_cat = cut(full_data_lm$meal_mean_normalized_visits_by_state_scaling, 
                                          breaks = c(-Inf, quantile(full_data_lm$meal_mean_normalized_visits_by_state_scaling, probs = c(1/3, 2/3)), Inf), 
                                          labels = c(0, 1, 2))

  # subseting the data into 3 parts, one for dichtomous movement, catagorical movement, and numerical movement
  full_data_lm_di = full_data_lm[,!names(full_data_lm) %in% c("edu_movement_cat","education_mean_normalized_visits_by_state_scaling","grocery_movement_cat", "grocery_mean_normalized_visits_by_state_scaling",
                                                              "transportation_mean_normalized_visits_by_state_scaling", "new_vac_increase", "new_vac_greater_mean",
                                                              "meal_movement_cat","meal_mean_normalized_visits_by_state_scaling"  )]
  
  full_data_lm_cat = full_data_lm[,!names(full_data_lm) %in% c("edu_movement_increase","education_mean_normalized_visits_by_state_scaling","grocery_movement_increase", "grocery_mean_normalized_visits_by_state_scaling",
                                                               "transportation_mean_normalized_visits_by_state_scaling", "new_vac_increase", "new_vac_greater_mean",
                                                               "meal_movement_increase","meal_mean_normalized_visits_by_state_scaling"  )]
  full_data_lm_num = full_data_lm[,!names(full_data_lm) %in% c("edu_movement_cat","edu_movement_increase","grocery_movement_cat", "grocery_movement_increase",
                                                               "transportation_mean_normalized_visits_by_state_scaling", "new_vac_increase", "new_vac_greater_mean",
                                                               "meal_movement_cat","meal_movement_increase"  )]
  
  
  write_csv(full_data_lm,"/Users/Erica/Desktop/APE_Thesis/Thesis/not_geo_lined_data.csv" )
  
###-------------Trying to look at time series--------------------------###
  
  names(full_data_lm)
  
  # MOVEMENT 1: dichotomous movement
    full_data_lm_di$week_num = full_data_lm_di$week

    full_data_lm_di$week <- as.Date(paste0("2020-03-23"), format = "%Y-%m-%d") + weeks(full_data_lm_di$week_num - 1)

    # Split the data frame by countyFIPS
    full_data_lm_di_list <- full_data_lm_di %>% split(.$countyFIPS)

    # Loop through each data frame and convert to a time series object
    ts_list_di <- lapply(full_data_lm_di_list, function(x) {
      ts(x[, -c(1, 2)], start = min(x$week), frequency = 1)
    })
    
    # MOVEMENT 2: CATAGORICAL movement
    full_data_lm_cat$week_num = full_data_lm_cat$week
    
    full_data_lm_cat$week <- as.Date(paste0("2020-03-23"), format = "%Y-%m-%d") + weeks(full_data_lm_cat$week_num - 1)
    
    # Split the data frame by countyFIPS
    full_data_lm_cat_list <- full_data_lm_cat %>% split(.$countyFIPS)
    
    # Loop through each data frame and convert to a time series object
    ts_list_cat <- lapply(full_data_lm_cat_list, function(x) {
      ts(x[, -c(1, 2)], start = min(x$week), frequency = 1)
    })
    
    # MOVEMENT 3: numeric movement
    full_data_lm_num$week_num = full_data_lm_num$week
    
    full_data_lm_num$week <- as.Date(paste0("2020-03-23"), format = "%Y-%m-%d") + weeks(full_data_lm_num$week_num - 1)
    
    # Split the data frame by countyFIPS
    full_data_lm_num_list <- full_data_lm_num %>% split(.$countyFIPS)
    
    # Loop through each data frame and convert to a time series object
    ts_list_num <- lapply(full_data_lm_num_list, function(x) {
      ts(x[, -c(1, 2)], start = min(x$week), frequency = 1)
    })

###------------Machine Learning Splits--------------------------###
    # splitting the data into train and test
    # we are doing this by location to keep time series data together
    set.seed(5)
    
    location_data = full_data_lm %>% group_by(countyFIPS)
    
    # training = 80% of our counties and testing = 20% of our counties
    unique_counties = unique(full_data_lm$countyFIPS)
    training = sample.split(unique_counties, SplitRatio = 0.8)  
    county_split = data.frame(unique_counties, training)
    test_list_counties = subset(county_split, training == FALSE)
    train_list_counties = subset(county_split, training == TRUE)
    
    
  # MOVEMENT 1: dichotomous movement
    test_list = ts_list_di[c(test_list_counties$unique_counties)]
    test_list_di = Filter(Negate(is.null), test_list)
    train_list = ts_list_di[c(train_list_counties$unique_counties)]
    train_list_di = Filter(Negate(is.null), train_list)
    
    
  # MOVEMENT 2: CATAGORICAL movement
    test_list = ts_list_cat[c(test_list_counties$unique_counties)]
    test_list_cat = Filter(Negate(is.null), test_list)
    train_list = ts_list_cat[c(train_list_counties$unique_counties)]
    train_list_cat = Filter(Negate(is.null), train_list)
    
    
  # MOVEMENT 3: numeric movement
    test_list = ts_list_num[c(test_list_counties$unique_counties)]
    test_list_num = Filter(Negate(is.null), test_list)
    train_list = ts_list_num[c(train_list_counties$unique_counties)]
    train_list_num = Filter(Negate(is.null), train_list)

###------------Gradient Boosted Trees (GBT) Model--------------------------###
    
    ###------------MOVEMENT 1: dichotomous movement--------------------------###   
    # total weeks
    n_steps = 106
    
    ## Convert the time series data to a list of data frames
    train_df = do.call(rbind, lapply(train_list_di, as.data.frame))
    test_df = do.call(rbind, lapply(test_list_di, as.data.frame))
    
    # Create outcome variable vector
    train_list_di = lapply(train_list_di, function(x) as.data.frame(x))
    train_y = sapply(train_list_di, function(x) x$new_deaths_per_1000)
    test_list_di = lapply(test_list_di, function(x) as.data.frame(x))
    test_y = sapply(test_list_di, function(x) x$new_deaths_per_1000)
    test_y = do.call(rbind, lapply(test_y, as.data.frame))
    
    test_y = data.matrix(test_y)
    
    
    # Create input variable matrix
    train_x = data.matrix(train_df[, !(names(train_df) %in% "new_deaths_per_1000")])
    test_x = data.matrix(test_df[, !(names(train_df) %in% "new_deaths_per_1000")])
    
    # Train GBT model
    model <- xgboost(data = train_x, label = train_y, nrounds = 50)
    
    # Make predictions on test set
    pred <- predict(model, test_x)
    
    # Evaluate model performance
    cor(test_y, pred)
    
    #  evaluate the performance of the Gradient Boosted Trees (GBT) model
    
    mae = as.data.frame(abs(test_y - pred))
    mse = as.matrix(((test_y - pred)^2))
    mape = as.matrix((abs((test_y - pred)/test_y)))
    
    results$mae = as.numeric(results$mae)
    results$mse = as.numeric(mse)
    results$mape = as.numeric(mape)
    
    
    
    # Calculate the mean absolute error (MAE)
    mae = mean(results$mae)
    
    # Calculate the root mean squared error (RMSE)
    rmse = sqrt(mean(results$mse))
    
    # Calculate the mean absolute percentage error (MAPE)
    mape = mean(results$mape)*100
    
    # Calculate the coefficient of determination (R-squared)
    r_squared = cor(test_y, pred)^2
    
    # Print the evaluation metrics
    cat("MAE:", mae, "\n")
    cat("RMSE:", rmse, "\n")
    cat("MAPE:", mape, "\n")
    cat("R-squared:", r_squared, "\n")
    
    # moderate preformance of the model
    #  R-squared of 0.6173938, it means that 60.67% of the variability in the mortality data can be explained by the GBT model
    #  MAE value of 0.7867125, on average, the model's predictions for mortality deviate from the true mortality values by 0.7867125 units
    #  RMSE value of 1.271944 means that the root mean squared error of the model's predictions is 1.271944 units.
    
    
    ###------------MOVEMENT 2: CATAGORICAL movement--------------------------###   
    # total weeks
    n_steps = 106
    
    ## Convert the time series data to a list of data frames
    train_df = do.call(rbind, lapply(train_list_cat, as.data.frame))
    test_df = do.call(rbind, lapply(test_list_cat, as.data.frame))
    
    # Create outcome variable vector
    train_list_cat = lapply(train_list_cat, function(x) as.data.frame(x))
    train_y = sapply(train_list_cat, function(x) x$new_deaths_per_1000)
    test_list_cat = lapply(test_list_cat, function(x) as.data.frame(x))
    test_y = sapply(test_list_cat, function(x) x$new_deaths_per_1000)
    test_y = do.call(rbind, lapply(test_y, as.data.frame))
    
    test_y = data.matrix(test_y)
    
    
    # Create input variable matrix
    train_x = data.matrix(train_df[, !(names(train_df) %in% "new_deaths_per_1000")])
    test_x = data.matrix(test_df[, !(names(train_df) %in% "new_deaths_per_1000")])
    
    # Train GBT model
    model <- xgboost(data = train_x, label = train_y, nrounds = 50)
    
    # Make predictions on test set
    pred <- predict(model, test_x)
    
    # Evaluate model performance
    cor(test_y, pred)
    
    #  evaluate the performance of the Gradient Boosted Trees (GBT) model
    
    mae = as.data.frame(abs(test_y - pred))
    mse = as.matrix(((test_y - pred)^2))
    mape = as.matrix((abs((test_y - pred)/test_y)))
    
    results$mae = as.numeric(results$mae)
    results$mse = as.numeric(mse)
    results$mape = as.numeric(mape)
    
    
    
    # Calculate the mean absolute error (MAE)
    mae = mean(results$mae)
    
    # Calculate the root mean squared error (RMSE)
    rmse = sqrt(mean(results$mse))
    
    # Calculate the mean absolute percentage error (MAPE)
    mape = mean(results$mape)*100
    
    # Calculate the coefficient of determination (R-squared)
    r_squared = cor(test_y, pred)^2
    
    # Print the evaluation metrics
    cat("MAE:", mae, "\n")
    cat("RMSE:", rmse, "\n")
    cat("MAPE:", mape, "\n")
    cat("R-squared:", r_squared, "\n")
    
    # moderate performance of the model
    #  R-squared of 0.6028941, it means that 60.67% of the variability in the mortality data can be explained by the GBT model
    #  MAE value of 0.7867125, on average, the model's predictions for mortality deviate from the true mortality values by 0.7867 units
    #  RMSE value of 1.317372 means that the root mean squared error of the model's predictions is 1.3446 units.
    
    
    ###------------MOVEMENT 3: numeric movement--------------------------###   
    # total weeks
    n_steps = 106
    
    ## Convert the time series data to a list of data frames
    train_df = do.call(rbind, lapply(train_list_num, as.data.frame))
    test_df = do.call(rbind, lapply(test_list_num, as.data.frame))
    
    # Create outcome variable vector
    train_list_num = lapply(train_list_num, function(x) as.data.frame(x))
    train_y = sapply(train_list_num, function(x) x$new_deaths_per_1000)
    test_list_num = lapply(test_list_num, function(x) as.data.frame(x))
    test_y = sapply(test_list_num, function(x) x$new_deaths_per_1000)
    test_y = do.call(rbind, lapply(test_y, as.data.frame))
    
    test_y = data.matrix(test_y)
    
    
    # Create input variable matrix
    train_x = data.matrix(train_df[, !(names(train_df) %in% "new_deaths_per_1000")])
    test_x = data.matrix(test_df[, !(names(train_df) %in% "new_deaths_per_1000")])
    
    # Train GBT model
    model <- xgboost(data = train_x, label = train_y, nrounds = 50)
    
    # Make predictions on test set
    pred <- predict(model, test_x)
    
    # Evaluate model performance
    cor(test_y, pred)
    
    #  evaluate the performance of the Gradient Boosted Trees (GBT) model
    
        mae = as.data.frame(abs(test_y - pred))
        mse = as.matrix(((test_y - pred)^2))
        mape = as.matrix((abs((test_y - pred)/test_y)))
        
        results$mae = as.numeric(results$mae)
        results$mse = as.numeric(mse)
        results$mape = as.numeric(mape)
        
        
        
        # Calculate the mean absolute error (MAE)
        mae = mean(results$mae)
        
        # Calculate the root mean squared error (RMSE)
        rmse = sqrt(mean(results$mse))
        
        # Calculate the mean absolute percentage error (MAPE)
        mape = mean(results$mape)*100
        
        # Calculate the coefficient of determination (R-squared)
        r_squared = cor(test_y, pred)^2
        
        # Print the evaluation metrics
        cat("MAE:", mae, "\n")
        cat("RMSE:", rmse, "\n")
        cat("MAPE:", mape, "\n")
        cat("R-squared:", r_squared, "\n")
        
  # moderate preformance of the model
        #  R-squared of 0.6067, it means that 60.67% of the variability in the mortality data can be explained by the GBT model
        #  MAE value of 0.7867, on average, the model's predictions for mortality deviate from the true mortality values by 0.7867 units
        #  RMSE value of 1.3446 means that the root mean squared error of the model's predictions is 1.3446 units.
    
###------------reason to remove transportation movement--------------------------###
length(unique(All_data_edu$countyFIPS))
length(unique(All_data_grocery$countyFIPS))
length(unique(All_data_meal$countyFIPS))
length(unique(All_data_trans$countyFIPS))

# decided to not include transportation since this only had 45 conties information out of the 159, making it not generlizable

  names(full_data_lm_num)
    
           
use_condaenv("r-reticulate", required = TRUE)

###------------linear models--------------------------###
# MOVEMENT 1: dichotomous movement
names(full_data_lm_di)
    # models
    model_simple <- lm(new_deaths_per_1000~ edu_movement_increase + 
                         grocery_movement_increase +meal_movement_increase, data=full_data_lm_di)
    model_full <- lm(new_deaths_per_1000~., data=full_data_lm_di)
    
    # Print model summary
    summary(model_simple)   
    summary(model_full)

# MOVEMENT 2: CATAGORICAL movement
    # models
    model_simple <- lm(new_deaths_per_1000~ education_mean_normalized_visits_by_state_scaling + 
                         grocery_mean_normalized_visits_by_state_scaling +meal_mean_normalized_visits_by_state_scaling, data=full_data_lm_cat)
    model_full <- lm(new_deaths_per_1000~., data=full_data_lm_cat)
    
    # Print model summary
    summary(model_simple)   
    summary(model_full)



# MOVEMENT 3: numeric movement
      # models
      model_simple <- lm(new_deaths_per_1000~ education_mean_normalized_visits_by_state_scaling + 
                           grocery_mean_normalized_visits_by_state_scaling +meal_mean_normalized_visits_by_state_scaling, data=full_data_lm_num)
      model_full <- lm(new_deaths_per_1000~., data=full_data_lm_num)
      
      # Print model summary
      summary(model_simple)   
      summary(model_full)

      
###------------Creating histograms of covid deaths with lines of movement--------------------------###
names(full_data_lm)
# reading in county names by fips
county_names = read_xlsx("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/Old/ga_county_fips.xlsx") %>% mutate(countyFIPS= as.numeric(substr(fips, 3,5 )))

 # first removing un needed variable
    graphing_epi_curves = full_data_lm[, (names(full_data_lm) %in% c("countyFIPS", "week", "new_vac_per_1000", "education_mean_normalized_visits_by_state_scaling",
                                                                 "grocery_mean_normalized_visits_by_state_scaling", "meal_mean_normalized_visits_by_state_scaling",
                                                                 "transportation_mean_normalized_visits_by_state_scaling", "new_deaths_per_1000"))]

    graphing_epi_curves = graphing_epi_curves %>% inner_join(county_names, by="countyFIPS")
# # Since we want to have the movement variables on the same graph of the histogram we are going to scale these values to have a mean of 5 which should allow for this
 #    # selecting columns to be scaled
 #    cols_to_scale <- c("education_mean_normalized_visits_by_state_scaling", "grocery_mean_normalized_visits_by_state_scaling", 
 #                       "meal_mean_normalized_visits_by_state_scaling", "transportation_mean_normalized_visits_by_state_scaling")
 #    
 #    mean_values = c(2763.736/15, 2046.341/15,1557.766/15, 503.4439/15)
 #    # scale the selected columns to the arbitrary value of 15
 #    graphing_epi_curves[, cols_to_scale] = scale(graphing_epi_curves[, cols_to_scale], center = TRUE, scale = mean_values)

 # changing the week variable from numeric to a date
    graphing_epi_curves$week_num = graphing_epi_curves$week
    graphing_epi_curves$week <- as.Date(paste0("2020-03-23"), format = "%Y-%m-%d") + weeks(graphing_epi_curves$week_num - 1)
    
 # changing the dataframe into a list of data frames:
    graphing_epi_curves = graphing_epi_curves %>% split(.$name)

 # creating a function to show a histogram of deaths over time with the 4 movement lines to be used for each df in list
    create_hist <- function(df) {
      ggplot(df, aes(x = week, y = new_deaths_per_1000), labs=FALSE) +
        geom_histogram(stat="identity", fill="gray45") +
        scale_x_date(date_breaks = "1 week") +
        geom_smooth(aes(x = week, y = education_mean_normalized_visits_by_state_scaling * max(new_deaths_per_1000) / max(education_mean_normalized_visits_by_state_scaling)), method = "loess", formula = y ~ x, color = "blue") +
        geom_smooth(aes(x = week, y = grocery_mean_normalized_visits_by_state_scaling * max(new_deaths_per_1000) / max(grocery_mean_normalized_visits_by_state_scaling)), method = "loess", formula = y ~ x, color = "red")+
        geom_smooth(aes(x = week, y = transportation_mean_normalized_visits_by_state_scaling * max(new_deaths_per_1000) / max(transportation_mean_normalized_visits_by_state_scaling)), method = "loess", formula = y ~ x, color = "green")+
        geom_smooth(aes(x = week, y = new_vac_per_1000 * max(new_deaths_per_1000) / max(new_vac_per_1000)),method = "loess", formula = y ~ x, color = "purple")+
        labs(title = "name", y = "New Deaths", x = "Dates")
    }

    # use lapply to create a list of histograms for each dataframe
    hist_list = lapply(graphing_epi_curves, create_hist)
    
    
    # titles are not correct so we are going to update that
      update_hist_titles <- function(plot_list, names_vec) {
        # Update the titles of each plot
        for (i in seq_along(plot_list)) {
          plot_list[[i]] <- plot_list[[i]] +
            ggtitle(names_vec[i])
        }
        
        return(plot_list)
      }
    
    
    # making the chnage
    updated_hist_list <- update_hist_titles(hist_list, county_names$name)
    

    # Split the list of plots into batches of 25
    plot_batches = split(updated_hist_list, ceiling(seq_along(hist_list)/9))
    
    # Loop over each batch of plots and display the histograms in a 5 by 5 grid
    for (i in seq_along(plot_batches)) {
      grid.arrange(grobs = plot_batches[[i]], ncol = 3)
    }
    
    
top_counties = c("Fulton", "Gwinnett", "Cobb", "DeKalb", "Clayton", "Chatham", "Richmond", "Hall", "Muscogee")
bottom_counties = c("Calhoun", "Chattahoochee", "Baker", "Echols", "Schley", "Webster", "Clay", "Quitman", "Taliaferro")

top_hist = updated_hist_list[top_counties]
grid.arrange(
  top_hist[[1]], top_hist[[2]], top_hist[[3]],
  top_hist[[4]], top_hist[[5]], top_hist[[6]],
  top_hist[[7]], top_hist[[8]], top_hist[[9]],
  nrow = 3
)

bottom_hist = updated_hist_list[bottom_counties]
grid.arrange(
  bottom_hist[[1]], bottom_hist[[2]], bottom_hist[[3]],
  bottom_hist[[4]], bottom_hist[[5]], bottom_hist[[6]],
  bottom_hist[[7]], bottom_hist[[8]], bottom_hist[[9]],
  nrow = 3
)



    
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
      
      
      
    
      
