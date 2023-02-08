
# loading in packages
pacman::p_load(tidyverse,tidycensus,tigris,sp,sf,tmap,spatstat,sparr,maptools,raster,dplyr, readxl,DCluster,spdep,SpatialEpi,
               CARBayes,data.table,coda,ggplot2, xlsx,reticulate,gplots,ComplexHeatmap,
               reshape2, av, gifski,  paletteer)

# loading data into R
time_indepentent_data = read.csv("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/time_indepentent_data.csv")
data = readRDS("~/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/weeks_poi_data.RDS")
time_depentent_data = read.csv("/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/time_depentent_data.csv")
All_data = st_read('/Users/Erica/Library/CloudStorage/OneDrive-EmoryUniversity/Erica thesis/all_data.gpkg') 


# Graphing the data
  # Preping time independent data for heatmap
    names(graph_time_ind_data)
    
    time_indepentent_data$per_trump = time_indepentent_data$trump_votes/time_indepentent_data$total_votes
    
    graph_time_ind_data = time_indepentent_data %>% dplyr::select(-"trump_votes", -"biden_votes", -"jorgensen_votes", -"total_votes")
    graph_time_ind_data = graph_time_ind_data %>% dplyr::select(-"countyFIPS")
    graph_time_ind_data = as.data.frame(graph_time_ind_data[-160,])
    row.names(graph_time_ind_data) = graph_time_ind_data$County.Name
    graph_time_ind_data = graph_time_ind_data %>% dplyr::select(-"level_of_urban", -"County.Name")
    
    df <- scale(graph_time_ind_data)

    # Looking at time independent heatmaps
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
      
  # looking at changes over time
      All_data_meal = All_data[All_data$Catagory == "meal", ]
        st_geometry(All_data_meal) <- All_data_meal$geom
      All_data_edu = All_data[All_data$Catagory == "education", ]
        st_geometry(All_data_edu) <- All_data_edu$geom
      All_data_trans = All_data[All_data$Catagory == "transportation", ]
        st_geometry(All_data_trans) <- All_data_trans$geom
      All_data_grocery = All_data[All_data$Catagory == "grocery", ]     
        st_geometry(All_data_grocery) <- All_data_grocery$geom
        names(All_data_meal_w1)
  # looking just at week 1 for meal catagory
      All_data_meal_w1 = All_data_meal[All_data_meal$week == 1,]
     
       base_map <- tm_shape(All_data_meal_w1) +
        tm_borders(alpha = 0.2) 
      coord_quickmap() +
        theme_void() 
      base_map
      
      map_with_data <- base_map +
        tm_shape(All_data_meal_w1) +
        tm_bubbles(size = "mean_normalized_visits_by_state_scaling", col = "Deaths",
                   border.col = "black", border.alpha = .5,
                   style="equal",
                   palette="-RdYlBu", contrast=1, 
                   title.size="Mean Visits by State Scaling",
                   title.col = "COVID Deaths",
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
            median(All_data_meal$Deaths)
            min(All_data_meal$Deaths)
            max(All_data_meal$Deaths)
            quantile(All_data_meal$Deaths)
            color_needed = c(0, 500, 1000, 1500, 2000, 2500, 4500, 6500, 8500, 10000,
                             13000, 16000, 19000, 22000, 25000,50000, 75000, 100000,
                             500000, 1000000)
      
      # removing un-need variables from graphing data
            names(All_data_meal)
            meal_graphing = All_data_meal %>% dplyr::select("week","mean_normalized_visits_by_state_scaling",
                                                            "Deaths", "geom")
            st_geometry(meal_graphing) <- meal_graphing$geom
            
       # making the map
            map_with_data <- base_map +
              tm_shape(meal_graphing) +
              tm_bubbles(size = "mean_normalized_visits_by_state_scaling", col = "Deaths",
                         border.col = "black", border.alpha = .5,
                         style="fixed", breaks = color_needed,
                         palette=paletteer_dynamic("cartography::turquoise.pal", 20),
                         contrast=1, 
                         title.size="Mean Visits by State Scaling",
                         title.col = "COVID Deaths",
                         sizes.legend=size_needed) +
              tm_facets(along = "week", free.coords = FALSE) +
              tm_layout(legend.outside = TRUE)
      
            tmap_animation(map_with_data, "meal_cat.mp4" , delay=100, outer.margins = 0)

            
            # Animating for all weeks for edu
            # need to determine size breaks
            median(All_data_edu$mean_normalized_visits_by_state_scaling)
            min(All_data_edu$mean_normalized_visits_by_state_scaling)
            max(All_data_edu$mean_normalized_visits_by_state_scaling)
            quantile(All_data_edu$mean_normalized_visits_by_state_scaling)
            size_needed = c(seq(0, 2000, by=200), 2500, 3000, 3500, 4000, 4500, 5000, 7500, 10000, 12500)
            
            # need to determine color breaks
            median(All_data_edu$Deaths)
            min(All_data_edu$Deaths)
            max(All_data_edu$Deaths)
            quantile(All_data_edu$Deaths)
            color_needed = c(0, 500, 1000, 1500, 2000, 2500, 4500, 6500, 8500, 10000,
                             13000, 16000, 19000, 22000, 25000,50000, 75000, 100000,
                             500000, 1000000)
            
            # removing un-need variables from graphing data
            names(All_data_meal)
            edu_graphing = All_data_edu %>% dplyr::select("week","mean_normalized_visits_by_state_scaling",
                                                            "Deaths", "geom")
            st_geometry(edu_graphing) <- edu_graphing$geom
            
            # making the map
            map_with_data <- base_map +
              tm_shape(edu_graphing) +
              tm_bubbles(size = "mean_normalized_visits_by_state_scaling", col = "Deaths",
                         border.col = "black", border.alpha = .5,
                         style="fixed", breaks = color_needed,
                         palette=paletteer_dynamic("cartography::turquoise.pal", 20),
                         contrast=1, 
                         title.size="Mean Visits by State Scaling",
                         title.col = "COVID Deaths",
                         sizes.legend=size_needed) +
              tm_facets(along = "week", free.coords = FALSE) +
              tm_layout(legend.outside = TRUE)
            
            tmap_animation(map_with_data, "edu_cat.gif" , delay=100, outer.margins = 0)            
             
            # Animating for all weeks for trans
            # need to determine size breaks
            median(All_data_trans$mean_normalized_visits_by_state_scaling)
            min(All_data_trans$mean_normalized_visits_by_state_scaling)
            max(All_data_trans$mean_normalized_visits_by_state_scaling)
            quantile(All_data_trans$mean_normalized_visits_by_state_scaling)
            size_needed = c(seq(0, 2600, by=200), 3000, 3500, 4000, 4500, 5000, 7500, 10000, 12500, 15000)
            
            # removing un-need variables from graphing data
            names(All_data_trans)
            trans_graphing = All_data_trans %>% dplyr::select("week","mean_normalized_visits_by_state_scaling",
                                                          "Deaths", "geom")
            st_geometry(trans_graphing) <- trans_graphing$geom
            
            # making the map
            map_with_data <- base_map +
              tm_shape(trans_graphing) +
              tm_bubbles(size = "mean_normalized_visits_by_state_scaling", col = "Deaths",
                         border.col = "black", border.alpha = .5,
                         style="fixed", breaks = color_needed,
                         palette=paletteer_dynamic("cartography::turquoise.pal", 20),
                         contrast=1, 
                         title.size="Mean Visits by State Scaling",
                         title.col = "COVID Deaths",
                         sizes.legend=size_needed) +
              tm_facets(along = "week", free.coords = FALSE) +
              tm_layout(legend.outside = TRUE)
            
            tmap_animation(map_with_data, "trans_cat.gif" , delay=100, outer.margins = 0)   
            
            
            # Animating for all weeks for grocery
            # need to determine size breaks
            median(All_data_grocery$mean_normalized_visits_by_state_scaling)
            min(All_data_grocery$mean_normalized_visits_by_state_scaling)
            max(All_data_grocery$mean_normalized_visits_by_state_scaling)
            quantile(All_data_grocery$mean_normalized_visits_by_state_scaling)
            size_needed = c(seq(0, 2500, by=30), 500, 1000, 2000, 3000, 4000, 7500, 10000, 12500, 15000, 17500)
            
            # removing un-need variables from graphing data
            names(All_data_grocery)
            grocery_graphing = All_data_grocery %>% dplyr::select("week","mean_normalized_visits_by_state_scaling",
                                                              "Deaths", "geom")
            st_geometry(grocery_graphing) <- grocery_graphing$geom
            
            # making the map
            map_with_data <- base_map +
              tm_shape(grocery_graphing) +
              tm_bubbles(size = "mean_normalized_visits_by_state_scaling", col = "Deaths",
                         border.col = "black", border.alpha = .5,
                         style="fixed", breaks = color_needed,
                         palette=paletteer_dynamic("cartography::turquoise.pal", 20),
                         contrast=1, 
                         title.size="Mean Visits by State Scaling",
                         title.col = "COVID Deaths",
                         sizes.legend=size_needed) +
              tm_facets(along = "week", free.coords = FALSE) +
              tm_layout(legend.outside = TRUE)
            
            tmap_animation(map_with_data, "grocery_cat.gif" , delay=100, outer.margins = 0) 
            
            
            
#specifying which version of python to use
reticulate::use_python(python = '/Library/Frameworks/Python.framework/Versions/3.11/bin/python3', required = T)
sys <- import("sys")
sys$version
sns <- import('seaborn')
plt <- import('matplotlib.pyplot')
pd <- import('pandas')

#building a seaborn pairplot using pairplot()
sns$pairplot(r_to_py(graph_time_ind_data),
             plot_kws=dict(marker=".", linewidth=1))
#display the plot
plt$show()

  # looking at correlation
  # code from: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
    cormat <- round(cor(graph_time_ind_data),2)
    melted_cormat <- melt(cormat)
    ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
      geom_tile()
    
    # Reorder the correlation matrix
    cormat <- reorder_cormat(cormat)
    upper_tri <- get_upper_tri(cormat)
    # Melt the correlation matrix
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    # Create a ggheatmap
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
    print(ggheatmap)
    
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

    
    
    
    
