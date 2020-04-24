# Script for statistical analysis and visualisation of data for the drone photogrammetry allometry project.

# Establish operating environment ----
home <- "C:/workspace/DroneAllometry/"

### troubleshooting ###
# options(repos = c(CRAN = "http://cran.rstudio.com"))  # set default CRAN mirror

#### Load required packages ####
# Download
  # install.packages("rnaturalearth")
  # install.packages(c("classInt", "data.table", "devtools", "digest", "dplyr", "e1071", "effectsize", "ff", "fs", "glue", "haven", "htmltools", "httpuv", "isoband", "jsonlite", "later", "lme4", "lubridate", "mapview", "mime", "mvtnorm", "nloptr", "plotrix", "processx", "promises", "ps", "purrr", "raster", "Rcpp", "reshape2", "rgl", "rJava", "rlang", "robust", "robustbase", "rrcov", "satellite", "scales", "sf", "sp", "stringi", "testthat", "tibble", "tidyr", "tidyselect", "units", "vctrs", "xfun"))
# install.packages("plotbiomes")
# install.packages("devtools")

library(devtools)
library(tidyverse)                                                              # For making life better.
library(readxl)                                                                 # For reading and writing .xlsx files.
library(viridis)                                                                # For friendly colour palettes.
library(scales)                                                                 # used for illustrating colours
library(sf)                                                                     # For working with Simple Feature objects
library(rnaturalearth)                                                          # Import Earth dataset global maps
library(rnaturalearthdata)                                                      # Import datasets for global map
library(patchwork)                                                              # A lovely tool for combining plots
library(grid)                                                                   # required for plot annotation
library(gridExtra)                                                              # for arranging multi-panel plots.
library(ggpubr)                                                                 # for arranging multi-panel plots.
# library(robustbase)                                                             # Used for robust regression with rlm().
devtools::install_github("valentinitnelav/plotbiomes") # because the plotbiomes package wasn't yet avaialble for R 3.6.1                                                           # Whittaker biomes for climate space plot
library(plotbiomes)                                                             # Whittaker biomes for climate space plot
library(lme4)                                                                   # For linear mixed effects models.
# library(lmerTest)                                                             # For extracting p values from linear mixed effects models (but can sometimes conflict with other packages, so use carfeully and be aware of errors!).
library(ggeffects)                                                              # For plotting mixed effects models.
library(sjPlot)                                                                 # For plotting mixed effects models.
library(xlsx)                                                                   # For writing Excel files of summary tables.
library("cvTools")                                                              # USed for cross-validation of models
library(RColorBrewer)
library(scales)                                                                 # Used for extracting hexadecimal colour codes
# library(broom)                                                                  # For summarising model outputs.
# library(nortest)

###*************************************************
#### Load data ----
###*************************************************

df <- read_excel(paste0(home, "outputs/processed_database.xlsx"),
                      na = "NA")                                                # Read in summary data.

#### Subset data
df <- df %>%
    filter(!is.na(AGB_g_m2) & !is.na(HAG_plotmean_of_cellmax_m))                # Filter for observations with both HAG and AGB values.

df_peak <- df %>%
    filter(PeakBiomass == TRUE) %>%                                             # Subset observations from peak biomass.
    filter(!is.na(AGB_g_m2) & !is.na(HAG_plotmean_of_cellmax_m))                # Filter for observations with both HAG and AGB values.

df_peak_ex_bryo <- df %>%
  filter(PeakBiomass == TRUE) %>%                                               # Subset observations from peak biomass.
  filter(plant_functional_type != "Bryophyte") %>%                              # Remove bryophyte observations.
  filter(!is.na(AGB_g_m2) & !is.na(HAG_plotmean_of_cellmax_m))                  # Filter for observations with both HAG and AGB values.

# Set minimum threshold number of observations for species-level analysis.
min_obs <- 4

# Subset species with more than theshold number of observations.
df_peak_filt <- df_peak %>%
  group_by(binomial_species) %>%
  filter(n() >= min_obs) %>%                                                    # Filter by threshold number of observations
  filter(plant_functional_type != "Bryophyte") %>%                              # Remove bryophyte observations.
  filter(!binomial_species %in% c("Mixed grasses", "Unknown spp."))             # Remove unwanted 'species'.

df_sev <- df %>%
  filter(SevilletaIAV == TRUE) %>%                                            # Subset observations from Sevilleta sites.
  filter(!is.na(AGB_g_m2) & !is.na(HAG_plotmean_of_cellmax_m))                # Filter for observations with both HAG and AGB values.



###*************************************************
#### Create/verify output directories ----
###*************************************************

loc_outputs <- paste0(home, "outputs")
dir.create(loc_outputs, showWarnings = FALSE)

loc_df <- paste0(home, "outputs/0 - Dataset-level")
dir.create(loc_df, showWarnings = FALSE)

loc_survey <- paste0(home, "outputs/1 - Survey-level plots")
dir.create(loc_survey, showWarnings = FALSE)

loc_species <- paste0(home, "outputs/2 - Species-level plots")
dir.create(loc_species, showWarnings = FALSE)

loc_pft <- paste0(home, "outputs/3 - PFT-level plots")
dir.create(loc_pft, showWarnings = FALSE)

loc_sevilleta <- paste0(home, "outputs/9 - Sevilleta")
dir.create(loc_sevilleta, showWarnings = FALSE)

###*************************************************
#### Create plot themes, symbology and scaling  ----
###*************************************************
theme_coding <- function(){
    theme_bw()+
        theme(axis.text = element_text(size = 6),
              axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
              axis.title = element_text(size = 8),
              panel.grid = element_blank(),
              plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), units = , "cm"),
              plot.title = element_text(size = 10, vjust = 1, hjust = 0.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 5, face = "italic"),
              legend.key.size = unit(0.9,"line"),
              legend.background = element_rect(color = "black", fill = "transparent", size = 4, linetype="blank"),
              legend.position = c(0.9, 0.9))
}

# Set the order of factor levels.
    df_peak <- df_peak %>%
      filter(plant_functional_type == "Succulent" |
               plant_functional_type == "Tree" |
               plant_functional_type == "Shrub" |
               plant_functional_type == "Forb" |
               plant_functional_type == "Graminoid" |
               plant_functional_type == "Fern" |
               plant_functional_type == "Bryophyte") %>%
      mutate(plant_functional_type == factor(plant_functional_type, levels = c("Succulent",
                                                                               "Tree",
                                                                               "Shrub",
                                                                               "Forb",
                                                                               "Graminoid",
                                                                               "Fern",
                                                                               "Bryophyte")))

# Specify symbology for PFT
    # List PFTs
      # pft_list <- sort(unique(df_peak$plant_functional_type))
      pft_list <- c("Fern",
                    "Forb",
                    "Graminoid",
                    "Shrub",
                    "Succulent",
                    "Tree"
                    )

    # Assign colour to each PFT
      # NB. The hexadecimal colour picker on Google is useful for manual selection (https://www.google.com/search?safe=off&client=firefox-b-d&sxsrf=ACYBGNRRbmRH08sX2olIHXglvm_Axq3oEQ%3A1580896026902&ei=Go86XoPjNqKDhbIPxoiToA0&q=hexadecimal+colour+picker&oq=hexadecimal+colour+picker&gs_l=psy-ab.3..0l2j0i22i30l2j0i22i10i30l2.2736.3423..3659...0.0..0.56.323.6......0....1..gws-wiz.......35i39.asZZ3ZLIbwk&ved=0ahUKEwiD2PDQkLrnAhWiQUEAHUbEBNQQ4dUDCAo&uact=5)
      pft_colours <- c("#047cd1",
                       "#c33ce6",
                       "#aaeb07",
                       "#2c8232",
                       "#ced104",
                       "#755504")
      scales::show_col(pft_colours)

    # Set shape parameters
      pft_shapes <- c(15, 16, 17, 18, 25, 22)

    # Combine into dataframe
      pft_symbology <- data.frame(pft_list, pft_colours, pft_shapes)

    # Specify PFT colours
      col_fern <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Fern"])
      col_forb <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Forb"])
      col_graminoid <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Graminoid"])
      col_shrub <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Shrub"])
      col_succulent <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Succulent"])
      col_tree <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Tree"])



### Compute scaling parameters from minimum and maximum values ###
    mat_max <- signif((1.1*max(df_peak$MAT, na.rm = TRUE)),2)
    mat_min <- signif((1.1*min(df_peak$MAT, na.rm = TRUE)),2)
    map_max <- signif((1.1*max(df_peak$MAP, na.rm = TRUE)),2)
    min_median_hag <- 0  # min(df_peak$HAG_plotmedian_of_cellmax_m, na.rm = TRUE)
    max_median_hag <- max(df_peak$HAG_plotmedian_of_cellmax_m, na.rm = TRUE)
    min_mean_hag <- 0  # min(df_peak$HAG_plotmean_of_cellmax_m, na.rm = TRUE)
    max_agb <- 1.35*max(df_peak$AGB_g_m2, na.rm = TRUE)
    max_mean_hag <- 1.1*max(df_peak$HAG_plotmean_of_cellmax_m, na.rm = TRUE)



### *************************************************
### Figure 1    ----
### *************************************************
### Figure 1a (Global Map) ----
# Create global map                                                             # Inspired by https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
      coastlines <- ne_coastline(scale = "medium", returnclass = "sf")
      countries <- ne_countries(scale = "medium", returnclass = "sf")

    # Create sample sites as SF object
      # Create dataframe of site locations
        sites <- data.frame(df_peak$SiteLongitude, df_peak$SiteLatitude)

      # Keep only distinct site locations and rename columns
        sites <- sites %>%
          distinct() %>%
          rename(
            longitude = df_peak.SiteLongitude,
            latitude = df_peak.SiteLatitude)

      # Convert to SF object
        sites_sf <- st_as_sf(sites, coords = c("longitude", "latitude"),
                       crs = 4326, agr = "constant")

    # Create global map
        (global_map <- ggplot(data = countries) +
          geom_sf(colour = "grey90", fill = "grey90") +  # add continents
          geom_sf(data = coastlines, colour = "black") +  # add coastlines outline
          geom_sf(data = sites_sf, size = 2, shape = 21, fill = "red") +  # Add sampling sites to plot
          coord_sf(crs = st_crs(54030)) +  # Reprojects all layers already drawn to the specified (Robinson) projection.
          theme_bw()
          )

        # NB. Ideally this figure would use the small-scale coastline dataset to
        # eliminate 'clutter' of islands. However, there appears to be a bug in
        # coord_sf that prevents the small-scale coastline dataset from being
        # reprojected to the Robson projections
        # (https://github.com/tidyverse/ggplot2/issues/3949)
        # coastlines <- ne_coastline(scale = "small", returnclass = "sf")

        # coastlines <- ne_coastline(scale = "small", returnclass = "sf")


        # (global_map <- ggplot(data = countries) +
        #     geom_sf(colour = "grey90", fill = "grey90") +  # add continents
        #     geom_sf(data = coastlines, colour = "black") +  # add coastlines outline
        #     # ggtitle("Sampling sites") +  # Add title to plot
        #     geom_sf(data = sites_sf, size = 2, shape = 21, fill = "red") +  # Add sampling sites to plot
        #     coord_sf(crs = st_crs('ESRI:54030')) +  # Reprojects all layers already drawn to the specified (Robinson) projection.
        #     theme_bw()
        # )
        #
        # ggplot(data = st_wrap_dateline(coastlines)) + geom_sf(colour = "black") +
        #   coord_sf(crs = st_crs('ESRI:54030')) +
        #   theme_bw()
        #
        # ggplot(data = st_wrap_dateline(coastlines)) + geom_sf(colour = "black") +
        #   coord_sf(crs = st_crs(54030)) +
        #   theme_bw()
#
        # configure()
        #
        # library(sf)
        # library(rgdal)
        # library(rgeos)
        # rgeos
        # install.packages("rgeos")
        #
        #
        # st_crs(54030)
        # library(PROJ)
        #
        # library(ggpubr)
        # install.packages("ggpubr")
        #
        # st_crs(54030)
        #
        # sessionInfo()
        #
        # install.packages("installr")
        #
        # library(installr)
        #
        # updateR()

        # NB. Ideally this global map would use the Winkel-Tripel projction to
        # further minimise distortion, however this projection isn't yet
        # supported in the SF package so for simplicty we've used the Robinson
        # projection


    # Export global map
        ggsave(paste0(loc_outputs, "/Global map from R.png"), width = 8, height = 6, dpi=500)





### Figure 1b (Climate Space) ----
  # Whittaker polygons after R.H. Whittaker. 1975. Communities and Ecosystems. 2d ed. Macmillan New York
    df$MAP_cm <- df$MAP/10                                      # Create new column with MAP in cm rather than mm.

  # Create modified colour palette (with forests in grey)
  # Create table of biome data
      biomes_tbl <- data.frame(biome_id = 1:9,
                               biome = c("Tropical rain forest",
                                         "Temperate rain forest",
                                         "Boreal forest",
                                         "Temperate seasonal forest",
                                         "Tropical seasonal forest/savanna",
                                         "Woodland/shrubland",
                                         "Temperate grassland/desert",
                                         "Subtropical desert",
                                         "Tundra"))
  # Manually specify biome colour palette
      biome_colours <- c("grey72",
                        "grey80",
                        "grey88",
                        "#A5C790",
                        "#e6da00",
                        "#DCBB50",
                        "#FCD57A",
                        "#D16E3F",
                        "#C1E1DD")
      # scales::show_col(biome_colours)  # Show colours

  # Add biome names to colour palette
      names(biome_colours) <- biomes_tbl$biome

  # Create plot with custom colour palette
      (Whittaker_plot <- ggplot(data = df,
                               aes(x = MAT,
                                   y = MAP_cm)) +
          theme_coding() +
          coord_cartesian(expand=FALSE) +
          geom_polygon(data = Whittaker_biomes,
                       aes(x    = temp_c,
                           y    = precp_cm,
                           fill = biome),
                       colour = "gray98", size   = 0.5) +                             # adjust polygon borders
          scale_fill_manual(name   = "Whittaker biomes",
                            breaks = names(biome_colours),
                            labels = names(biome_colours),
                            values = biome_colours) +
          geom_point() +
          theme(legend.position = c(0.3, 0.736),
                legend.text = element_text(size = 7.8, face = 'bold')) +
          labs(x = expression("Mean Annual Temperature " ( degree~C)),
               y = expression("Mean Annual Precipitation (cm yr"^"-1"*")"))
          )

      # Export climate space plot
        outfile <- file.path(home, "outputs",paste("Sampled climate space (peak biomass).png",sep="")) # Specify filepath
        png(filename=outfile, width = 11, height = 11, units = 'cm', res = 500)         # Save plot
        plot(Whittaker_plot)
        dev.off()

      rm(biomes_tbl)                                                                  # Tidying up


### Combine Figure 1 parts a and b using the patchwork package
      (figure1 <- global_map / Whittaker_plot +
      plot_annotation(tag_levels = 'A') &
        theme(plot.tag.position = c(0.0, 0.97)  # Control horizontal and vertical position of panel labels
              # plot.tag = element_text(size = 10, hjust = 0, vjust = 0)
              ))

      # Export figure
      outfile <- file.path(home, "outputs/Figure 1/",paste(" Figure 1 (parts A and B).png",sep="")) # Specify filepath
      png(filename=outfile, width = 12, height = 16, units = 'cm', res = 500)         # Save plot
      plot(figure1)
      dev.off()



### ******************************
### 2. df-level analysis ####
### ******************************

### Visualisation of solar offsets
  plot_solar_offset_mins <- hist(df$minutes_offset_from_solar_noon)
  outfile <- file.path(loc_df, paste("Histogram of time offset between survey and solar noon.png",sep="")) # Specify filepath
  png(filename=outfile, width = 10, height = 10, units = 'cm', res = 500)       # Save plot
  plot(plot_solar_offset_mins)
  dev.off()

  plot_solar_offset_degrees <- hist(df$solar_elevation_offset)
  outfile <- file.path(loc_df, paste("Histogram of sun elevation offset between survey and solar noon.png",sep="")) # Specify filepath
  png(filename=outfile, width = 10, height = 10, units = 'cm', res = 500)       # Save plot
  plot(plot_solar_offset_degrees)
  dev.off()


# ### Analyse mean versus median canopy height
# # Fit model of HAGs with a fixed effect for functional group
# # model_HAGs <- lmer(HAG_plotmedian_of_cellmax_m ~ HAG_plotmean_of_cellmax_m *
# #                        plant_functional_type + (1|survey_code),
# #                    data = df_peak)
# # summary(model_HAGs)
# #
# # # # It is possible to constrain the overall intercept to zero.  The code for that model would I think be:
# # model_HAGs <- lmer(HAG_plotmedian_of_cellmax_m ~ HAG_plotmean_of_cellmax_m *
# #                        plant_functional_type + 0 + (HAG_plotmean_of_cellmax_m|survey_code), data = df_peak)
# #
# # summary(model_HAGs)
# #
# # preds_HAGs <- ggpredict(model_HAGs, terms = c("HAG_plotmean_of_cellmax_m"))
# #
# #
# # # Create plot of mean versus median canopy height, including model predictions and errors and a fixed effect of functional type.
# # mean_median_HAGs <- ggplot() +
# #     geom_ribbon(data = preds_HAGs, aes(ymin = conf.low, ymax = conf.high, x = x), alpha = 0.4) +
# #     geom_point(data = df_peak,
# #                aes(x = HAG_plotmean_of_cellmax_m,
# #                    y = HAG_plotmedian_of_cellmax_m, colour = plant_functional_type), shape=20, size = 2, na.rm = TRUE) +
# #     geom_line(data = preds_HAGs, aes(x = x, y = predicted), size = 0.5) +
# #     scale_colour_viridis_d() +
# #     geom_abline(slope = 1, na.rm = TRUE, colour = "black", linetype = "dashed") +
# #     theme_coding() +
# #     coord_cartesian(ylim = c(0, (max_mean_hag*1.3)),
# #                     xlim = c(0, (max_mean_hag*1.3)),
# #                     expand=FALSE) +
# #     labs(x = expression("Mean canopy height (m)"),
# #          y = expression("Median canopy height (m)"),
# #          title = "Mean versus median canopy height",
# #          colour = "Plant Functional Type") +
# #     theme(legend.position = c(0.82, 0.25),
# #           legend.text = element_text(size = 7, face = "plain"),
# #           legend.title = element_text(size = 7.2, face = 'bold'))
# #
# # outfile <- file.path(loc_df, "Mean Vs Median HAG.png")                     # Filename for output.
# # png(filename=outfile, width = 10, height = 10, units = 'cm', res = 400)         # Export plot.
# # plot(mean_median_HAGs)
# # dev.off()
#
#
#
# #### Analyse HAG versus AGB for all observations in peak dataset
# # Model of AGB with a fixed effect for functional group
#
# #### TO DO (Intercept of mixed effects models):
# # Is it possible (and sensible?) to specify an intercept of zero in the above fixed-effect model?
# # I think a fixed intercept would be justified on physical grounds, but I haven't been able to
# # find any guidance about implementing this so maybe it's simply not appropriate/possible
# # with fixed effects models?
#
# model_HAGs_fg <- lmer(AGB_g_m2 ~ HAG_plotmean_of_cellmax_m *
#                           plant_functional_type + (1|survey_code),
#                       data = df_peak_ex_bryo)
#
# summary(model_HAGs_fg)
#
# preds_HAGs_fg <- ggpredict(model_HAGs_fg, terms = c("HAG_plotmean_of_cellmax_m", "plant_functional_type"))
#
#
# ### Scatterplot of all observations in peak df, by PFT ###
# # Create scatter plot of all observations of HAG versus AGB.
# # Includes model predictions and errors with a fixed effect of functional type.
# # TO DO (Andy):
# # I'd like to configure the legend to display the line, symbol and shading for each PFT.
# # currently, there is an issue with the different names used for grouping variables in the
# #  different datasets (daa=preds_HAGs_fg=group, whereas data=df_peak=plant_functional_type...)
# # I'm not sure of the best way to reconcile this... maybe adding to the ggplot() function?
# # (HAG_Vs_AGB_by_PFT <- ggplot() +
# #     geom_ribbon(data = preds_HAGs_fg,
# #                 aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
# #                 alpha = 0.2, show.legend = T) +
# #     geom_point(data = df_peak,
# #                aes(x = HAG_plotmean_of_cellmax_m,
# #                    y = AGB_g_m2,
# #                    colour = plant_functional_type,
# #                    shape = plant_functional_type,
# #                    alpha = 0.7), show.legend = T) +
# #     geom_line(data = preds_HAGs_fg, aes(x = x, y = predicted, colour = group),
# #               size = 0.5, show.legend = T) +
# #     scale_colour_viridis_d() +
# #     scale_fill_viridis_d() +
# #     scale_shape_manual(values = pft_shapes) +
# #     labs(x = expression("Mean canopy height (m)"),
# #          y = expression("Dry biomass (g m"^"-2"*")"),
# #          title = expression("Canopy height predicts aboveground biomass"),
# #          colour = "Plant functional type",
# #          shape = "Plant functional type") +
# #     theme_coding() +
# #     theme(plot.title = element_text(face="italic"),
# #           legend.title = element_text(size=8),
# #           legend.position = c(0.15, 0.8)) +
# #     coord_cartesian(ylim = c(0, max_agb), xlim = c(0, max_mean_hag), expand=FALSE))
# #
# #
# # # Export scatter plot
# #   outfile <- file.path(loc_df, "HAG Vs AGB by PFT - new.png",sep="")
# #   png(filename=outfile, width = 14, height = 14, units = 'cm', res = 600)
# #   plot(HAG_Vs_AGB_by_PFT)
# #   dev.off()
#
#
# # Original scatter plot - wih constrained intercept
# (HAG_Vs_AGB_by_PFT1 <- ggplot(data = df_peak_ex_bryo,
#                              aes(x = HAG_plotmean_of_cellmax_m,
#                                  y = AGB_g_m2,
#                                  colour = plant_functional_type,
#                                  shape = plant_functional_type)) +
#     geom_point(alpha = 0.7, na.rm = TRUE) +
#     scale_colour_viridis_d() +
#     scale_shape_manual(values = pft_shapes) +
#     labs(x = expression("Mean canopy height (m)"),
#          y = expression("Dry biomass (g m"^"-2"*")"),
#          title = expression("Canopy height predicts aboveground biomass"),
#          colour = "Plant functional type",
#          shape = "Plant functional type") +
#     theme_coding() +
#     theme(plot.title = element_text(face="italic"),
#           legend.title = element_text(size=8),
#           legend.position = c(0.85, 0.79)) +
#     coord_cartesian(ylim = c(0, max_agb), xlim = c(0, max_mean_hag), expand=FALSE) +
#     geom_smooth(method="lm", formula= y ~ x-1,
#                 aes(group=plant_functional_type,
#                     colour=plant_functional_type),
#                 se=FALSE, size=0.5, na.rm = TRUE)
#   )
#
# # Export scatter plot
# outfile <- file.path(loc_df, "HAG Vs AGB by PFT - original.png",sep="")
# png(filename=outfile, width = 14, height = 14, units = 'cm', res = 600)
# plot(HAG_Vs_AGB_by_PFT1)
# dev.off()
#
#
#
# # Mixed effects models - IN DEVELOPMENT
#
# # 1st model: biomass as a function of height, with survey_code as a fixed effect.
# model <- lmer(AGB_g_m2 ~ HAG_plotmean_of_cellmax_m + (1|survey_code), data = df_peak)
# summary(model)
#
#
# # 2nd model: include taxononic group.
# model <- lmer(AGB_g_m2 ~ HAG_plotmean_of_cellmax_m * plant_functional_type + 0 + (1|survey_code), data = df_peak)
# summary(model)






### ******************************
### 3. Survey-level analysis ####
# Plot height above ground biomass against aboveground biomass by survey_code.
### ******************************
df %>%
    group_by(survey_code) %>%
    do({plot <- ggplot(., aes(x = HAG_plotmean_of_cellmax_m,
                              y = AGB_g_m2,
                              colour = binomial_species,
                              fill = binomial_species,
                              label = PlotID)) +
        geom_point(shape = 1, na.rm = TRUE) +
        geom_text(aes(label = PlotID), hjust = -0.5,  vjust = -0.5, size=1.3) +
        scale_colour_viridis_d() +
        scale_fill_viridis_d() +
        labs(x = expression("Mean canopy height (m)"),
             y = expression("Dry biomass (g m"^"-2"*")"),
             title = paste(.$survey_code)) +
        theme_coding() +
        theme(legend.position = c(0.35, 0.8)) +
        coord_cartesian(ylim = c(0, 1.2 * max(.$AGB_g_m2)),
                        xlim = c(0, 1.2 * max(.$HAG_plotmean_of_cellmax_m)),
                        expand = FALSE) +
        geom_smooth(method=lm,
                    formula= y ~ x-1,
                    aes(group=binomial_species),
                    se=FALSE, size=0.5, na.rm = TRUE, alpha=0.2)
    ggsave(paste0(loc_survey, "/", unique(.$survey_code),
                  ".png", sep = ''), width = 10, height = 10,
           units = 'cm', plot = plot)
    })


### ******************************
### 4. Species-level analysis of mean height versus biomass ####
### ******************************

# 4.1 Table of model parameters for species-level ####
{
  # Create empty dataframe to be populated with species-level parameters
  species_summary <- data.frame(plant_functional_type = character(),
                                order = character(),
                                family = character(),
                                species = character(),
                                observations = integer(),
                                n_of_surveys = integer(),
                                lm_slope = double(),
                                lm_slope_error = double(),
                                lm_r2 = double(),
                                lm_p = double(),
                                lm_loocv = double(),
                                stringsAsFactors=FALSE
  )

  # List unique species
  species_l <- sort(unique(df_peak_filt$binomial_species))

  # Derive model parameters for species
  for(j in species_l){
    species_dat <- filter(df_peak_filt, binomial_species == j)

    if(nrow(species_dat) < min_obs){next}     # skip group if there are not enough observations.

    x <- species_dat$HAG_plotmean_of_cellmax_m
    y <- species_dat$AGB_g_m2

    ### fit models
    model_lm <- lm(y ~ x + 0, na.action=na.exclude)
    sum_lm <- summary(model_lm)


    ### Cross-Validation of model fits ###
    # Estimate the prediction error of a linear model via (repeated) K-fold cross-validation.
    # Split df into training and validation 'sets', fit model to
    # training set, and calculate residual error for the validation 'set'.
    # with Leave-One-Out Cross-Validation (LOOCV), training set is n-1
    # and validation set is n=1, and the exericse is repeated n times.
    # Setting K = n equals leave-one-out cross-validation.
    lm_loocv <- cvTools::repCV(model_lm, cost = rtmspe, K = nrow(species_dat))

    # Extract model parameters as new list
    species_info <- c(plant_functional_type = as.character(unique(species_dat$plant_functional_type)),
                      order = as.character(unique(species_dat$PlotOrder)),
                      family = as.character(unique(species_dat$PlotFamily)),
                      species = as.character(unique(species_dat$binomial_species)),
                      observations = min(length(species_dat$AGB_g_m2),length(species_dat$HAG_plotmean_of_cellmax_m)),
                      n_of_surveys = length(unique(species_dat$survey_code)),
                      lm_slope = trunc(sum_lm$coefficients[1]),
                      lm_slope_error = trunc(sum_lm$coefficients[2]),
                      lm_r2 = round(sum_lm$r.squared, 2),
                      lm_p = round(sum_lm$coefficients[4],4),
                      lm_loocv = trunc(as.numeric(lm_loocv$cv))
    )

    # Convert list into dataframe.
    species_info <- data.frame(lapply(species_info, type.convert), stringsAsFactors=FALSE)

    # Append new row to summary table.
    species_summary <- rbind(species_summary, species_info)

  }

  rm(species_info)  # Tidy up

  # Refine table of species-level model parameters
  # Order species_summary_table alphabetically by group, order, family and binomial species. NB. tolower deals with case issues.
    species_summary2 <- species_summary[order(tolower(species_summary$plant_functional_type),
                                              tolower(species_summary$order),
                                              tolower(species_summary$family),
                                              tolower(species_summary$species)
                                              ),]

  # Compute relative LOOCV errors
    species_summary2$lm_loocv_pc <- species_summary2$lm_loocv/species_summary2$lm_slope * 100  # Convert absolute errors into relative percentage error.
    species_summary2$lm_loocv_pc <- round(species_summary2$lm_loocv_pc, 1)

  # Drop absolute LOOCV error
    species_summary3 <- subset(species_summary2, select = -c(lm_loocv))

  # Change values lower than threhsold to "0.0001", and remove scientific notation in the characterised vector
    species_summary3$lm_p[species_summary2$lm_p < 0.0001] <- "<0.0001"
    species_summary3$lm_p[species_summary2$lm_p == "1e-04"] <- "0.0001"
    species_summary3$lm_p[species_summary2$lm_p == "2e-04"] <- "0.0002"
    species_summary3$lm_p[species_summary2$lm_p == "3e-04"] <- "0.0003"

  # Export model summary as Excel file.
    model_summary_path <- paste(loc_outputs,"/Species summaries.xlsx", sep = "")
    write.xlsx(species_summary3, model_summary_path, row.names=FALSE)

}





# ******************************************************
# 4.2 Create species-level plots of HAG versus ABG ####
  # NB. 'add_count(binomial_species)' and 'filter(n>X)' are used to exclude observations with fewer than X observations from the rest of the pipe.
  # Calculate optimum XY ratio for consistent display.
    mhag <- tapply(df_peak_filt$HAG_plotmean_of_cellmax_m, df_peak_filt$binomial_species, max)  # Create array of maximum height values for each species.
    magb <- tapply(df_peak_filt$AGB_g_m2,                  df_peak_filt$binomial_species, max)  # Create array of maximum biomass values for each species.
    yx_ratio <- max(magb/mhag)  # Calculate maximum XY ratio in the df.

  # Create plots of each species, coloured by survey code
    df_peak_filt %>%
        group_by(binomial_species) %>%
        add_count(binomial_species) %>%
        # filter(n > min_obs) %>%
        do({
            # Create scaling parameters for constant ratios
            x <- 1.1*(max(.$HAG_plotmean_of_cellmax_m))
            y <- 1.1*(max(.$AGB_g_m2))
            xlim <- c(0, x)
            ylim <- c(0, max(x)*yx_ratio)

            # create plots for each species, grouped by survey_code.
            plot <- ggplot(., aes(x = HAG_plotmean_of_cellmax_m,
                                  y = AGB_g_m2,
                                  colour = survey_code)) +
                geom_point(shape = 1, na.rm = TRUE) +
                scale_colour_viridis_d() +
                labs(x = expression("Mean canopy height (m)"),
                     y = expression("Dry biomass (g m"^"-2"*")"),
                     title = paste(.$binomial_species)) +
                theme_coding() +
                theme(plot.title = element_text(face="italic")) +
                theme(legend.position = c(0.35, 0.8)) +
                coord_cartesian(ylim = ylim,
                                xlim = xlim,
                                expand = FALSE) +
                geom_smooth(method=lm,
                            formula= y ~ x-1,
                            aes(group=survey_code,
                                colour=survey_code),
                            se=TRUE, size=0.5, na.rm = TRUE)

            # Save plots
            ggsave(paste0(loc_species, "/", unique(.$binomial_species),
                          ".png", sep = ''), width = 10, height = 10,
                   units = 'cm', plot = plot)
        })




### Portrait version (for manuscript) #####
    {
      ncolfig <- 6  # Set number of columns to be plotted.

      # Compute format requirements
      arrary_n_obs <- tapply(df_peak_filt$AGB_g_m2, df_peak_filt$binomial_species, length)  # Create arrary of number of observations for each species.
      valid_species <- length(arrary_n_obs)  # Calculate the number of species with equal to or more than the threshold observations.
      nrows <- ceiling(valid_species/ncolfig)  # Calculate number of rows of sub-plots.

      # Calculate maximum XY ratio in the df.
      mhag <- tapply(df_peak_filt$HAG_plotmean_of_cellmax_m, df_peak_filt$binomial_species, max)  # Create array of maximum height values for each species.
      magb <- tapply(df_peak_filt$AGB_g_m2,                  df_peak_filt$binomial_species, max)  # Create array of maximum biomass values for each species.
      yx_ratio <- max(magb/mhag)  # Calculate maximum XY ratio in the df.

      png(paste(loc_outputs, "/Figure S1 - Species level/", "Figure S1 - species-level height vs biomass (tall).png", sep = ''), width=600*ncolfig, height=550*nrows)
      par(mfrow=c(nrows, ncolfig), mar = c(10, 10, 8, 2), oma=c(10, 10, 10, 2), mgp=c(2, 2, 0))

      # Group by PFT
      for(pft in pft_list){

        # subset data by PFT
        pft_dat <- filter(df_peak_filt, plant_functional_type == pft)

        species_by_pft <- sort(unique(pft_dat$binomial_species))  # List species

        for(j in species_by_pft){
          species_dat <- filter(pft_dat, binomial_species == j)

          x <- species_dat$HAG_plotmean_of_cellmax_m
          y <- species_dat$AGB_g_m2

          n_obs <- min(length(x), length(y))  # Return number of observations.
          n_samples <- length(unique(species_dat$survey_code))  # Return number of samples.

          pft_colour <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list==pft])  # Set point colour

          ### fit models
          model_lm <- lm(y ~ x + 0, na.action=na.exclude)
          sum_lm <- summary(model_lm)

          # Extract model parameters
          lm_slope <- trunc(sum_lm$coefficients[1])
          lm_r2 <- round(sum_lm$r.squared, 2)

          # Set axes scaling parameters
          xlim <- c(0, max(x))
          ylim <- c(0, max(x)) * yx_ratio

          # Create plots
          plot(x, y, xlim=xlim, ylim=ylim, xlab="", ylab="", main=j, font.main=3,
               cex.axis=4, pch=16, cex=3.5, cex.main=5)
          points(x, y, pch=21, cex=5, col="black", bg=pft_colour, lwd=2)
          abline(model_lm, col="black", lty=3, lwd=5)
          mtext(paste("y =", lm_slope,"x"), side=3, adj=0.05, line=-4, cex=3.5);
          mylabel = bquote(italic(R)^2 == .(format(lm_r2, digits = 3)))
          mtext(mylabel, side=3, adj=0.05, line=-8, cex=3.5)
          mtext(paste("Samples =", n_samples), side=3, adj=0.05, line=-12, cex=3.5)
          mtext(paste("n =", n_obs), side=3, adj=0.05, line=-16, cex=3.5)
        }


        # Add axis labels.
        y_axis_text <- expression("Dry biomass (g m"^"-2"*")")
        x_axis_text <- "Mean canopy height (m)"
        mtext(y_axis_text, side=2, line=2, cex=5, outer=T)
        mtext(x_axis_text, side=1, line=2, cex=5, outer=T)
      }

      dev.off()  # Close plotting device

      rm(y_axis_text, x_axis_text)  # Tidy up
    }


       ### Landscape version (for visualisation) ###
    {
      ncolfig <- 8  # Set number of columns to be plotted.

            # Compute format requirements
            arrary_n_obs <- tapply(df_peak_filt$AGB_g_m2, df_peak_filt$binomial_species, length)  # Create arrary of number of observations for each species.
            valid_species <- length(arrary_n_obs)  # Calculate the number of species with greater than the threshold observations.
            nrows <- ceiling(valid_species/ncolfig)  # Calculate number of rows of sub-plots.

            # Calculate maximum XY ratio in the df.
            mhag <- tapply(df_peak_filt$HAG_plotmean_of_cellmax_m, df_peak_filt$binomial_species, max)  # Create array of maximum height values for each species.
            magb <- tapply(df_peak_filt$AGB_g_m2,                  df_peak_filt$binomial_species, max)  # Create array of maximum biomass values for each species.
            yx_ratio <- max(magb/mhag)  # Calculate maximum XY ratio in the df.

            png(paste(loc_outputs, "/", "Figure S1 - Species-level height vs biomass (wide).png", sep = ''), width=600*ncolfig, height=3000)
            par(mfrow=c(nrows, ncolfig), mar = c(10, 10, 8, 2), oma=c(10, 10, 10, 2), mgp=c(2, 2, 0))

            # Group by PFT
            for(pft in pft_list){

              # subset data by PFT
              pft_dat <- filter(df_peak_filt, plant_functional_type == pft)

              species_by_pft <- sort(unique(pft_dat$binomial_species))  # List species

              for(j in species_by_pft){
                species_dat <- filter(pft_dat, binomial_species == j)

                # if(nrow(species_dat) < min_obs){next}     # skip group if there are not enough observations.

                x <- species_dat$HAG_plotmean_of_cellmax_m
                y <- species_dat$AGB_g_m2

                n_obs <- min(length(x), length(y))  # Return number of observations.
                n_samples <- length(unique(species_dat$survey_code))  # Return number of samples.

                pft_colour <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list==pft])  # Set point colour

                ### fit models
                model_lm <- lm(y ~ x + 0, na.action=na.exclude)
                sum_lm <- summary(model_lm)

                # Extract model parameters as new list
                lm_slope <- trunc(sum_lm$coefficients[1])
                lm_r2 <- round(sum_lm$r.squared, 2)

                # Set axes scaling parameters
                xlim <- c(0, max(x))
                ylim <- c(0, max(x)) * yx_ratio

                # Create plots
                plot(x, y, xlim=xlim, ylim=ylim, xlab="", ylab="", main=j, font.main=3,
                     cex.axis=4, pch=16, cex=3.5, cex.main=5)
                points(x, y, pch=16, cex=3.5, col=pft_colour)
                abline(model_lm, col="black", lty=3, lwd=5)
                mtext(paste("y =", lm_slope,"x"), side=3, adj=0.05, line=-4, cex=3.5);
                mylabel = bquote(italic(R)^2 == .(format(lm_r2, digits = 3)))
                mtext(mylabel, side=3, adj=0.05, line=-8, cex=3.5)
                mtext(paste("Samples =", n_samples), side=3, adj=0.05, line=-12, cex=3.5)
                mtext(paste("n =", n_obs), side=3, adj=0.05, line=-16, cex=3.5)
              }

              # Add axis labels.
              y_axis_text <- expression("Dry biomass (g m"^"-2"*")")
              x_axis_text <- "Mean canopy height (m)"

              mtext(y_axis_text, side=2, line=2, cex=5, outer=T)
              mtext(x_axis_text, side=1, line=2, cex=5, outer=T)
            }

            dev.off()  # Close plotting device

            rm(y_axis_text, x_axis_text)  # Tidy up
    }










### **************************
### 5. PFT-level analysis ####
### **************************

### 5.1 Produce summary table of parameters for all PFT-level models ####
        {
            # Create empty dataframe to be populated with PFT-level parameters
            pft_summary <- data.frame(plant_functional_type = character(),
                                      observations = integer(),
                                      n_of_surveys = integer(),
                                      lm_slope = double(),
                                      lm_slope_error = double(),
                                      lm_r2 = double(),
                                      lm_p = double(),
                                      lm_loocv = double(),
                                      stringsAsFactors=FALSE
                                      )

            # List PFTs
            pft_l <- sort(unique(df_peak_ex_bryo$plant_functional_type))

            # Derive model parameters for all PFTs
            for(j in pft_l){
                pft_dat <- filter(df_peak_ex_bryo, plant_functional_type == j)

                # if(nrow(pft_dat) < min_obs){next}     # skip group if there are not enough observations.

                x <- pft_dat$HAG_plotmean_of_cellmax_m
                y <- pft_dat$AGB_g_m2

                ### fit models
                model_lm <- lm(y ~ x + 0, na.action=na.exclude)
                sum_lm <- summary(model_lm)

                ### Cross-Validation of model fits ###
                # Estimate the prediction error of a linear model via (repeated) K-fold cross-validation.
                # Split df into training and validation 'sets', fit model to
                # training set, and calculate residual error for the validation 'set'.
                # with Leave-One-Out Cross-Validation (LOOCV), training set is n-1
                # and validation set is n=1, and the exericse is repeated n times.
                # Setting K = n equals leave-one-out cross-validation.
                lm_loocv <- cvTools::repCV(model_lm, cost = rtmspe, K = nrow(pft_dat))

                # Extract model parameters as new list
                pft_info <- c(plant_functional_type = as.character(unique(pft_dat$plant_functional_type)),
                              observations = min(length(pft_dat$AGB_g_m2),length(pft_dat$HAG_plotmean_of_cellmax_m)),
                              n_of_surveys = length(unique(pft_dat$survey_code)),
                              lm_slope = trunc(sum_lm$coefficients[1]),
                              lm_slope_error = trunc(sum_lm$coefficients[2]),
                              lm_r2 = round(sum_lm$r.squared, 2),
                              lm_p = round(sum_lm$coefficients[4],4),
                              lm_loocv = trunc(as.numeric(lm_loocv$cv))
                              )

                # Convert list into dataframe.
                pft_info <- data.frame(lapply(pft_info, type.convert), stringsAsFactors=FALSE)

                # Append new row to summary table.
                pft_summary <- rbind(pft_summary, pft_info)

            }

            # Refine table of species-level model parameters
                pft_summary2 <- pft_summary  # Keeping the original 'pft_summary' for subsequent plotting

                # Compute relative LOOCV errors
                pft_summary2$lm_loocv_pc <- pft_summary2$lm_loocv/pft_summary2$lm_slope * 100  # Convert absolute errors into relative percentage error.
                pft_summary2$lm_loocv_pc <- round(pft_summary2$lm_loocv_pc, 1)

                # Drop absolute LOOCV error
                pft_summary2 <- subset(pft_summary2, select = -c(lm_loocv))

                # Change values lower than threhsold to "0.0001", and remove scientific notation in the characterised vector
                pft_summary2$lm_p[pft_summary2$lm_p < 0.0001] <- "<0.0001"
                pft_summary2$lm_p[pft_summary2$lm_p == "1e-04"] <- "0.0001"
                pft_summary2$lm_p[pft_summary2$lm_p == "2e-04"] <- "0.0002"
                pft_summary2$lm_p[pft_summary2$lm_p == "3e-04"] <- "0.0003"


            rm(pft_info)  # Tidy up

            # Export model summary as Excel file.
            model_summary_path_xlsx <- paste(loc_outputs,"/PFT summaries.xlsx", sep = "")
            write.xlsx(pft_summary2, model_summary_path_xlsx, row.names=FALSE)

            # model_summary_path_txt <- paste(loc_outputs,"/PFT summaries.txt", sep = "")
            # write.table(pft_summary2, model_summary_path_txt, row.names=FALSE, sep=",")
        }





### 5.2 Multi-panel plot of plant functional types, with constant axis ratios across all plots ####
    # Calculate maximum XY ratio in the df.
    {
    mhag <- tapply(df_peak_ex_bryo$HAG_plotmean_of_cellmax_m, df_peak_ex_bryo$plant_functional_type, max)  # Create array of maximum height values for each PFT.
    magb <- tapply(df_peak_ex_bryo$AGB_g_m2,                  df_peak_ex_bryo$plant_functional_type, max)  # Create array of maximum biomass values for each PFT.
    yx_ratio <- max(magb/mhag)                                                  # Calculate maximum XY ratio in the df.
    # Create a list to hold the plot objects.
    PFT_plotlist <- list()
    PFT_plotlist[]

    # For loop to itterate through each PFT.
    for(pft in pft_list){
      # subset data by PFT
      pft_dat <- filter(df_peak_ex_bryo, plant_functional_type == pft)

        x <- pft_dat$HAG_plotmean_of_cellmax_m
        y <- pft_dat$AGB_g_m2

        n_obs <- min(length(x), length(y))                                      # Return number of observations.
        n_species <- length(unique(pft_dat$binomial_species))                        # Return number of species
        pft_colour <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list==pft])  # Set point colour.

        model_lm <- lm(y ~ x + 0, na.action=na.exclude)                         # Fit model.
        sum_lm <- summary(model_lm)                                             # Summarise model.

        # Extract model parameters as new list
        lm_slope <- trunc(sum_lm$coefficients[1])
        lm_r2 <- round(sum_lm$r.squared, 2)

        # Set axes scaling parameters
        xlim <- c(0, max(x))
        ylim <- c(0, max(x)) * yx_ratio

        # Plot data
        (pft_plot <- ggplot(data=pft_dat, aes(x = HAG_plotmean_of_cellmax_m,
                                y = AGB_g_m2)) +
          geom_point(shape=21, na.rm=TRUE, colour="black", fill=pft_colour, alpha=0.5, ) +
          labs(x = expression("Mean canopy height (m)"),
               y = expression("Dry biomass (g m"^"-2"*")"),
               title = paste(pft)) +
          geom_smooth(method=lm,  formula= y ~ x-1, se=FALSE, size=0.5,
                      na.rm = TRUE, alpha=0.2, colour = "black") +
          coord_cartesian(xlim = c(0, max(pft_dat$HAG_plotmean_of_cellmax_m*1.05)),
                          ylim = c(0, max(pft_dat$HAG_plotmean_of_cellmax_m*1.05*yx_ratio)),
                          expand=FALSE) +
          theme_coding() +
          annotate(geom='text', x = (0.04*max(x)), y = (0.99*(max(x)* yx_ratio)), size =3, label = paste('y =', lm_slope, 'x'), hjust = 0, parse=FALSE) +
          annotate(geom='text', x = (0.04*max(x)), y = (0.91*(max(x)* yx_ratio)), size =3, label = paste('R^2 == ', round(lm_r2, 2)), hjust = 0, parse = TRUE) +
          annotate(geom='text', x = (0.04*max(x)), y = (0.80*(max(x)* yx_ratio)), size =3, label = paste('Species ==', n_species), hjust=0, parse = TRUE) +
          annotate(geom='text', x = (0.04*max(x)), y = (0.72*(max(x)* yx_ratio)), size =3, label = paste('n ==', n_obs), hjust=0, parse = TRUE)
          )

        PFT_plotlist[[pft]] <- pft_plot  # add each plot into plot list

        # Save plots
        ggsave(paste0(loc_pft, "/", pft,
                      ".png", sep = ''), width = 7, height = 7,
               units = 'cm', plot = pft_plot)


      }

    # Combine plots with patchwork
    (PFT_Figure <- PFT_plotlist$Fern + PFT_plotlist$Forb + PFT_plotlist$Graminoid +
        PFT_plotlist$Shrub + PFT_plotlist$Succulent + PFT_plotlist$Tree)

    # Export figure
    outfile <- file.path(home, "outputs/Figure 2/",paste(" Figure 2 (from R).png",sep="")) # Specify filepath
    png(filename=outfile, width = 16, height = 11, units = 'cm', res = 500)         # Save plot
    plot(PFT_Figure)
    dev.off()
    }




### *************************************************
### 6. Influence of Wind & Sun ----
### *************************************************
  # Here we examine the influence of (i) wind speed and (ii) solar elevation on
  # the relationship between mean canopy height and aboveground biomass, to try
  # to understand how sensitive these photogrammetric approaches might be to
  # envrionmental parameters.

  ### Review distribution of envrionmental parameters
    # Create histrogram of wind speeds
      (wind_histogram <- ggplot(df_peak_ex_bryo, aes(x=wind_speed)) +
         geom_histogram(binwidth=1) +
         labs(x = expression("Wind Speed (m s"^"-2"*")"),
              y = expression("Count of Harvest Plots"),
              title = expression("Histogram of wind speeds")) +
         theme_coding())

      outfile <- file.path(loc_df, paste("Histogram of wind speeds.png",sep="")) # Specify filepath
      png(filename=outfile, width = 10, height = 10, units = 'cm', res = 500)     # Save plot
      plot(wind_histogram)
      dev.off()


  ### Extract model paraemters for different species
    {
      # Create empty dataframe to be populated for each sample
      df_influence_wind <- data.frame(plant_functional_type = character(),
                            order = character(),
                            family = character(),
                            species = character(),
                            observations = integer(),
                            lm_slope = double(),
                            lm_slope_error = double(),
                            lm_r2 = double(),
                            lm_p = double(),
                            survey_code = character(),
                            site_code = character(),
                            wind_speed = double(),
                            solar_elevation = double(),
                            sky_conditions_code = double(),
                            stringsAsFactors=FALSE
                            )

    ### Extract models for each sample of each species
      # List unique species
      survey_l <- sort(unique(df_peak_filt$survey_code))

      # Derive model parameters for species
      for(j in survey_l){
        survey_dat <- filter(df_peak_filt, survey_code == j)                   # Subset data by survey code.
        species_l <- sort(unique(survey_dat$binomial_species))                 # List species present in survey.
        for(s in species_l){
          species_dat <- filter(survey_dat, binomial_species == s)             # Subset data by survey code.

          if(nrow(species_dat) < min_obs){next}                                 # skip species if there are insufficient observations.

          x <- species_dat$HAG_plotmean_of_cellmax_m
          y <- species_dat$AGB_g_m2

          # Fit models
          model_lm <- lm(y ~ x + 0, na.action=na.exclude)
          sum_lm <- summary(model_lm)

          # Extract parameters as new list
          species_info <- c(plant_functional_type = as.character(unique(species_dat$plant_functional_type)),
                            order = as.character(unique(species_dat$PlotOrder)),
                            family = as.character(unique(species_dat$PlotFamily)),
                            species = as.character(unique(species_dat$binomial_species)),
                            observations = min(length(species_dat$AGB_g_m2),length(species_dat$HAG_plotmean_of_cellmax_m)),
                            lm_slope = trunc(sum_lm$coefficients[1]),
                            lm_slope_error = trunc(sum_lm$coefficients[2]),
                            lm_r2 = round(sum_lm$r.squared, 2),
                            lm_p = round(sum_lm$coefficients[4],4),
                            survey_code = (unique(species_dat$survey_code)),
                            site_code = (unique(species_dat$site_code)),
                            wind_speed = as.character(unique(species_dat$wind_speed)),
                            solar_elevation = as.character(unique(species_dat$solar_elevation)),
                            sky_conditions_code = as.character(unique(species_dat$sky_conditions_code))
                            )

          # Convert list into dataframe.
          species_info <- data.frame(lapply(species_info, type.convert), stringsAsFactors=FALSE)

          # Append new row to summary table.
          df_influence_wind <- rbind(df_influence_wind, species_info)

          # Order species_summary_table alphabetically by plant functional group, order, family and binomial species. NB. tolower deals with case issues.
          df_influence_wind <- df_influence_wind[order(tolower(df_influence_wind$plant_functional_type),
                                             tolower(df_influence_wind$order),
                                             tolower(df_influence_wind$family),
                                             tolower(df_influence_wind$species)
                                             ),]
        }
      }

      # Tidy up environment
      rm(survey_l,
         species_l,
         survey_dat,
         species_info)

      # Filtering to subset only species with multiple samples
      df_influence_wind_filt <- df_influence_wind %>%
        group_by(species) %>%
        add_count(n_distinct(survey_code)) %>%
        filter(n>1)


      # Compute the number of species sampled more then once
      n_of_species_in_df_influence_wind <- length(unique(df_influence_wind_filt$species))
      print(n_of_species_in_df_influence_wind)
      }


      # Set scaling parameters
      max_wind <- 10
      max_slope <- 15000
      max_sun <-  round(max(df_influence_wind_filt$solar_elevation),-1)
      min_sun <-  round(min(df_influence_wind_filt$solar_elevation),-1)
      annotation_size <- 1.5

  ## Visualise wind influence
      {
      # Ceate wind plot without species annotations
      (wind.plot <- ggplot(data = df_influence_wind_filt,
                           aes(x = wind_speed,
                               y = lm_slope,
                               label = species,
                               colour = plant_functional_type,
                               shape = plant_functional_type)) +
          geom_point(alpha = 0.7, na.rm = TRUE) +
          geom_errorbar(aes(ymin=lm_slope-lm_slope_error, ymax=lm_slope+lm_slope_error), width=0.05) +
          scale_colour_manual(values = c(col_shrub, col_graminoid, col_succulent)) +
          scale_shape_manual(values = pft_shapes) +
          labs(x = expression("Wind Speed (m s"^"-1"*")"),
               y = expression("Model Slope (g m"^"-1"*")")) +
          theme_coding() +
          coord_cartesian(ylim = c(0, max_slope), xlim = c(0, max_wind), expand=FALSE) +
          geom_smooth(method=lm,
                      formula= y ~ x,
                      aes(group=species),
                      se=FALSE, size=1.0, na.rm = TRUE, alpha=0.3) +
          annotate("text", fontface=3, size = annotation_size, colour = col_graminoid, x = 4.9, y = 5900, label = "A. nelsonii") +
          annotate("text", fontface=3, size = annotation_size, colour = col_graminoid, x = 3.7, y = 1700, label = "B. macra") +
          annotate("text", fontface=3, size = annotation_size, colour = col_graminoid, x = 7.86, y = 4410, label = "B. eriopoda") +
          annotate("text", fontface=3, size = annotation_size, colour = col_graminoid, x = 3.7, y = 4900, label = "E. curvula") +
          annotate("text", fontface=3, size = annotation_size, colour = col_graminoid, x = 1.4, y = 1160, label = "P. spicata") +
          annotate("text", fontface=3, size = annotation_size, colour = col_shrub, x = 4.7, y = 8100, label = "A. tridentata") +
          annotate("text", fontface=3, size = annotation_size, colour = col_shrub, x = 7.9, y = 5000, label = "G. sarothrae") +
          annotate("text", fontface=3, size = annotation_size, colour = col_shrub, x = 4.62, y = 7450, label = "C. vulgaris") +
          annotate("text", fontface=3, size = annotation_size, colour = col_shrub, x = 6.58, y = 7100, label = "A. fasciculatum") +
          annotate("text", fontface=3, size = annotation_size, colour = col_shrub, x = 6.48, y = 5520, label = "A. sparsifolium") +
          annotate("text", fontface=3, size = annotation_size, colour = col_shrub, x = 4.8, y = 9950, label = "L. tridentata") +
          annotate("text", fontface=3, size = annotation_size, colour = col_succulent, x = 7.74, y = 11800, label = "Y. glauca") +
          theme(legend.position = c(0.15, 0.87))
      )

      # Export figure
      outfile <- file.path(loc_df, paste("Interaction wind speed and model slopes - labelled.png",sep="")) # Specify filepath
      png(filename=outfile, width = 8, height = 8, units = 'cm', res = 500)     # Save plot
      plot(wind.plot)
      dev.off()


      # Ceate wind plot without species annotations
    (wind.plot <- ggplot(data = df_influence_wind_filt,
                           aes(x = wind_speed,
                               y = lm_slope,
                               label = species,
                               colour = plant_functional_type,
                               shape = plant_functional_type)) +
        geom_point(alpha = 0.7, na.rm = TRUE) +
        geom_errorbar(aes(ymin=lm_slope-lm_slope_error, ymax=lm_slope+lm_slope_error), width=0.05) +
       scale_colour_manual(values = c(col_shrub, col_graminoid, col_succulent)) +
       scale_shape_manual(values = pft_shapes) +
        labs(x = expression("Wind Speed (m s"^"-1"*")"),
             y = expression("Model Slope (g m"^"-1"*")")) +
        theme_coding() +
        coord_cartesian(ylim = c(0, max_slope), xlim = c(0, max_wind), expand=FALSE) +
        geom_smooth(method=lm,
                    formula= y ~ x,
                    aes(group=species),
                    se=FALSE, size=1.0, na.rm = TRUE, alpha=0.3) +
        theme(legend.position = c(0.15, 0.87))
      )

    # Export figure
      outfile <- file.path(loc_df, paste("Interaction wind speed and model slopes.png",sep=""))
      png(filename=outfile, width = 8, height = 8, units = 'cm', res = 500)
      plot(wind.plot)
      dev.off()
      }


    ## Calculate rough estemate of the average affect of wind speed on allometric functions
        {
        # Create empty dataframe to be populated
        df_influence_wind_slope <- data.frame(species = character(),
                                              slope = double(),
                                              stringsAsFactors=FALSE
                                              )

        # List unique species
        species_l <- sort(unique(df_influence_wind_filt$species))

        # Derive model parameters for species
        for(s in species_l){
          species_dat <- filter(df_influence_wind_filt, species == s)

          x <- species_dat$wind_speed
          y <- species_dat$lm_slope

          # Fit models
          model_lm <- lm(y ~ x, na.action=na.exclude)
          sum_lm <- summary(model_lm)

          # Extract parameters as new list
          species_info <- c(species = as.character(unique(species_dat$species)),
                            slope = trunc(sum_lm$coefficients[2])
          )

          # Convert list into dataframe.
          species_info <- data.frame(lapply(species_info, type.convert), stringsAsFactors=FALSE)

          # Append new row to summary table.
          df_influence_wind_slope <- rbind(df_influence_wind_slope, species_info)
        }
      }

      median(df_influence_wind_slope$slope)
      mean(df_influence_wind_slope$slope)



  ### Influence of sun elevation
      # Create histrogram of solar elevation
      (solar_elevation_histogram <- ggplot(df_peak_ex_bryo, aes(x=solar_elevation)) +
          geom_histogram(binwidth=5) +
          labs(x = expression("Solar Elevation (°)"),
               y = expression("Count of Harvest Plots"),
               title = expression("Histogram of solar elevations")) +
          theme_coding())

      outfile <- file.path(loc_df, paste("Histogram of solar elevations.png",sep="")) # Specify filepath
      png(filename=outfile, width = 10, height = 10, units = 'cm', res = 500)     # Save plot
      plot(solar_elevation_histogram)
      dev.off()

      # plot solar elevation against sky conditions
      (solar_elevation_vs_sky <- ggplot(df_peak_ex_bryo, aes(x=solar_elevation, y=sky_conditions_code)) +
          geom_point() +
          labs(x = expression("Solar Elevation (°)"),
               y = expression("Sky code (0 = clear)")) +
          theme_coding())

      outfile <- file.path(loc_df, paste("Sun elevation vs sky conditions.png",sep="")) # Specify filepath
      png(filename=outfile, width = 10, height = 10, units = 'cm', res = 500)     # Save plot
      plot(solar_elevation_vs_sky)
      dev.off()


      ## Filter dataframe to more than one sample with clear sky codes ('0')
      df_influence_sun <- df_influence_wind_filt %>%
        filter(sky_conditions_code == 0) %>%                                    # Filter by sky conditions
        group_by(species) %>%                                                   # Gorup by species.
        add_count(n_distinct(survey_code)) %>%                                 # Count number of survey codes per (species) group.
        filter(n>1) %>%                                                         # Filter to more than one sample
        select(-c('n_distinct(survey_code)'))                                  # Drop old, now incorrect, column.

      ## create plot
        (sun.plot <- ggplot(data = df_influence_sun,
                             aes(x = solar_elevation,
                                 y = lm_slope,
                                 label = species,
                                 colour = plant_functional_type,
                                 shape = plant_functional_type)) +
           geom_point(alpha = 0.7, na.rm = TRUE) +
           geom_errorbar(aes(ymin=lm_slope-lm_slope_error, ymax=lm_slope+lm_slope_error), width=0.05) +
            scale_colour_manual(values = c(col_shrub, col_graminoid)) +
            scale_shape_manual(values = pft_shapes) +
           labs(x = expression("Sun Elevation (°)"),
                y = expression("Model Slope (g m"^"-1"*")")) +
           theme_coding() +
           coord_cartesian(ylim = c(0, max_slope), xlim = c(min_sun, max_sun), expand=FALSE) +
           geom_smooth(method=lm,
                       formula= y ~ x,
                       aes(group=species),
                       se=FALSE, size=1.0, na.rm = TRUE, alpha=0.3) +
           theme(legend.position = c(0.15, 0.915))
        )

        # Export figure
        outfile <- file.path(loc_df, paste("Interaction sun elevation and model slopes.png",sep="")) # Specify filepath
        png(filename=outfile, width = 8, height = 8, units = 'cm', res = 500)     # Save plot
        plot(sun.plot)
        dev.off()


  ### Combine plots with patchwork
        (Interactions <- wind.plot + sun.plot +
          plot_annotation(tag_levels = 'A') &
          theme(plot.tag.position = c(0.0, 0.97))  # Control horizontal and vertical position of panel labels
             )

    # Export figure
      outfile <- file.path(loc_outputs, paste("Figure 3/Figure 3 - Interactions.png",sep="")) # Specify filepath
      png(filename=outfile, width = 16, height = 8, units = 'cm', res = 500)     # Save plot
      plot(Interactions)
      dev.off()



  ### 6.1 Modelling influence ----
    # Exploratory post-hoc analysis to test whether wind speed had a significant
    # interaction effect on the relationship between mean canopy height and biomass.
    # The null hypotheses is that wind speed has no effect on the relationship
    # between height and biomass. With the current dataset we are unable to control for
    # differences in site or anticeedent condition. However, we do expect sensitivity
    # to wind speed will differ between taxanomic groups, and we can account for this
    # by treating species as a random effect.

    # We think it is sensible to treat wind speed as a continuous  (fixed) rather than categorical  (random) variable? (at most 4 levels, but usually 2)
    # model predicts biomass as a function of canopy height interacting with wind speed, with speceis included as a random effect
    model_wind <- lmer(AGB_g_m2 ~ HAG_plotmean_of_cellmax_m * wind_speed + (1|binomial_species), data = df_peak)
    summary(model_wind)


    # NB. Including plant_functional_type and then binomial_species as nested random effects would
    # better accounting for hierarchical structure in these data, but there isn't yet enough data
    # for this to be informative.
        # model_wind <- lmer(AGB_g_m2 ~ HAG_plotmean_of_cellmax_m * wind_speed + (1|plant_functional_type/binomial_species), data = df_peak)


    # NB. we don't have enough data to usefully include survey_code as a random effect (representing site and year)
      # model_wind <- lmer(AGB_g_m2 ~ HAG_plotmean_of_cellmax_m * wind_speed + (1|survey_code), data = df_peak)


    # # Include plant_functional_type and then binomial_species as nested random effects
    # # accounting for (some of) the hierarchical structure of the data
    # summary(model_wind)


    # Visualise model predictions
    ggpredict(model_wind, terms = c("HAG_plotmean_of_cellmax_m")) %>% plot()

    ggpredict(model_wind, terms = c("wind_speed")) %>% plot()
    # Is this plot useful for illustrating the overall influence of wind on the allometric function?


    # Visualise random effects
    library(glmmTMB)
    (re.effects <- plot_model(model_wind, type = "re", show.values = TRUE))


    # predictions <- ggpredict(model_wind, terms = c("HAG_plotmean_of_cellmax_m"))

    # # Plot model predictions on data
    # (plot.model <- ggplot() +
    #     geom_line(data = predictions, aes(x = x, y = predicted),
    #               size = 0.5) +
    #     geom_ribbon(data = predictions, aes(ymin = conf.low, ymax = conf.high, x = x), alpha = 0.4) +
    #     geom_point(data = df_peak, aes(x = HAG_plotmean_of_cellmax_m, y = AGB_g_m2),
    #                alpha = 0.1, size = 2) +
    #     theme_classic() +
    #     labs(x = "HAG_plotmean_of_cellmax_m", y = "AGB_g_m2"))










### *************************************************
### 7. Output text report from analysis ----
### *************************************************

  ### Prepare output metircs
    today <- as.character(Sys.Date())
    species_R2_mean <- round(mean(species_summary3$lm_r2),2)
    species_R2_min <- round(min(species_summary3$lm_r2),2)
    species_R2_max <- round(max(species_summary3$lm_r2),2)


  ### Save report
    report_path <- file(paste0(loc_outputs,"/Report 4 - survey analysis.txt"))
    cat("Drone Allometry Experiment: Summary Report \n",
        "Andrew Cunliffe <andrewmcunliffe@gmail.com> \n",
        "Generated on: ", today, "\n",
        "---------------------------------------------------------------------------------", "\n", "\n",
        "For species-level models, R2 ranges from ", species_R2_min, " to ", species_R2_max, ", with a mean of: ",
         species_R2_mean, "\n",
        file = report_path)






### *************************************************
#### 8. Sevilleta Analysis ----
### *************************************************

# working with: sev_df
# Sevilleta species sampled include:
#  c("Larrea tridentata", "Bouteloua eriopoda", "Gutierrezia sarothrae", "Yucca elata")
# and also several one off species (huniper, pinion, cane cholla cacti, prickely pear cacti)
# Compute scaling parameters from minimum and maximum values
sev_max_mean_hag <- max(df_sev$HAG_plotmean_of_cellmax_m, na.rm = TRUE)
sev_max_agb <- 1.35*max(df_sev$AGB_g_m2, na.rm = TRUE)


df_sev %>%
    group_by(binomial_species) %>%
    add_count(binomial_species) %>%
    filter(n>3) %>%
    do({
        plot <- ggplot(., aes(x = HAG_plotmean_of_cellmax_m,
                              y = AGB_g_m2,
                              colour = survey_code)) +
            geom_point(shape = 1, na.rm = TRUE) +
            scale_colour_viridis_d() +
            labs(x = expression("Mean canopy height (m)"),
                 y = expression("Dry biomass (g m"^"-2"*")"),
                 title = paste(.$binomial_species)) +
            theme_coding() +
            theme(plot.title = element_text(face="italic")) +
            theme(legend.position = c(0.35, 0.8)) +
            coord_cartesian(ylim = c(0, sev_max_agb),
                            xlim = c(0, sev_max_mean_hag),
                            expand = FALSE) +
            geom_smooth(method="lm",
                        formula= y ~ x-1,
                        aes(group=survey_code,
                            colour=survey_code),
                        se=TRUE, size=0.5, na.rm = TRUE)
        ggsave(paste0(loc_sevilleta, "/", unique(.$binomial_species),
                      ".png", sep = ''), width = 10, height = 10,
               units = 'cm', plot = plot)                                       # Save plots
    })


# Mean HAG Vs. AGB grouped by species
# (mean of maximum) canopy height as predictor of aboveground biomass by binomial taxa. Creates:
# (i) individual plots,
# (ii) multipanel figure,
# (iii) model summary statistics (as Excel sheet)

# Create dataframe to collate model summary statistics.
sev_summary_table <- data.frame(binomial_species = character(),
                                survey_code = character(),
                                nls_slope = double(),
                                nls_slope_error = double(),
                                nls_r2 = double(),
                                nls_p = double(),
                                rob_slope = double(),
                                rob_slope_error = double(),
                                rob_r2 = double(),
                                rob_p = double(),
                                Observations = integer(),
                                stringsAsFactors=FALSE
)

#
# # Export Sevilleta IAV model summary
# model_summary_path <- paste(loc_sevilleta, "/Sevilleta model summaries.xlsx", sep = "")
# write.xlsx(sev_summary_table, model_summary_path, row.names=FALSE)
#






# check whether the unwanted file exists and remove it
if (file.exists("Rplots.pdf")){
    file.remove("Rplots.pdf")
}

