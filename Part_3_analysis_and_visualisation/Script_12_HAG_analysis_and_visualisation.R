# Script for statistical analysis and visualisation of data for the drone photogrammetry allometry project.

###*************************************************
#### Load packages ----
###*************************************************
# Download packags
# install.packages("ggfortify")

# Load packags
library(devtools)
library(tidyverse)                                                              # For making life better.
library(viridis)                                                                # For friendly colour palettes.
library(scales)                                                                 # used for illustrating colours
library(sf)                                                                     # For working with Simple Feature objects
library(rnaturalearth)                                                          # Import Earth dataset global maps
library(rnaturalearthdata)                                                      # Import datasets for global map
library(patchwork)                                                              # A lovely tool for combining plots
library(grid)                                                                   # required for plot annotation
library(gridExtra)                                                              # for arranging multi-panel plots.
library(ggpubr)                                                                 # for arranging multi-panel plots.
# devtools::install_github("valentinitnelav/plotbiomes") # because the plotbiomes package wasn't yet avaialble for R 3.6.1                                                           # Whittaker biomes for climate space plot
library(plotbiomes)                                                             # Whittaker biomes for climate space plot
library(lme4)                                                                   # For linear mixed effects models.
library(lmerTest)                                                                   # For linear mixed effects models.
library(ggeffects)                                                              # For plotting mixed effects models.
library(sjPlot)                                                                 # For plotting mixed effects models.
library(cvTools)                                                                # USed for cross-validation of models
library(RColorBrewer)
library(scales)                                                                 # Used for extracting hexadecimal colour codes
library(ggfortify)                                                              # For ggplot based evaluators of regression models
library(broom)                                                                  # For summarising model outputs.
library(optimx)                                                                 # For GLMM
library(car)                                                                    # For GLMM
library(MuMIn)                                                                  # for calculating marginal and conditional fixed effects.
library(broom.mixed)                                                            # For GLMM
library(performance)                                                            # Evaluating mixed effects models
library(see)                                                                    # Evaluating mixed effects models



###*************************************************
#### Load data ----
###*************************************************
df <- read_csv("outputs/0_processed_data/processed_database.csv",
               col_types = cols())                                              # Read in processed data base.


#### Subset data ----
df <- df %>%
    filter(!is.na(AGB_g_m2) & !is.na(HAG_plotmean_of_cellmax_m))                # Filter for observations with both HAG and AGB values.


# Subset observations during peak biomass only
df_peak <- df %>%
  filter(PeakBiomass == TRUE) %>%                                               # Subset observations from peak biomass.
  filter(!is.na(AGB_g_m2) & !is.na(HAG_plotmean_of_cellmax_m))                  # Filter for observations with both HAG and AGB values.


# # Subset dataframe for analysis of wind influence
# df_for_wind <- df %>%
#   filter(PeakBiomass == TRUE) %>%                                               # Subset observations from peak biomass.
#   filter(!is.na(AGB_g_m2) & !is.na(HAG_plotmean_of_cellmax_m)) %>%              # Filter for observations with both HAG and AGB values.
#   filter(!plant_functional_type %in% c("Bryophyte", "Succulent")) %>%           # Remove invalid Bryophyte, and Succulent observations so that models will converge.
#   filter(!binomial_species %in% c("Mixed grasses", "Unknown spp."))             # Remove unwanted 'species'.

# Subset dataframe for analysis of sun influence
# df_for_sun <- df %>%
#   filter(PeakBiomass == TRUE) %>%                                               # Subset observations from peak biomass.
#   filter(!is.na(AGB_g_m2) & !is.na(HAG_plotmean_of_cellmax_m)) %>%              # Filter for observations with both HAG and AGB values.
#   filter(sky_conditions_code <= 4) %>%                                          # Filter observations collected under clear sky conditions.
#   filter(!plant_functional_type %in% c("Bryophyte")) %>%                        # Remove invalid bryophyte observations.
#   # filter(plant_functional_type %in% c("Shrub", "Graminoid")) %>%               # Retain only well sampled shrub and graminois PFT.
#   group_by(binomial_species) %>%
#   filter(n() >= min_obs)                                                        # Filter by threshold number of observations

# Subset df with unique information for each survey for reporting in supplementary.
df_survey <- df %>%
  select(survey_code,
         Investigators,
         SiteName,
         Country,
         SiteLatitude,
         SiteLongitude,
         SiteElevation,
         MAT,
         MAP,
         KoppenCC,
         KoppenCC_long,
         community_description,
         community_species,
         soil,
         disturbance,
         DateSurvey,
         SurveyDateTimeUTC,
         DateHarvest,
         PeakBiomass,
         EPSG,
         UTM_zone,
         Notes_on_survey_harvest,
         wind_speed,
         sky_conditions_code,
         sky_conditions,
         drone_and_sensor,
         solar_elevation,
         solar_noon_elevation,
         solar_elevation_offset,
         solar_noon_utc,
         minutes_offset_from_solar_noon,
         site_code
         ) %>%
  distinct ()


df_peak_ex_bryo <- df %>%
  filter(PeakBiomass == TRUE) %>%                                               # Subset observations from peak biomass.
  filter(plant_functional_type != "Bryophyte") %>%                              # Remove bryophyte observations.
  filter(!is.na(AGB_g_m2) & !is.na(HAG_plotmean_of_cellmax_m))                  # Filter for observations with both HAG and AGB values.

# Subset species with more than theshold number of observations.
min_obs <- 4                                                                    # Set minimum threshold number of observations for species-level analysis.
df_peak_filt <- df_peak %>%
  group_by(binomial_species) %>%
  filter(n() >= min_obs) %>%                                                    # Filter by threshold number of observations
  filter(plant_functional_type != "Bryophyte") %>%                              # Remove bryophyte observations.
  filter(!binomial_species %in% c("Mixed grasses", "Unknown spp."))             # Remove unwanted 'species'.


# Subset observations collected under clear sky conditions.
df_peak_filt_clear <- df_peak_filt %>%
  filter(sky_conditions_code == 0)

df_sev <- df %>%
  filter(SevilletaIAV == TRUE) %>%                                              # Subset observations from Sevilleta sites.
  filter(!is.na(AGB_g_m2) & !is.na(HAG_plotmean_of_cellmax_m))                  # Filter for observations with both HAG and AGB values.



###*************************************************
#### Create plot themes, symbology and scaling  ----
###*************************************************
theme_plots <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 10, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 10,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
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
                    "Tree",
                    "Succulent"
                    )

    # Assign PFT colours
      # Hexadecimal colour picker is useful for manual selection:
      # https://www.google.com/search?safe=off&client=firefox-b-d&sxsrf=ACYBGNRRbmRH08sX2olIHXglvm_Axq3oEQ%3A1580896026902&ei=Go86XoPjNqKDhbIPxoiToA0&q=hexadecimal+colour+picker&oq=hexadecimal+colour+picker&gs_l=psy-ab.3..0l2j0i22i30l2j0i22i10i30l2.2736.3423..3659...0.0..0.56.323.6......0....1..gws-wiz.......35i39.asZZ3ZLIbwk&ved=0ahUKEwiD2PDQkLrnAhWiQUEAHUbEBNQQ4dUDCAo&uact=5
      pft_colours <- c("#047cd1",
                       "#c33ce6",
                       "#aaeb07",
                       "#2c8232",
                       "#755504",
                       "#faaf00"
                       )
      scales::show_col(pft_colours)  # Plot colour pallet

    # Set shape parameters
      pft_shapes <- c(15, 16, 17, 18, 25, 22)

    # Combine into dataframe
      pft_symbology <- data.frame(pft_list, pft_colours, pft_shapes)

    # Specify PFT colours
      col_fern <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Fern"])
      col_forb <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Forb"])
      col_graminoid <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Graminoid"])
      col_shrub <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Shrub"])
      col_tree <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Tree"])
      col_succulent <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list=="Succulent"])



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
### 1a. Global Map ----
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

        ## NB. Ideally this figure would use the small-scale coastline dataset to
        ## eliminate 'clutter' of islands. However, there appears to be a bug in
        ## coord_sf that prevents the small-scale coastline dataset from being
        ## reprojected to the Robson projections
        ## (https://github.com/tidyverse/ggplot2/issues/3949)
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
        ggsave("outputs/Global map from R.png", width = 8, height = 6, dpi=500)





### 1b Climate Space ----
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
          theme_plots() +
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
          theme(legend.position = c(0.34, 0.736),
                legend.text = element_text(size = 9.6)) +
          # theme(legend.position = c(0.3, 0.736),
                # legend.text = element_text(size = 7.8, face = 'bold')) +
          labs(x = expression("Mean Annual Temperature " ( degree~C)),
               y = expression("Mean Annual Precipitation (cm yr"^"-1"*")"))
          )

# Export climate space plot
png("outputs/Sampled climate space (peak biomass).png",
    width = 11, height = 11, units = 'cm', res = 500)         # Save plot
plot(Whittaker_plot)
dev.off()

rm(biomes_tbl)                                                                  # Tidying up


### Combine Figure 1 parts a and b using the patchwork package
(figure1 <- global_map / Whittaker_plot +
    plot_annotation(tag_levels = 'A') &
    theme(plot.tag.position = c(0.0, 0.97)  # Control horizontal and vertical position of panel labels
          )
  )

# Export figure
png("outputs/Figure_1/Figure 1 (parts A and B).png",
    width = 12, height = 16, units = 'cm', res = 500)         # Save plot
plot(figure1)
dev.off()



### ******************************
### 2. df-level analysis ####
### ******************************

### Visualisation of solar offsets
  plot_solar_offset_mins <- hist(df$minutes_offset_from_solar_noon)

  png("outputs/1_dataset_level/Histogram of time offset between survey and solar noon.png",
      width = 10, height = 10, units = 'cm', res = 500)       # Save plot
  plot(plot_solar_offset_mins)
  dev.off()

  plot_solar_offset_degrees <- hist(df$solar_elevation_offset)
  png("outputs/1_dataset_level/Histogram of sun elevation offset between survey and solar noon.png",
      width = 10, height = 10, units = 'cm', res = 500)       # Save plot
  plot(plot_solar_offset_degrees)
  dev.off()


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
#     theme_plots() +
#     theme(plot.title = element_text(face="italic"),
#           legend.title = element_text(size=8),
#           legend.position = c(0.85, 0.79)) +
#     coord_cartesian(ylim = c(0, max_agb), xlim = c(0, max_mean_hag), expand=FALSE) +
#     geom_smooth(method="lm", formula= y ~ x-1,
#                 aes(group=plant_functional_type,
#                     colour=plant_functional_type),
#                 se=FALSE, size=0.5, na.rm = TRUE)
#   )






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
        theme_plots() +
        theme(legend.position = c(0.35, 0.8)) +
        coord_cartesian(ylim = c(0, 1.2 * max(.$AGB_g_m2)),
                        xlim = c(0, 1.2 * max(.$HAG_plotmean_of_cellmax_m)),
                        expand = FALSE) +
        geom_smooth(method=lm,
                    formula= y ~ x-1,
                    aes(group=binomial_species),
                    se=FALSE, size=0.5, na.rm = TRUE, alpha=0.2)
    ggsave(paste0("outputs/2_survey_level/", unique(.$survey_code),
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
                                lm_adjr2 = double(),
                                tstatistic = double(),
                                lm_p = double(),
                                lm_loocv = double(),
                                shapiro_wilk_normality_test_p_value = double(),
                                Kendall_cc_tau = double(),
                                Kendall_cc_p = double(),
                                stringsAsFactors=FALSE
  )

  # List unique species
  species_l <- sort(unique(df_peak_filt$binomial_species))

  # Derive model parameters for species
  for(species in species_l){
    species_dat <- filter(df_peak_filt, binomial_species == species)

    if(nrow(species_dat) < min_obs){next}     # skip group if there are not enough observations.

    x <- species_dat$HAG_plotmean_of_cellmax_m
    y <- species_dat$AGB_g_m2

    ### fit models
    model_lm <- lm(y ~ x + 0, na.action=na.exclude)
    sum_lm <- summary(model_lm)

    # Test normality with Shapiro-Wilk test
    residuals <- resid(model_lm)
    normality_test <- shapiro.test(residuals)

    # Kendall correlation coefficint
    cc <- cor.test(x, y, method="kendall")

    ### Cross-Validation of model fits ###
    ## Estimate the prediction error of a linear model via (repeated) K-fold cross-validation.
    ## Split df into training and validation 'sets', fit model to
    ## training set, and calculate residual error for the validation 'set'.
    ## with Leave-One-Out Cross-Validation (LOOCV), training set is n-1
    ## and validation set is n=1, and the exericse is repeated n times.
    ## Setting K = n equals leave-one-out cross-validation.
    lm_loocv <- cvTools::repCV(model_lm, cost = rtmspe, K = nrow(species_dat))

    # Extract model parameters as new list
    species_info <- c(plant_functional_type = as.character(unique(species_dat$plant_functional_type)),
                      order = as.character(unique(species_dat$plot_order)),
                      family = as.character(unique(species_dat$plot_family)),
                      species = as.character(unique(species_dat$binomial_species)),
                      observations = min(length(species_dat$AGB_g_m2),length(species_dat$HAG_plotmean_of_cellmax_m)),
                      n_of_surveys = length(unique(species_dat$survey_code)),
                      lm_slope = trunc(sum_lm$coefficients[1]),
                      lm_slope_error = trunc(sum_lm$coefficients[2]),
                      lm_r2 = round(sum_lm$r.squared, 2),
                      lm_adjr2 = round(sum_lm$adj.r.squared, 2),
                      tstatistic = round(sum_lm[["coefficients"]][, "t value"], 3),
                      lm_p = round(sum_lm$coefficients[4], 4),
                      lm_loocv = trunc(as.numeric(lm_loocv$cv)),
                      shapiro_wilk_normality_test_p_value = round(normality_test$p.value[1], 3),
                      Kendall_cc_tau = round(cc$estimate,3),
                      Kendall_cc_p = round(cc$p.value,4)
    )

    # Convert list into dataframe.
    species_info <- data.frame(lapply(species_info, type.convert), stringsAsFactors=FALSE)

    # Append new row to summary table.
    species_summary <- rbind(species_summary, species_info)

    # Create diagnostic plots for regression model (with base R)
    png(paste0("outputs/3_species_level/diagnostics/", species, "- diagnostic.png"),
        width = 16, height = 16, units = "cm", res = 300)
    par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
    plot(model_lm)
    dev.off()

    # Create histogram of canopy heights
    (canopy_heights_histogram <- ggplot(species_dat, aes(x=HAG_plotmean_of_cellmax_m)) +
      geom_histogram(binwidth=0.05) +
      labs(x = expression("Canopy height (m)"),
              y = expression("Sample count"),
      title = expression("Histogram of canopy heights")) +
         theme_plots())

      png(paste0("outputs/3_species_level/histograms/", species, "-heights.png"),
          width = 16, height = 16, units = "cm", res = 300)
      plot(canopy_heights_histogram)
      dev.off()
    }

  rm(species_info)                                                              # Tidy up

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
    species_summary3 <- subset(species_summary2, select = -c(lm_loocv,
                                                             shapiro_wilk_normality_test_p_value))

  # Change values lower than threhsold to "0.0001", and remove scientific notation in the characterised vector
    species_summary3$lm_p[species_summary2$lm_p < 0.0001] <- "<0.0001"
    species_summary3$lm_p[species_summary2$lm_p == "1e-04"] <- "0.0001"
    species_summary3$lm_p[species_summary2$lm_p == "2e-04"] <- "0.0002"
    species_summary3$lm_p[species_summary2$lm_p == "3e-04"] <- "0.0003"
    species_summary3$lm_p[species_summary2$lm_p == "4e-04"] <- "0.0004"
    species_summary3$lm_p[species_summary2$lm_p == "5e-04"] <- "0.0005"
    species_summary3$lm_p[species_summary2$lm_p == "6e-04"] <- "0.0006"
    species_summary3$lm_p[species_summary2$lm_p == "7e-04"] <- "0.0007"
    species_summary3$lm_p[species_summary2$lm_p == "8e-04"] <- "0.0008"
    species_summary3$lm_p[species_summary2$lm_p == "9e-04"] <- "0.0009"

    species_summary3$lm_p[Kendall_cc_p$lm_p < 0.0001] <- "<0.0001"
    species_summary3$lm_p[Kendall_cc_p$lm_p == "1e-04"] <- "0.0001"
    species_summary3$lm_p[Kendall_cc_p$lm_p == "2e-04"] <- "0.0002"
    species_summary3$lm_p[Kendall_cc_p$lm_p == "3e-04"] <- "0.0003"
    species_summary3$lm_p[Kendall_cc_p$lm_p == "4e-04"] <- "0.0004"
    species_summary3$lm_p[Kendall_cc_p$lm_p == "5e-04"] <- "0.0005"
    species_summary3$lm_p[Kendall_cc_p$lm_p == "6e-04"] <- "0.0006"
    species_summary3$lm_p[Kendall_cc_p$lm_p == "7e-04"] <- "0.0007"
    species_summary3$lm_p[Kendall_cc_p$lm_p == "8e-04"] <- "0.0008"
    species_summary3$lm_p[Kendall_cc_p$lm_p == "9e-04"] <- "0.0009"


  # Export model summary
    write.csv(species_summary3, "outputs/summary of species models.csv", row.names=FALSE)
}





# ******************************************************
# 4.2 Create individual species plots of HAG versus ABG ####
  ## NB. 'add_count(binomial_species)' and 'filter(n>X)' are used to exclude observations with fewer than X observations from the rest of the pipe.
  ## Calculate optimum XY ratio for consistent display.
    mhag <- tapply(df_peak_filt$HAG_plotmean_of_cellmax_m, df_peak_filt$binomial_species, max)  # Create array of maximum height values for each species.
    magb <- tapply(df_peak_filt$AGB_g_m2,                  df_peak_filt$binomial_species, max)  # Create array of maximum biomass values for each species.
    yx_ratio <- max(magb/mhag)  # Calculate maximum XY ratio in the df.

  # Create plots of each species, coloured by survey code
    df_peak_filt %>%
        group_by(binomial_species) %>%
        add_count(binomial_species) %>%
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
                theme_plots() +
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
            ggsave(paste0("outputs/3_species_level/", unique(.$binomial_species),
                          ".png"), width = 10, height = 10, units = 'cm', plot = plot)
        })




# 4.3 Create multi-panel species figure ####
# with pooled samples for species. This was implemented in a for loop because facet_wrap methods cannot apply constant axis ratio.
{
  # Calculate maximum XY ratio in the df.
  mhag <- tapply(df_peak_filt$HAG_plotmean_of_cellmax_m, df_peak_filt$plant_functional_type, max)  # Create array of maximum height values for each PFT.
  magb <- tapply(df_peak_filt$AGB_g_m2,                  df_peak_filt$plant_functional_type, max)  # Create array of maximum biomass values for each PFT.
  yx_ratio <- max(magb/mhag)                                                    # Calculate maximum XY ratio in the df.

  species_plotlist <- list()                                                    # Create a list to hold the plot objects.

  # For loop to itterate through each species
  for(pft in pft_list){
    pft_dat <- filter(df_peak_filt, plant_functional_type == pft)               # subset data by PFT
    species_by_pft <- sort(unique(pft_dat$binomial_species))                    # List species in PFT

    for(species in species_by_pft){
      species_dat <- filter(pft_dat, binomial_species == species)

      if(nrow(species_dat) < min_obs){next}                                     # skip group if there are not enough observations.

      x <- species_dat$HAG_plotmean_of_cellmax_m
      y <- species_dat$AGB_g_m2
      model_lm <- lm(y ~ x + 0, na.action=na.exclude)                           # Fit model.
      sum_lm <- summary(model_lm)                                               # Summarise model.
      lm_slope <- trunc(sum_lm$coefficients[1])                                 # Extract model parameters
      # lm_r2 <- round(sum_lm$r.squared, 2)                                       # Extract model parameters
      lm_adjr2 <- round(sum_lm$adj.r.squared, 2)                                       # Extract model parameters
      n_samples <- length(unique(species_dat$survey_code))                      # Return number of samples.
      n_obs <- min(length(x), length(y))                                        # Return number of observations.

      # Set axes scaling parameters
      pft_colour <- as.character(pft_symbology$pft_colours[pft_symbology$pft_list==pft])  # Set point colour

      # Add (local) plot objects to a plot list in the global environment
      species_plotlist[[species]] <- local({

      # Plot data
      (species_plot <- ggplot(data=species_dat,
                              aes(x = HAG_plotmean_of_cellmax_m,
                                  y = AGB_g_m2)) +
         geom_point(shape=21, na.rm=TRUE, colour="black", fill=pft_colour, alpha=0.5, ) +
         labs(x = "", # expression("Mean canopy height (m)"),
              y = "", #expression("Dry biomass (g m"^"-2"*")"),
              title = paste(species)) +
         geom_smooth(method=lm,  formula= y ~ x-1, se=FALSE, size=0.5,
                     na.rm = TRUE, alpha=0.2, colour = "black") +
         coord_cartesian(xlim = c(0, max(species_dat$HAG_plotmean_of_cellmax_m*1.05)),
                         ylim = c(0, max(species_dat$HAG_plotmean_of_cellmax_m*1.05*yx_ratio)),
                         expand=FALSE) +
         scale_x_continuous(breaks = c(0, round(max(species_dat$HAG_plotmean_of_cellmax_m)/2,2), round(max(species_dat$HAG_plotmean_of_cellmax_m),2)),
                          limits = c(0, max(species_dat$HAG_plotmean_of_cellmax_m)*1.05)
                          ) +
         theme_plots() +
         theme(plot.title = element_text(face="italic", size = 8.5),
               plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = , "cm")) +
         annotate(geom='text', x = (0.04*max(x)), y = (1*(max(x)* yx_ratio)), size =2.9, label = paste('y =', lm_slope, 'x'), hjust = 0, parse=FALSE) +
         annotate(geom='text', x = (0.04*max(x)), y = (0.89*(max(x)* yx_ratio)), size =2.9, label = paste('Adj.R^2 == ', round(lm_adjr2, 2)), hjust = 0, parse = TRUE) +
         annotate(geom='text', x = (0.04*max(x)), y = (0.77*(max(x)* yx_ratio)), size =2.9, label = paste('Surveys ==', n_samples), hjust=0, parse = TRUE) +
         annotate(geom='text', x = (0.04*max(x)), y = (0.67*(max(x)* yx_ratio)), size =2.9, label = paste('n ==', n_obs), hjust=0, parse = TRUE)
      )
      })
  }
}

  ### Portrait version ###
  # Combine plots with patchwork
  species_figure_p <- wrap_plots(species_plotlist, ncol = 6)

  # Export figure
  png("outputs/Figure_S1_species_level/Figure S1 - species-level height vs biomass.png",
      width = 29, height = 30, units = 'cm', res = 400)         # Save plot
  plot(species_figure_p)
  dev.off()

  ### Landscape version ###
  # Combine plots with patchwork
  species_figure_l <- wrap_plots(species_plotlist, ncol = 7)

  # Export figure
  png("outputs/species_level_present/Species-level height vs biomass.png",
      width = 39, height = 27, units = 'cm', res = 400)         # Save plot
  plot(species_figure_l)
  dev.off()
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
                                      lm_adjr2 = double(),
                                      tstatistic = double(),
                                      lm_p = double(),
                                      lm_loocv = double(),
                                      Kendall_cc_tau = double(),
                                      Kendall_cc_p = double(),
                                      stringsAsFactors=FALSE
                                      )

            # List PFTs
            pft_l <- sort(unique(df_peak_ex_bryo$plant_functional_type))

            # Derive model parameters for all PFTs
            for(pft in pft_l){
                pft_dat <- filter(df_peak_ex_bryo, plant_functional_type == pft)
                pft
                # if(nrow(pft_dat) < min_obs){next}     # skip group if there are not enough observations.

                x <- pft_dat$HAG_plotmean_of_cellmax_m
                y <- pft_dat$AGB_g_m2

                ### fit models
                model_lm <- lm(y ~ x + 0, na.action=na.exclude)
                sum_lm <- summary(model_lm)

                # Kendall correlation coefficint
                cc <- cor.test(x, y, method="kendall")

                ### Cross-Validation of model fits ###
                ## Estimate the prediction error of a linear model via (repeated) K-fold cross-validation.
                ## Split df into training and validation 'sets', fit model to
                ## training set, and calculate residual error for the validation 'set'.
                ## with Leave-One-Out Cross-Validation (LOOCV), training set is n-1
                ## and validation set is n=1, and the exericse is repeated n times.
                ## Setting K = n equals leave-one-out cross-validation.
                lm_loocv <- cvTools::repCV(model_lm, cost = rtmspe, K = nrow(pft_dat))

                # Extract model parameters as new list
                pft_info <- c(plant_functional_type = as.character(unique(pft_dat$plant_functional_type)),
                              observations = min(length(pft_dat$AGB_g_m2),length(pft_dat$HAG_plotmean_of_cellmax_m)),
                              n_of_surveys = length(unique(pft_dat$survey_code)),
                              lm_slope = trunc(sum_lm$coefficients[1]),
                              lm_slope_error = trunc(sum_lm$coefficients[2]),
                              lm_r2 = round(sum_lm$r.squared, 2),
                              lm_adjr2 = round(sum_lm$adj.r.squared, 2),
                              tstatistic = round(sum_lm[["coefficients"]][, "t value"], 3),
                              lm_p = round(sum_lm$coefficients[4],4),
                              lm_loocv = trunc(as.numeric(lm_loocv$cv)),
                              Kendall_cc_tau = round(cc$estimate,3),
                              Kendall_cc_p = round(cc$p.value,4)
                              )

                # Convert list into dataframe.
                pft_info <- data.frame(lapply(pft_info, type.convert), stringsAsFactors=FALSE)

                # Append new row to summary table.
                pft_summary <- rbind(pft_summary, pft_info)

                # Create diagnostic plots for regression model (with base R)
                png(paste0("outputs/4_PFT_level_plots/diagnostics/", pft, "- diagnostic.png"),
                    width = 16, height = 16, units = "cm", res = 300)
                par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
                plot(model_lm)
                dev.off()
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

                pft_summary2$Kendall_cc_p[pft_summary2$Kendall_cc_p < 0.0001] <- "<0.0001"

            rm(pft_info)  # Tidy up

            # Export model summary.
            write.csv(pft_summary2, "outputs/summary of PFT models.csv", row.names=FALSE)
        }




### 5.2 Multi-panel plot of plant functional types, with constant axis ratios across all plots ####
    # Calculate maximum XY ratio in the df.
    {
    mhag <- tapply(df_peak_ex_bryo$HAG_plotmean_of_cellmax_m, df_peak_ex_bryo$plant_functional_type, max)  # Create array of maximum height values for each PFT.
    magb <- tapply(df_peak_ex_bryo$AGB_g_m2,                  df_peak_ex_bryo$plant_functional_type, max)  # Create array of maximum biomass values for each PFT.
    yx_ratio <- max(magb/mhag)                                                  # Calculate maximum XY ratio in the df.
    # Create a list to hold the plot objects.
    PFT_plotlist <- list()

    # Create list with intervals values for x-axis labels
    intervals <- c(0.5, 0.5, 0.2, 0.5, 0.2, 0.5)
    interval.df <- data.frame(pft_list, intervals)

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
        # lm_r2 <- round(sum_lm$r.squared, 2)
        lm_adjr2 <- round(sum_lm$adj.r.squared, 2)

        # Set axes label intervals
        pft_interval <- interval.df$intervals[interval.df$pft_list==pft]  # Set point colour.

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
          scale_x_continuous(breaks = seq(0,max(pft_dat$HAG_plotmean_of_cellmax_m*1.05), by = pft_interval)) +  # Prescribe break intervals on x axis
          theme_plots() +
          annotate(geom='text', x = (0.04*max(x)), y = (1*(max(x)* yx_ratio)), size =2.9, label = paste('y =', lm_slope, 'x'), hjust = 0, parse=FALSE) +
          annotate(geom='text', x = (0.04*max(x)), y = (0.88*(max(x)* yx_ratio)), size =2.9, label = paste('Adj.R^2 == ', round(lm_adjr2, 2)), hjust = 0, parse = TRUE) +
          annotate(geom='text', x = (0.04*max(x)), y = (0.76*(max(x)* yx_ratio)), size =2.9, label = paste('Species ==', n_species), hjust=0, parse = TRUE) +
          annotate(geom='text', x = (0.04*max(x)), y = (0.65*(max(x)* yx_ratio)), size =2.9, label = paste('n ==', n_obs), hjust=0, parse = TRUE)
          )

        PFT_plotlist[[pft]] <- pft_plot  # add each plot into plot list

        # Save plots
        ggsave(paste0("outputs/4_PFT_level_plots/", pft, ".png", sep = ''),
               width = 7, height = 7, units = 'cm',
               plot = pft_plot)


      }

    # Combine plots with patchwork
    (PFT_Figure <- PFT_plotlist$Fern + PFT_plotlist$Forb + PFT_plotlist$Graminoid +
        PFT_plotlist$Shrub + PFT_plotlist$Tree + PFT_plotlist$Succulent)

    # Export figure
    png("outputs/Figure_2/Figure 2 (from R).png",
        width = 16, height = 11, units = 'cm', res = 500)         # Save plot
    plot(PFT_Figure)
    dev.off()
    }




### *************************************************
### *************************************************
## Exploratory post-hoc investigation of the influence of (i) wind speed and
## (ii) solar elevation on the relationship between mean canopy height and
## aboveground biomass, to better to understand how sensitive these
## photogrammetric approaches are to these envrionmental parameters.
### 6.1 Subset data and reanalyse for wind and influence testing ----
# Subset dataframe for analysis of wind influence
df_for_influence <- df %>%
  filter(PeakBiomass == TRUE) %>%                                               # Subset observations from peak biomass.
  filter(!is.na(AGB_g_m2) & !is.na(HAG_plotmean_of_cellmax_m)) %>%              # Filter for observations with both HAG and AGB values.
  filter(!plant_functional_type %in% c("Bryophyte")) %>%                        # Remove invalid Bryophyte, and Succulent observations so that models will converge.
  filter(!binomial_species %in% c("Mixed grasses", "Unknown spp."))             # Remove unwanted 'species'.


### Extract model paraemters for different species
{
  # Create empty dataframe to be populated for each sample
  df_influence_summary <- data.frame(plant_functional_type = character(),
                                     order = character(),
                                     family = character(),
                                     species = character(),
                                     species_short = character(),
                                     observations = integer(),
                                     lm_slope = double(),
                                     lm_slope_error = double(),
                                     lm_CI83_low = double(),
                                     lm_CI83_high = double(),
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
  survey_l <- sort(unique(df_for_influence$survey_code))

  # Derive model parameters for species
  for(j in survey_l){
    survey_dat <- filter(df_for_influence, survey_code == j)                   # Subset data by survey code.
    species_l <- sort(unique(survey_dat$binomial_species))                 # List species present in survey.
    for(s in species_l){
      species_dat <- filter(survey_dat, binomial_species == s)             # Subset data by survey code.

      if(nrow(species_dat) < min_obs){next}                                 # skip species if there are insufficient observations.

      x <- species_dat$HAG_plotmean_of_cellmax_m
      y <- species_dat$AGB_g_m2

      # Fit models
      model_lm <- lm(y ~ x + 0, na.action=na.exclude)
      sum_lm <- summary(model_lm)

      # Calculate 83% Confidence intervals for visual interpretation (https://www.nature.com/articles/nmeth.2659)
      CI83_low <- round(confint(model_lm, level=0.83)[1],1)
      CI83_high <- round(confint(model_lm, level=0.83)[2],1)

      # Extract parameters as new list
      sample_info <- c(plant_functional_type = as.character(unique(species_dat$plant_functional_type)),
                       order = as.character(unique(species_dat$plot_order)),
                       family = as.character(unique(species_dat$plot_family)),
                       species = as.character(unique(species_dat$binomial_species)),
                       species_short = as.character(unique(species_dat$species_short)),
                       observations = min(length(species_dat$AGB_g_m2),length(species_dat$HAG_plotmean_of_cellmax_m)),
                       lm_slope = trunc(sum_lm$coefficients[1]),
                       lm_slope_error = trunc(sum_lm$coefficients[2]),
                       lm_CI83_low = CI83_low,
                       lm_CI83_high = CI83_high,
                       lm_r2 = round(sum_lm$r.squared, 2),
                       lm_p = round(sum_lm$coefficients[4],4),
                       survey_code = (unique(species_dat$survey_code)),
                       site_code = (unique(species_dat$site_code)),
                       wind_speed = as.character(unique(species_dat$wind_speed)),
                       solar_elevation = as.character(unique(species_dat$solar_elevation)),
                       sky_conditions_code = as.character(unique(species_dat$sky_conditions_code))
      )

      # # Convert list into dataframe.
      sample_info <- data.frame(lapply(sample_info, type.convert), stringsAsFactors=FALSE)

      # Append new row to summary table.
      df_influence_summary <- rbind(df_influence_summary, sample_info)

      # Order species_summary_table alphabetically by plant functional group, order, family and binomial species. NB. tolower deals with case issues.
      df_influence_summary <- df_influence_summary[order(tolower(df_influence_summary$plant_functional_type),
                                                   tolower(df_influence_summary$order),
                                                   tolower(df_influence_summary$family),
                                                   tolower(df_influence_summary$species)
      ),]
    }
  }

  # Tidy up environment
  rm(survey_l,
     species_l,
     survey_dat,
     sample_info)

  # Subset samples for wind influence analsys
  df_influence_summary_wind <- df_influence_summary

  # Subset wind species sampled multiple times
  df_influence_summary_wind_multiple <- df_influence_summary %>%
    group_by(species) %>%
    add_count(n_distinct(survey_code)) %>%
    filter(n>1)

  # Subset samples for sun influence analsys
  df_influence_summary_sun <- df_influence_summary %>%
      filter(sky_conditions_code <= 4)  # Filter observations by sky conditions.

  # Subset wind species sampled multiple times
  df_influence_summary_sun_multiple <- df_influence_summary %>%
    filter(sky_conditions_code <= 4) %>%   # Filter observations by sky conditions.
    group_by(species) %>%
    add_count(n_distinct(survey_code)) %>%
    filter(n>1)


  # # Compute the number of species sampled more then once
  n_of_species_in_df_influence_wind <- length(unique(df_influence_summary_wind_multiple$species))
  print(n_of_species_in_df_influence_wind)
}


### Calculate estimate of the average affect of wind speed on allometric functions ###
## Also used to order the faccet plot below!
{
  # Create empty dataframe to be populated
  df_influence_wind_slope <- data.frame(species = character(),
                                        species_short = character(),
                                        slope = double(),
                                        stringsAsFactors=FALSE
  )

  # List unique species
  species_l <- sort(unique(df_influence_summary_wind_multiple$species))

  # Derive model parameters for each species
  for(s in species_l){
    species_dat <- filter(df_influence_summary_wind_multiple, species == s)

    x <- species_dat$wind_speed
    y <- species_dat$lm_slope

    # Fit models
    model_lm <- lm(y ~ x, na.action=na.exclude)
    sum_lm <- summary(model_lm)

    # Extract parameters as new list
    species_info <- c(species = as.character(unique(species_dat$species)),
                      species_short = as.character(unique(species_dat$species_short)),
                      slope = trunc(sum_lm$coefficients[2])
    )

    # Convert list into dataframe.
    species_info <- data.frame(lapply(species_info, type.convert), stringsAsFactors=FALSE)

    # Append new row to summary table.
    df_influence_wind_slope <- rbind(df_influence_wind_slope, species_info)

    # order by slope
    df_influence_wind_slope <- df_influence_wind_slope[order(-df_influence_wind_slope$slope),]
  }
}

median(df_influence_wind_slope$slope)
mean(df_influence_wind_slope$slope)

### Calculate estimate of the average affect of sun elevation on allometric functions ###
## Also used to order the faccet plot below!
{
  # Create empty dataframe to be populated
  df_influence_sun_slope <- data.frame(species = character(),
                                       species_short = character(),
                                       slope = double(),
                                       stringsAsFactors=FALSE
                                       )

  # List unique species
  species_l <- sort(unique(df_influence_summary_sun_multiple$species))

  # Derive model parameters for each species
  for(s in species_l){
    species_dat <- filter(df_influence_summary_sun_multiple, species == s)

    x <- species_dat$wind_speed
    y <- species_dat$lm_slope

    # Fit models
    model_lm <- lm(y ~ x, na.action=na.exclude)
    sum_lm <- summary(model_lm)

    # Extract parameters as new list
    species_info <- c(species = as.character(unique(species_dat$species)),
                      species_short = as.character(unique(species_dat$species_short)),
                      slope = trunc(sum_lm$coefficients[2])
    )

    # Convert list into dataframe.
    species_info <- data.frame(lapply(species_info, type.convert), stringsAsFactors=FALSE)

    # Append new row to summary table.
    df_influence_sun_slope <- rbind(df_influence_sun_slope, species_info)

    # order by slope
    df_influence_sun_slope <- df_influence_sun_slope[order(-df_influence_sun_slope$slope),]
  }
}

median(df_influence_sun_slope$slope)
mean(df_influence_sun_slope$slope)




### 6.2 Visualising wind and sun influence ----
## Review distribution of envrionmental parameters
# Create histrogram of wind speeds
{
  (wind_histogram <- ggplot(df_for_influence, aes(x=wind_speed)) +
     geom_histogram(binwidth=1) +
     labs(x = expression("Wind Speed (m s"^"-2"*")"),
          y = expression("Count of Harvest Plots"),
          title = expression("Histogram of wind speeds")) +
     theme_plots())

  png("outputs/5_interaction_effects/Histogram of wind speeds by plot.png",
      width = 10, height = 10, units = 'cm', res = 500)     # Save plot
  plot(wind_histogram)
  dev.off()
}

{
  (wind_histogram <- ggplot(df_survey, aes(x=wind_speed)) +
     geom_histogram(binwidth=1) +
     labs(x = expression("Wind Speed (m s"^"-2"*")"),
          y = expression("Count of Surveys"),
          title = expression("Histogram of wind speeds")) +
     theme_plots())

  png("outputs/5_interaction_effects/Histogram of wind speeds by survey.png",
      width = 10, height = 10, units = 'cm', res = 500)     # Save plot
  plot(wind_histogram)
  dev.off()
}


## Create histrogram of solar elevation
(solar_elevation_histogram <- ggplot(df_for_influence, aes(x=solar_elevation)) +
    geom_histogram(binwidth=5) +
    labs(x = expression("Solar Elevation (°)"),
         y = expression("Count of Harvest Plots"),
         title = expression("Histogram of solar elevations")) +
    theme_plots())

png("outputs/5_interaction_effects/Histogram of solar elevations.png",
    width = 10, height = 10, units = 'cm', res = 500)                     # Save plot
plot(solar_elevation_histogram)
dev.off()

# plot solar elevation against sky conditions
(solar_elevation_vs_sky <- ggplot(df_for_influence, aes(x=solar_elevation, y=sky_conditions_code)) +
    geom_point() +
    labs(x = expression("Solar Elevation (°)"),
         y = expression("Sky code (where 0 = clear)")) +
    theme_plots())

png("outputs/5_interaction_effects/Sun elevation vs sky conditions.png",
    width = 10, height = 10, units = 'cm', res = 500)     # Save plot
plot(solar_elevation_vs_sky)
dev.off()



# Set scaling parameters
max_wind <- 10
max_slope <- 15000
max_sun <-  round(max(df_influence_summary_wind_multiple$solar_elevation),-1)
min_sun <-  round(min(df_influence_summary_wind_multiple$solar_elevation),-1)
annotation_size <- 1.5




### Visualise wind influence per-species
{
  # Ceate wind plot with species annotations
  (wind.plot <- ggplot(data = df_influence_summary_wind_multiple,
                       aes(x = wind_speed,
                           y = lm_slope,
                           label = species,
                           colour = plant_functional_type,
                           shape = plant_functional_type)) +
     geom_point(alpha = 0.7, na.rm = TRUE) +
     geom_errorbar(aes(ymin=lm_CI83_low, ymax=lm_CI83_high), width=0.05) +
     scale_colour_manual(values = c(col_shrub, col_graminoid, col_succulent)) +
     scale_shape_manual(values = pft_shapes) +
     labs(x = expression("Wind Speed (m s"^"-1"*")"),
          y = expression("Allometric Model Slope (g m"^"-3"*")")) +
     theme_plots() +
     theme(legend.title = element_blank()) +
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
     theme(legend.position = c(0.25, 0.95))
  )

  ggsave(wind.plot,
         filename = "outputs/5_interaction_effects/wind interaction with slopes.png",
         width = 8,
         height = 8,
         units = "cm")



  ### Create multi-panel wind interaction plot
  # Used for supplementary figure
  # Specify the levels of the factors (species) to control plot order.
  ordered_factors <- df_influence_wind_slope$species_short

  df_influence_summary_wind_multiple$species_short <- factor(df_influence_summary_wind_multiple$species_short,
                                                 levels = ordered_factors)

  (wind.plot3 <- ggplot(data = df_influence_summary_wind_multiple,
                        aes(x = wind_speed,
                            y = lm_slope,
                            colour = plant_functional_type,
                            shape = plant_functional_type)) +
      facet_wrap(~ species_short) +
      geom_point(alpha = 0.7, na.rm = TRUE) +
      geom_errorbar(aes(ymin=lm_CI83_low, ymax=lm_CI83_high), width=0.05) +
      scale_colour_manual(values = c(col_shrub, col_graminoid, col_succulent)) +
      scale_shape_manual(values = pft_shapes) +
      labs(x = expression("Wind Speed (m s"^"-1"*")"),
           y = expression("Allometric Model Slope (g m"^"-3"*")")) +
      theme_plots() +
      theme(legend.position = "none") +
      theme(strip.text = element_text(face = "italic")) +
      coord_cartesian(ylim = c(0, 30000), xlim = c(0, 8), expand=FALSE) +
      geom_smooth(method=lm,
                  formula= y ~ x,
                  aes(group=species),
                  se=FALSE, size=1.0, na.rm = TRUE, alpha=0.3)
  )

  ggsave(wind.plot3,
         filename = "outputs/5_interaction_effects/wind interaction by species.png",
         width = 16,
         height = 19,
         units = "cm")
  ggsave(wind.plot3,
         filename = "outputs/Figure_S3_wind/wind interaction by species.png",
         width = 16,
         height = 19,
         units = "cm")
}


### Create multi-panel sun interaction plot
# Used for supplementary figure
# Specify the levels of the factors (species) to control plot order.
ordered_factors <- df_influence_sun_slope$species_short

df_influence_summary_sun_multiple$species_short <- factor(df_influence_summary_sun_multiple$species_short,
                                                      levels = ordered_factors)

(sun.plot3 <- ggplot(data = df_influence_summary_sun_multiple,
                      aes(x = solar_elevation,
                          y = lm_slope,
                          colour = plant_functional_type,
                          shape = plant_functional_type)) +
    facet_wrap(~ species_short) +
    geom_point(alpha = 0.7, na.rm = TRUE) +
    geom_errorbar(aes(ymin=lm_CI83_low, ymax=lm_CI83_high), width=0.05) +
    scale_colour_manual(values = c(col_shrub, col_graminoid)) +
    scale_shape_manual(values = pft_shapes) +
    labs(x = expression("Sun Elevation (°)"),
         y = expression("Allometric Model Slope (g m"^"-3"*")")) +
    theme_plots() +
    theme(legend.position = "none") +
    theme(strip.text = element_text(face = "italic")) +
    coord_cartesian(ylim = c(0, 15000), xlim = c(25, 75), expand=FALSE) +
    geom_smooth(method=lm,
                formula= y ~ x,
                aes(group=species),
                se=FALSE, size=1.0, na.rm = TRUE, alpha=0.3)
)

ggsave(sun.plot3,
       filename = "outputs/5_interaction_effects/sun interaction by species.png",
       width = 16,
       height = 19,
       units = "cm")
ggsave(sun.plot3,
       filename = "outputs/Figure_S4_sun/sun interaction by species.png",
       width = 16,
       height = 19,
       units = "cm")




### Ceate wind influence plot at PFT-level
{
  (wind.plot.pft <- ggplot(data = df_influence_summary_wind,
                           aes(x = wind_speed,
                               y = lm_slope,
                               colour = plant_functional_type,
                               shape = plant_functional_type)) +
     geom_point(alpha = 0.4, na.rm = TRUE) +
     geom_errorbar(aes(ymin=lm_CI83_low, ymax=lm_CI83_high), width=0.05, alpha = 0.4) +
     scale_colour_manual(values = c(col_shrub, col_graminoid, col_forb, col_fern, col_succulent, col_tree)) +
     scale_shape_manual(values = pft_shapes) +
     labs(x = expression("Wind Speed (m s"^"-1"*")"),
          y = expression("Density (g m"^"-3"*")")) +
     theme_plots() +
     theme(legend.title = element_blank()) +
     coord_cartesian(ylim = c(0, 15000), xlim = c(0, 8), expand=FALSE) +
     geom_smooth(method=lm,
                 formula= y ~ x,
                 aes(group=plant_functional_type),
                 se=FALSE, size=1.3, na.rm = TRUE, alpha=0.3) +
     theme(legend.position = c(0.16, 0.86))
  )

  ggsave(wind.plot.pft,
         filename = "outputs/5_interaction_effects/Wind interaction by PFT.png",
         width = 12,
         height = 12,
         units = "cm")

}



### Ceate sun influence plot at PFT-level
{
  (sun.plot.pft <- ggplot(data = df_influence_summary_sun,
                           aes(x = solar_elevation,
                               y = lm_slope,
                               colour = plant_functional_type,
                               shape = plant_functional_type)) +
     geom_point(alpha = 0.4, na.rm = TRUE) +
     geom_errorbar(aes(ymin=lm_CI83_low, ymax=lm_CI83_high), width=0.05, alpha = 0.4) +
     scale_colour_manual(values = c(col_shrub, col_graminoid, col_forb, col_succulent, col_tree)) +
     scale_shape_manual(values = pft_shapes) +
     labs(x = expression("Sun Elevation (°)"),
          y = expression("Density (g m"^"-3"*")")) +
     theme_plots() +
     theme(legend.title = element_blank()) +
     coord_cartesian(ylim = c(0, 15000), xlim = c(20, 90), expand=FALSE) +
     geom_smooth(method=lm,
                 formula= y ~ x,
                 aes(group=plant_functional_type),
                 se=FALSE, size=1.3, na.rm = TRUE, alpha=0.3) +
     theme(legend.position = c(0.85, 0.88))
  )

  ggsave(sun.plot.pft,
         filename = "outputs/5_interaction_effects/Sun interaction by PFT.png",
         width = 12,
         height = 12,
         units = "cm")

}





# Combine plots with patchwork
(
  Figure_S2 <-
    wind.plot.pft + sun.plot.pft +
    plot_annotation(tag_levels = 'A')
)

# Export figure
ggsave(
  Figure_S2,
  filename = "outputs/Figure_S2/Figure S2 - Wind and sun interaction by PFT.png",
  width = 22,
  height = 11,
  units = "cm"
)





### 6.3 Modelling influence of wind and sun ----

### Subset data
## Wind speed
df_for_wind <- df_for_influence %>%
  filter(!plant_functional_type %in% c("Succulent"))  # Remove Succulent observations so that GLMM will converge.

## Cloud cover
df_for_cloud <- df_for_influence %>%
  mutate(cloud = if_else(sky_conditions_code >= 6, "Cloudy", "Clear"))

## sun elevation
df_for_sun <- df_for_influence %>%
  filter(sky_conditions_code <= 5)  # Filter by sky conditions



# List  codes for survey that were cloudy
Cloudy_surveys <- df_for_cloud %>%
  filter(cloud == "Cloudy") %>%
  distinct(survey_code)

Cloudy_surveys


## Reviewing biomass distribution and heteroscedasticity:
ggdensity(df_for_wind$AGB_g_m2, fill = "lightgray")  # Density plot
ggqqplot(df_for_wind$AGB_g_m2)  # QQ plot
shapiro.test(df_for_wind$AGB_g_m2)  #Shapiro_Wilk W Test of normality

## Testing assumptions of normally distribution and heteroscedasticity:
ggdensity(df_for_sun$AGB_g_m2, fill = "lightgray")  # Density plot
ggqqplot(df_for_sun$AGB_g_m2)  # QQ plot
shapiro.test(df_for_sun$AGB_g_m2)  #Shapiro_Wilk W Test of normality

## These plots and Shapiro test indicate non-normal (skewed) distribution for
## biomass in both wind and sun datasets, which requires either:
## (i) transformation of the data, or
## (ii) nonparametric stats, or
## (iii) Generalised Linear Model (GLM)



### Modelling the influence of wind speed
## Generalised Linear Mixed Model (GLMM) Using an identity link function because
## this has lower multicollinearity, more normal residuals and better
## homoscedasticity compared with a log link function.
wind_model <-lme4::glmer(AGB_g_m2 ~  HAG_plotmean_of_cellmax_m * wind_speed + (1|plant_functional_type),
                        data = df_for_wind,
                        family = Gamma(link = "identity"))

performance::check_model(wind_model)  # Evaluate model performence
summary(wind_model)  # See model summary
# MuMIn::r.squaredGLMM(wind_model)  # Doesn't work with Gamma(“identity”)
Anova(wind_model)  # Analysis of Deviance  (Type II Wald chisquare tests)

# Calculate the variance explained by the PFT random effect
random_effect_variance <- as.data.frame(VarCorr(wind_model),comp="Variance")[1, 'vcov']/(as.data.frame(VarCorr(wind_model),comp="Variance")[1, 'vcov']+as.data.frame(VarCorr(wind_model),comp="Variance")[2, 'vcov'])
random_effect_variance <- round(random_effect_variance,2)*100
print(paste("the PFT random effect explains ", random_effect_variance, " % of the variance in the model"))

# Export diagnsotic plots
png("outputs/5_interaction_effects/wind model diagnostics.png")
performance::check_model(wind_model)
dev.off()



### Modelling the influence of cloud cover
cloud_model <- lmerTest::lmer(AGB_g_m2 ~  HAG_plotmean_of_cellmax_m  * cloud + (1|plant_functional_type),
                              data = df_for_cloud)

performance::check_model(cloud_model)  # Evaluate model performence

summary(cloud_model)

MuMIn::r.squaredGLMM(cloud_model)  # Doesn't work with Gamma(“identity”)
Anova(cloud_model)  # Analysis of Deviance  (Type II Wald chisquare tests)

# Calculate the variance explained by the PFT random effect
random_effect_variance <- as.data.frame(VarCorr(cloud_model),comp="Variance")[1, 'vcov']/(as.data.frame(VarCorr(cloud_model),comp="Variance")[1, 'vcov']+as.data.frame(VarCorr(cloud_model),comp="Variance")[2, 'vcov'])
random_effect_variance <- round(random_effect_variance,2)*100
print(paste("the PFT random effect explains ", random_effect_variance, " % of the variance in the model"))

# Export diagnsotic plots
png("outputs/5_interaction_effects/cloud model diagnostics.png")
performance::check_model(cloud_model)  # Evaluate model performence
dev.off()



### Modelling the influence of sun elevation
sun_model <- lmerTest::lmer(AGB_g_m2 ~  HAG_plotmean_of_cellmax_m * solar_elevation + (1|plant_functional_type),
                            data = df_for_sun)

performance::check_model(sun_model)  # Evaluate model performence
performance::check_collinearity(sun_model)

summary(sun_model)

MuMIn::r.squaredGLMM(sun_model)  # Doesn't work with Gamma(“identity”)
Anova(sun_model)  # Analysis of Deviance  (Type II Wald chisquare tests)

# Calculate the variance explained by the PFT random effect
random_effect_variance <- as.data.frame(VarCorr(sun_model),comp="Variance")[1, 'vcov']/(as.data.frame(VarCorr(sun_model),comp="Variance")[1, 'vcov']+as.data.frame(VarCorr(sun_model),comp="Variance")[2, 'vcov'])
random_effect_variance <- round(random_effect_variance,2)*100
print(paste("the PFT random effect explains ", random_effect_variance, " % of the variance in the model"))

# Export diagnsotic plots
png("outputs/5_interaction_effects/sun model diagnostics.png")
performance::check_model(sun_model)  # Evaluate model performence
dev.off()



#### Visualise interaction models ----
{
  wind_levels <- "wind_speed[1, 3, 5]"  # set levels
  predictions_wind <- ggeffects::ggpredict(wind_model, terms = c("HAG_plotmean_of_cellmax_m", wind_levels))

  (interaction_plot_wind <- ggplot() +
     geom_line(
       data = predictions_wind,
       aes(x = x, y = predicted, colour = group),
       size = 1) +
     geom_ribbon(data = predictions_wind,
                 aes(x = x,
                     ymin = conf.low,
                     ymax = conf.high,
                     fill = group),
                 alpha = 0.2) +
     theme_plots() +
     theme(legend.title = element_text(size = 8),
           legend.text = element_text(size = 6, face = "italic"),
           legend.key.size = unit(0.9, "line"),
           legend.background = element_rect(color = "black",
                                            fill = "transparent", size = 4,
                                            linetype = "blank"),
           legend.position = c(0.25, 0.9)) +
     labs(x = "mean canopy height (m)",
          y = expression("Aboveground biomass (g m" ^ "-2" * ")"),
          fill = "Wind speed", colour = "Wind speed") +
     coord_cartesian(ylim = c(0, 10000),
                     xlim = c(0, 2),
                     expand=FALSE) +
     scale_colour_viridis_d(expression("Wind speed (m s" ^ "-2" * ")"), option = "magma", direction = 1, end = 0.8) +
     scale_fill_viridis_d(expression("Wind speed (m s" ^ "-2" * ")"), option = "magma", direction = 1, end = 0.8)
  )

  ggsave(interaction_plot_wind,
         filename = "outputs/5_interaction_effects/wind_interaction.png",
         width = 12,
         height = 10,
         units = "cm")
}

# Visualise cloud interaction model ----
{
  cloud_levels <- "cloud[Clear, Cloudy]"  # set levels
  predictions_cloud <- ggeffects::ggpredict(cloud_model, terms = c("HAG_plotmean_of_cellmax_m", cloud_levels))

  (interaction_plot_cloud <- ggplot() +
      geom_line(
        data = predictions_cloud,
        aes(x = x, y = predicted, colour = group),
        size = 1) +
      geom_ribbon(data = predictions_cloud,
                  aes(x = x,
                      ymin = conf.low,
                      ymax = conf.high,
                      fill = group),
                  alpha = 0.2) +
      theme_plots() +
      theme(legend.title = element_text(size = 8),
            legend.text = element_text(size = 6, face = "italic"),
            legend.key.size = unit(0.9, "line"),
            legend.background = element_rect(color = "black",
                                             fill = "transparent", size = 4,
                                             linetype = "blank"),
            legend.position = c(0.25, 0.9)) +
      labs(x = "mean canopy height (m)",
           y = expression("Aboveground biomass (g m" ^ "-2" * ")"),
           fill = "Cloud Cover", colour = "Cloud Cover") +
      coord_cartesian(ylim = c(0, 10000),
                      xlim = c(0, 2),
                      expand=FALSE) +
      scale_colour_viridis_d(expression("Cloud Cover"), option = "magma", direction = 1, end = 0.8) +
      scale_fill_viridis_d(expression("Cloud Cover"), option = "magma", direction = 1, end = 0.8)
  )


  ggsave(interaction_plot_cloud,
         filename = "outputs/5_interaction_effects/cloud interaction.png",
         width = 12,
         height = 10,
         units = "cm")
  ggsave(interaction_plot_cloud,
         filename = "outputs/Figure_S4_cloud/Figure S4 - cloud interaction.png",
         width = 12,
         height = 10,
         units = "cm")
}

# Visualise sun interaction model ----
{
  sun_levels <- "solar_elevation[25, 50, 75]"  # set levels
  predictions_sun <- ggeffects::ggpredict(sun_model, terms = c("HAG_plotmean_of_cellmax_m", sun_levels))

  (interaction_plot_sun <- ggplot() +
     geom_line(
       data = predictions_sun,
       aes(x = x, y = predicted, colour = group),
       size = 1) +
     geom_ribbon(data = predictions_sun,
                 aes(x = x,
                     ymin = conf.low,
                     ymax = conf.high,
                     fill = group),
                 alpha = 0.2) +
     theme_plots() +
     theme(legend.title = element_text(size = 8),
           legend.text = element_text(size = 6, face = "italic"),
           legend.key.size = unit(0.9, "line"),
           legend.background = element_rect(color = "black",
                                            fill = "transparent", size = 4,
                                            linetype = "blank"),
           legend.position = c(0.25, 0.9)) +
     labs(x = "mean canopy height (m)",
          y = expression("Aboveground biomass (g m" ^ "-2" * ")"),
          fill = "Sun Elevation", colour = "Sun Elevation") +
     coord_cartesian(ylim = c(0, 10000),
                     xlim = c(0, 2),
                     expand=FALSE) +
     scale_colour_viridis_d(expression("Sun elevation (°)"), option = "magma", direction = 1, end = 0.8) +
     scale_fill_viridis_d(expression("Sun elevation (°)"), option = "magma", direction = 1, end = 0.8)
  )

  ggsave(interaction_plot_sun,
         filename = "outputs/5_interaction_effects/sun_interaction.png",
         width = 12,
         height = 10,
         units = "cm")
}

# combine interaction plots with patchwork
(interaction_plots <- interaction_plot_wind + interaction_plot_sun +
    plot_annotation(tag_levels = 'A') &
    theme(plot.tag.position = c(0.0, 0.97))  # Control horizontal and vertical position of panel labels
    )

ggsave(interaction_plots,
       filename = "outputs/Figure_3_Interactions/Figure 3 - Interactions.png",
       width = 18,
       height = 9,
       units = "cm")











### Assessing reconstruction 'success'
## Subjectively assigned quality scores
# Histogram of subjectivly assigned scores
hist(df_peak$point_cloud_quality)

# Count the number of plots in each quality class
# NB. this doesn't include the 16 discarded grass plots sthat would be in class 3!
plot_quality <- df_peak %>%
  group_by(point_cloud_quality) %>%
  summarise(n = n())

# Calculate proportion of plots in each quality class
# NB. this doesn't include the 16 discarded grass plots sthat would be in class 3!
plot_quality$proportion = plot_quality$n/741

plot_quality


## Subjectively assigned elevation bias values
(plot_of_elevation_bias <- ggplot(df_peak, aes(x = corner_apparent_bias)) +
    geom_density(fill="lightgrey") +
    labs(x = "\nVisually-estimated bias in corner elevations (cm)\n (GNSS elevation minus point cloud elevation)",
         y = "Density\n") +
    theme_plots() +
    geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=0.5)
)

# save plot
ggsave(
  plot_of_elevation_bias,
  filename = "outputs/1_dataset_level/Elevation bias in plot corners.png",
  width = 12,
  height = 12,
  units = "cm"
)

DTM_bias <- df_peak %>%
  group_by(corner_apparent_bias) %>%
  summarise(n = n())




### *************************************************
### 7. Output text report from analysis ----
### *************************************************

  ### Prepare output metircs
    today <- as.character(Sys.Date())
    species_R2_mean <- round(mean(species_summary3$lm_r2),2)
    species_R2_min <- round(min(species_summary3$lm_r2),2)
    species_R2_max <- round(max(species_summary3$lm_r2),2)

    species_adjR2_mean <- round(mean(species_summary3$lm_adjr2),2)
    species_adjR2_min <- round(min(species_summary3$lm_adjr2),2)
    species_adjR2_max <- round(max(species_summary3$lm_adjr2),2)

    species_loocv_mean <- round(mean(species_summary3$lm_loocv_pc),1)
    species_aloocv_min <- round(min(species_summary3$lm_loocv_pc),1)
    species_loocv_max <- round(max(species_summary3$lm_loocv_pc),1)

    survey_wind_mean <- round(mean(df_survey$wind_speed),2)
    Survey_wind_median <- median(df_survey$wind_speed)

    plot_wind_mean <- round(mean(df_peak_ex_bryo$wind_speed),2)
    plot_wind_median <- median(df_peak_ex_bryo$wind_speed)

    n_significant_KCC <- sum(species_summary3$Kendall_cc_p <0.05)
    Kendall_cc_tau_sig_mean <- round(mean(species_summary3$Kendall_cc_tau[species_summary3$Kendall_cc_p<0.05]),3)
    Kendall_cc_tau_sig_max <- round(max(species_summary3$Kendall_cc_tau[species_summary3$Kendall_cc_p<0.05]),3)
    Kendall_cc_tau_sig_min <- round(min(species_summary3$Kendall_cc_tau[species_summary3$Kendall_cc_p<0.05]),3)


  ### Save report
    cat("Drone Allometry Experiment: Summary Report \n",
        "Andrew Cunliffe <andrewmcunliffe@gmail.com> \n",
        "Generated on: ", today, "\n",
        "---------------------------------------------------------------------------------", "\n", "\n",
        "For species-level models, R2 ranges from ", species_R2_min, " to ", species_R2_max, ", with a mean of: ",
         species_R2_mean, "\n",
        "For species-level models, Adjusted R2 ranges from ", species_adjR2_min, " to ", species_adjR2_max, ", with a mean of: ",
        species_adjR2_mean, "\n",
        "For species-level models, relative LOOCV ranges from ", species_aloocv_min, " to ", species_loocv_max, ", with a mean of: ",
        species_loocv_mean, "\n",
        n_significant_KCC, " species have statistically significant Kendall's correlation coefficients.",
        "The mean value of the statistically significant tau coefficients was ", Kendall_cc_tau_sig_mean, " and ranged from ",
        Kendall_cc_tau_sig_min, " to ", Kendall_cc_tau_sig_max, "\n", "\n",
        "At the survey-level, the mean wind speed was ", survey_wind_mean, " m s-1 (median: ", Survey_wind_median, " m s-1)", "\n",
        "At the plot-level, the mean wind speed was ", plot_wind_mean, " m s-1 (median: ", plot_wind_median, " m s-1)", "\n", "\n",
        "---------------------------------------------------------------------------------", "\n",
        "End of Report", "\n",
        file = "outputs/Report 4 - survey analysis.txt")


