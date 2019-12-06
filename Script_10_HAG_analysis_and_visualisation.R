# Script for statistical analysis and visualisation of observations for the
# drone photogrammetry-allometry project.
# Written by Andrew Cunliffe (andrewmcunliffe@gmial.com) with input from Isla Myers-Smith.

# Establish operating environment ----
home <- "C:/workspace/Geospatial_Pipeline/Geospatial_Pipeline_Python/"

### troubleshooting ###
# options(repos = c(CRAN = "http://cran.rstudio.com"))  # set default CRAN mirror

#### Load required packages ####
library(devtools)

# Download
# if(!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
# if(!require(viridis)) {install.packages("viridis"); require(viridis)}
# if(!require(grid)) {install.packages("grid"); require(grid)}
# if(!require(gridExtra)) {install.packages("gridExtra"); require(gridExtra)}
# if(!require(ggpubr)) {install.packages("ggpubr"); require(ggpubr)}
# if(!require(nortest)) {install.packages("nortest"); require(nortest)}
# if(!require(MASS)) {install.packages("MASS"); require(MASS)}
# if(!require(jtools)) {install.packages("jtools"); require(jtools)}
# if(!require(robustbase)) {install.packages("robustbase"); require(robustbase)}
# if(!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}
# if(!require(ggeffects)) {install.packages("ggeffects"); require(ggeffects)}
# if(!require(sjPlot)) {install.packages("sjPlot"); require(sjPlot)}
install.packages("broom")

# Install packages
library(tidyverse)                                                              # For making life better.
library(readxl)                                                                 # For reading and writing .xlsx files.
library(viridis)                                                                # For friendly colour palettes.
library(grid)                                                                   # required for plot annotation
library(gridExtra)                                                              # for arranging multi-panel plots.
library(ggpubr)                                                                 # for arranging multi-panel plots.
library(nortest)
# library(MASS)                                                                 # Used for robust regression with rlm().
# library(jtools)                                                               # Used for pretty model summaries (but not curerently used).
library(robustbase)                                                             # Used for robust regression with rlm().
library(plotbiomes)                                                             # Adding Whittaker biome to climate space diagram.
library(lme4)                                                                   # For linear mixed effects models.
# library(lmerTest)                                                               # For extracting p values from linear mixed effects models (but can sometimes conflict with other packages, so use carfeully and be aware of errors!).
library(ggeffects)                                                              # For plotting mixed effects models.
library(sjPlot)                                                                 # For plotting mixed effects models.
library(xlsx)                                                                   # For creating Excel files of summary tables.
library(broom)                                                                  # For summarising model outputs.


#### Load data ----
dataset <- read_excel(paste0(home, "outputs/processed_master_database.xlsx"),   # Read in summary data.
                      na = "NA")
peak_dataset <- dataset[dataset$PeakBiomass == TRUE, ]                          # Observations from peak biomass.
sev_dataset <- dataset[dataset$SevilletaIAV == TRUE, ]                          # Observations from Sevilleta interannual variation.


#### Create/verify output directories ----
loc_dataset <- paste0(home, "outputs/0 - Dataset-level")
dir.create(loc_dataset, showWarnings = FALSE)

loc_project <- paste0(home, "outputs/1 - Project-level")
dir.create(loc_project, showWarnings = FALSE)

loc_species <- paste0(home, "outputs/2 - Species-level plots")
dir.create(loc_species, showWarnings = FALSE)

loc_pft <- paste0(home, "outputs/3 - PFT-level plots")
dir.create(loc_pft, showWarnings = FALSE)

loc_sevilleta <- paste0(home, "outputs/9 - Sevilleta")
dir.create(loc_sevilleta, showWarnings = FALSE)


#### Create plotting themes ----
    theme_coding <- function(){
        theme_bw()+
            theme(axis.text = element_text(size = 6),
                  axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
                  axis.title = element_text(size = 8),
                  panel.grid = element_blank(),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                  plot.title = element_text(size = 10, vjust = 1, hjust = 0.5),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 5, face = "italic"),
                  legend.key.size = unit(0.9,"line"),
                  legend.background = element_rect(color = "black", fill = "transparent", size = 4, linetype="blank"),
                  legend.position = c(0.9, 0.9))
    }


# parameters for scale_shape_manual
pft_shapes <- c(15, 16, 17, 18, 15, 16, 17, 18)

#### Compute scaling parameters from minimum and maximum values ####
mat_max <- signif((1.1*max(peak_dataset$MAT, na.rm = TRUE)),2)
mat_min <- signif((1.1*min(peak_dataset$MAT, na.rm = TRUE)),2)
map_max <- signif((1.1*max(peak_dataset$MAP, na.rm = TRUE)),2)
max_agb <- 1.35*max(peak_dataset$AGB_spatially_normalised_g_m2, na.rm = TRUE)
max_mean_hag <- 1.1*max(peak_dataset$HAG_plotmean_of_cellmax_m, na.rm = TRUE)
min_mean_hag <- 0  # min(peak_dataset$HAG_plotmean_of_cellmax_m, na.rm = TRUE)
min_median_hag <- max(peak_dataset$HAG_plotmedian_of_cellmax_m, na.rm = TRUE)
max_median_hag <- min(peak_dataset$HAG_plotmedian_of_cellmax_m, na.rm = TRUE)

#### 1. Plot of climate space on Whittaker biomes ####
# Whittaker polygons after R.H. Whittaker. 1975. Communities and Ecosystems. 2d ed. Macmillan New York
peak_dataset$MAP_cm <- peak_dataset$MAP/10                                      # Create new column with MAP in cm rather than mm.

# Create modified colour palette (with forests in grey)
# create table of biome data
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
# create colour palette
biome_colors <- c("grey60",
                  "grey70",
                  "grey85",
                  "#A5C790",
                  "#A09700",
                  "#DCBB50",
                  "#FCD57A",
                  "#D16E3F",
                  "#C1E1DD")
#add biome names to colour palette
names(biome_colors) <- biomes_tbl$biome

# Create plot with custom colour palette
Whittaker_plot <- ggplot(data = peak_dataset,
                   aes(x = MAT,
                       y = MAP_cm)) +
    theme_coding() +
    # coord_cartesian(xlim = c(mat_min, mat_max), ylim = c(0, (map_max/10)), expand=FALSE) +
    coord_cartesian(expand=FALSE) +
    geom_polygon(data = Whittaker_biomes,
                 aes(x    = temp_c,
                     y    = precp_cm,
                     fill = biome),
                 # adjust polygon borders
                 colour = "gray98",
                 size   = 0.5) +
    scale_fill_manual(name   = "Whittaker biomes",
                      breaks = names(biome_colors),
                      labels = names(biome_colors),
                      values = biome_colors) +
    geom_point() +
    theme(legend.position = c(0.296, 0.736),
          legend.text = element_text(size = 7.2, face = 'bold')) +
    labs(x = expression("Mean Annual Temperature " ( degree~C)),
         y = expression("Mean Annual Precipitation (cm yr"^"-1"*")"),
         title = expression("Sampled climate space"))


outfile <- file.path(home, "outputs",paste("Sampled_climate_space.png",sep="")) # Specify filepath
png(filename=outfile, width = 10, height = 10, units = 'cm', res = 500)         # Save plot
plot(Whittaker_plot)
dev.off()



#### 2. Dataset-level analysis ####
### Plot of mean versus median canopy height ###
# Create plot
mean_median_HAGs <- ggplot(data = dataset,
                           aes(x <- HAG_plotmean_of_cellmax_m,
                               y <- HAG_plotmedian_of_cellmax_m),
                           colour = binomial_species) +
        geom_point(shape=1, na.rm = TRUE) +
        scale_colour_viridis_d() +
        geom_abline(slope = 1, na.rm = TRUE, colour = "grey") +
        theme_coding() +
        coord_cartesian(ylim = c(0, (max_mean_hag*1.3)),
                        xlim = c(0, (max_mean_hag*1.3)),
                        expand=FALSE) +
        labs(x = expression("Mean canopy height (m)"),
             y = expression("Median canopy height (m)"))

outfile <- file.path(loc_dataset, "Mean Vs Median HAG.png")                     # Filename for output.
png(filename=outfile, width = 10, height = 10, units = 'cm', res = 400)         # Export plot.
plot(mean_median_HAGs)
dev.off()



#### Scatterplot of all observations in peak dataset, by PFT ###
# Manually specify the order of factor levels.
peak_dataset$plant_functional_type <- factor(peak_dataset$plant_functional_type,
                                             levels = c("Succulent",
                                                        "Tree",
                                                        "Shrub",
                                                        "Forb",
                                                        "Graminoid",
                                                        "Fern",
                                                        "Bryophyte"
                                                        ))
# Create scatter plot
HAG_Vs_AGB_by_PFT <- ggplot(data = peak_dataset,
                            aes(x = HAG_plotmean_of_cellmax_m,
                                y = AGB_spatially_normalised_g_m2,
                                colour = plant_functional_type,
                                shape = plant_functional_type)) +
    geom_point(alpha = 0.7, na.rm = TRUE) +
    scale_colour_viridis_d() +
    scale_shape_manual(values = pft_shapes) +
    labs(x = expression("Mean canopy height (m)"),
         y = expression("Dry biomass (g m"^"-2"*")"),
         title = expression("Canopy height predicts aboveground biomass"),
         colour = "Plant functional type",
         shape = "Plant functional type") +
    theme_coding() +
    theme(plot.title = element_text(face="italic"),
          legend.title = element_text(size=8),
          legend.position = c(0.85, 0.79)) +
    coord_cartesian(ylim = c(0, max_agb), xlim = c(0, 2.1), expand=FALSE) +
    geom_smooth(method="lmrob", formula= y ~ x-1,
                aes(group=plant_functional_type,
                    colour=plant_functional_type),
                se=FALSE, size=0.5, na.rm = TRUE)

# Export scatter plot
outfile <- file.path(home, "outputs/HAG Vs AGB by PFT.png",sep="")
png(filename=outfile, width = 14, height = 14, units = 'cm', res = 600)
plot(HAG_Vs_AGB_by_PFT)
dev.off()


# Isla's advice needed ####
# Mixed effects models - IN DEVELOPMENT ####


# 1st model: biomass as a function of height, with ProjectCode as a fixed effect.
model <- lmer(AGB_spatially_normalised_g_m2 ~ HAG_plotmean_of_cellmax_m + (1|ProjectCode), data = peak_dataset)
summary(model)


# 2nd model: include taxononic group.
model <- lmer(AGB_spatially_normalised_g_m2 ~ HAG_plotmean_of_cellmax_m * plant_functional_type + 0 + (1|ProjectCode), data = peak_dataset)
summary(model)


# Wind test
# Here we are interested in a post-hoc analysis to test whether wind speed is a
# signifcant interaction term in the peak_dataset.
model <- lmer(AGB_spatially_normalised_g_m2 ~ HAG_plotmean_of_cellmax_m * Wind_speed + (1|ProjectCode), data = peak_dataset)
summary(model)
# This summary suggests that wind is a signifcant interaction term.

# Plot model predictions
ggpredict(model, terms = c("HAG_plotmean_of_cellmax_m")) %>% plot()

predictions <- ggpredict(model, terms = c("HAG_plotmean_of_cellmax_m"))

dataset_simple <- peak_dataset %>%
    dplyr::select(AGB_spatially_normalised_g_m2, HAG_plotmean_of_cellmax_m) %>%
    distinct()

# Plot model predictions on data
(plot.model <- ggplot() +
        geom_line(data = predictions, aes(x = x, y = predicted),
                  size = 0.5) +
        geom_ribbon(data = predictions, aes(ymin = conf.low, ymax = conf.high, x = x), alpha = 0.4) +
        geom_point(data = dataset_simple, aes(x = HAG_plotmean_of_cellmax_m, y = AGB_spatially_normalised_g_m2),
                   alpha = 0.1, size = 2) +
        theme_classic() +
        labs(x = "\nHAG_plotmean_of_cellmax_m", y = "AGB_spatially_normalised_g_m2\n"))


# Visualise random effects
(re.effects <- plot_model(model, type = "re", show.values = TRUE))

# Add more stuff here...







#### 3. Project-level analysis ####
# Plot height above ground against aboveground biomass by ProjectCode.
dataset %>%
    filter(AGB_spatially_normalised_g_m2 > 0 & HAG_plotmean_of_cellmax_m > 0) %>%   # Filter incomplete observations.
    group_by(ProjectCode) %>%
    do({plot <- ggplot(., aes(x = HAG_plotmean_of_cellmax_m,
                              y = AGB_spatially_normalised_g_m2,
                              colour = binomial_species)) +
        geom_point(shape = 1, na.rm = TRUE) +
        scale_colour_viridis_d() +
        labs(x = expression("Mean canopy height (m)"),
             y = expression("Dry biomass (g m"^"-2"*")"),
             title = paste(.$ProjectCode)) +
        theme_coding() +
        theme(legend.position = c(0.35, 0.8)) +
        coord_cartesian(ylim = c(0, max_agb),
                        xlim = c(min_mean_hag, max_mean_hag),
                        expand = FALSE) +
        geom_smooth(method="lmrob",
                    formula= y ~ x-1,
                    aes(group=binomial_species),
                    se=TRUE, size=0.5, na.rm = TRUE)
    ggsave(paste0(loc_project, "/", unique(.$ProjectCode),
                  ".png", sep = ''), width = 10, height = 10,
           units = 'cm', plot = plot)
    })


#### 4. Species-level analysis ####
# Analyse mean (of max) HAG versus AGB, by species
list_of_species_plots <- list()                                                 # Initialize empty list for multipanel plot.

species_summary_table <- data.frame(plant_functional_type = character(),        # Create dataframe to collate model summary statistics.
                                    order = character(),
                                    family = character(),
                                    binomial_species = character(),
                                    nls_slope = double(),
                                    nls_slope_error = double(),
                                    nls_r2 = double(),
                                    nls_p = double(),
                                    rob_slope = double(),
                                    rob_slope_error = double(),
                                    rob_r2 = double(),
                                    rob_p = double(),
                                    Observations = integer(),
                                    n_of_surveys = integer(),
                                    Koppen_cc = character(),
                                    stringsAsFactors=FALSE
                                    )

# Pipeline for analysis
peak_dataset %>%
    filter(AGB_spatially_normalised_g_m2 > 0 & HAG_plotmean_of_cellmax_m > 0) %>%   # Filter incomplete observations.
    group_by(binomial_species) %>%

    do({
        # create plots for each species, grouped by ProjectCode.
        plot <- ggplot(., aes(x = HAG_plotmean_of_cellmax_m,
                              y = AGB_spatially_normalised_g_m2,
                              colour = ProjectCode)) +
        geom_point(shape = 1, na.rm = TRUE) +
        scale_colour_viridis_d() +
        labs(x = expression("Mean canopy height (m)"),
             y = expression("Dry biomass (g m"^"-2"*")"),
             title = paste(.$binomial_species)) +
        theme_coding() +
        theme(plot.title = element_text(face="italic")) +
        theme(legend.position = c(0.35, 0.8)) +
        coord_cartesian(ylim = c(0, max_agb),
                        xlim = c(min_mean_hag, max_mean_hag),
                        expand = FALSE) +
        geom_smooth(method="lmrob",
                    formula= y ~ x-1,
                    se=TRUE, size=0.5, colour = "black", na.rm = TRUE)
        ggsave(paste0(loc_species, "/", unique(.$binomial_species),
                  ".png", sep = ''), width = 10, height = 10,
           units = 'cm', plot = plot)                                           # Save plots

        # create plots for each species, pooling observations.
        plot_pooled <- ggplot(., aes(x = HAG_plotmean_of_cellmax_m,
                                          y = AGB_spatially_normalised_g_m2)) +
           geom_point(shape = 1, na.rm = TRUE) +
           scale_colour_viridis_d() +
           labs(x = expression("Mean canopy height (m)"),
                y = expression("Dry biomass (g m"^"-2"*")"),
                title = paste(.$binomial_species)) +
           theme_coding() +
           theme(plot.title = element_text(face="italic")) +
           theme(legend.position = c(0.35, 0.8)) +
           coord_cartesian(ylim = c(0, max_agb),
                           xlim = c(min_mean_hag, max_mean_hag),
                           expand = FALSE) +
           geom_smooth(method="lmrob",
                       formula= y ~ x-1,
                       se=TRUE, size=0.5, colour = "black", na.rm = TRUE)

        ggsave(paste0(loc_species, "/", unique(.$binomial_species),
                      " agregated.png", sep = ''), width = 10, height = 10,
               units = 'cm', plot = plot_pooled)                                # Save plots

# Isla's advice needed ####
# Adapting from the previous for loop (below) I'm trying to add each of these
# 'plot_pooled' plots to a list, so that they can be plotted together
# (with grid.arrange), but I can't get the syntax to work and haven't been able
# to find a solution information online.
# I think it's possible that there is an issue with assign inside the pipe,
# preventing plot_pooled from being added to the list_of_species_plots in the
# intended way.

        plotname <- paste("plot_pooled", unique(.$binomial_species), sep="")    # Assign unique name to each plot
        list_of_species_plots[[plotname]] <- plot_pooled                        # Add plots to list for composite figure

        # Fit and summarise species-level models
        model.lms <- lm(.$AGB_spatially_normalised_g_m2 ~ .$HAG_plotmean_of_cellmax_m + 0)  # Define model: linear with constrained intercept. # Ordinary Least Squares Regression
        model.rb <- lmrob(.$AGB_spatially_normalised_g_m2 ~ .$HAG_plotmean_of_cellmax_m + 0, method = 'MM')  # Define model: Robust Regression - Robustbase package (preferentially using MM-estimation)


# Isla's advice needed ####
# This isn't currently working. model info is not beeing added to the summary
# table - I think this might also be an issue with assignment from within the
# pipe.
# Do you think it's worth trying to use the Boom package or similar to summarise these model outputs?
# I tried using tidy(model.rb, conf.int=TRUE)  # It is strange that this doesn't work, the tidy function of Broom should support lmrob objects (https://www.rdocumentation.org/packages/broom/versions/0.5.2/topics/tidy.lmRob)

        model_info <- list()
        model_info <- c(plant_functional_type = as.character(unique(.$plant_functional_type)),
                        order = as.character(unique(.$PlotOrder)),
                        family = as.character(unique(.$PlotFamily)),
                        binomial_species = as.character(unique(.$binomial_species)),
                        nls_slope = round(summary(model.lms)$coefficients[1], digits = 2),
                        nls_slope_error = round(summary(model.lms)$coefficients[2], digits = 2),
                        nls_r2 = round(summary(model.lms)$r.squared, digits = 3),
                        nls_p = round(summary(model.lms)$coefficients[1,4], digits = 5),
                        rob_slope = round(summary(model.rb)$coefficients[1], digits = 2),
                        rob_slope_error = round(summary(model.rb)$coefficients[2], digits = 2),
                        rob_r2 = round(summary(model.rb)$r.squared, digits = 3),
                        rob_p = round(summary(model.rb)$coefficients[1,4], digits = 5),
                        observations = min(length(.$AGB_spatially_normalised_g_m2),length(.$HAG_plotmean_of_cellmax_m)),
                        n_of_surveys = length(unique(.$ProjectCode)),
                        Koppen_cc = paste(as.character(unique(.$KoppenCC)), collapse=", "))

        model_info <- data.frame(lapply(model_info, type.convert), stringsAsFactors=FALSE)  # Convert to dataframe.
        species_summary_table <- rbind(species_summary_table, model_info)       # Add new record to table.
        })






# print(list_of_species_plots)    # Development testing only

### Produce table of species-level results ###
# Order species_summary_table alphabetically by group, order, family and binomial species. NB. tolower deals with case issues.
species_summary_table <- species_summary_table[order(tolower(species_summary_table$plant_functional_type),
                                                     tolower(species_summary_table$order),
                                                     tolower(species_summary_table$family),
                                                     tolower(species_summary_table$binomial_species)
                                                     ),]

# Remove binomial species = 'Mixed Grasses' prior to exporting the table of model parameters.
species_summary_table2 <- subset(species_summary_table, binomial_species !='Mixed Grasses')

# Export model summary to excel file.
model_summary_path <- paste(loc_dataset,"/Species summaries.xlsx", sep = "")
write.xlsx(species_summary_table2, model_summary_path, row.names=FALSE)         # Export results table as Excel file.



### Create multipanel plots
# list_of_species_plots_2 <- list_of_species_plots excluding Mixed Grass        # Remove 'Mixed grasses' from the list of species-level plots

## Create multipanel figure of species plots ##
# # Landscape layout
#     number_of_col <- 6                                                          # Specify number of columns.
#     outfile <- file.path(loc_dataset,
#                          paste("TEST S1 HAG Vs AGB by taxa - wide.png",sep="")) # Specify output filename.
#     png(filename=outfile, width = 30, height = 18, units = 'cm',
#         res = 300, bg = 'white')                                                # NB. This PNG call is key to include a white background.
#     do.call("grid.arrange", c(plot_list_species,
#                               ncol=number_of_col,
#                               top = "Canopy height predicts biomass strongly at the species level"))
#     dev.off()
#
# # Portrait layout
#     number_of_col <- 4                                                          # Specify number of columns.
#     outfile <- file.path(loc_dataset,
#                          paste("TEST S1 HAG Vs AGB by species - tall.png",
#                                             sep=""))                            # Specify output filename.
#     png(filename=outfile, width = 18, height = 24, units = 'cm',
#         res = 300, bg = 'white')                                                # NB. This PNG call is key to include a white background.
#     do.call("grid.arrange", c(list_of_species_plots,
#                               ncol=number_of_col,
#                               top = "Canopy height predicts biomass strongly at the species level"))
#     dev.off()









############## old code from species for loop ----
# # For loop, to iterate through each species in the processed dataframe, subset speceis that have been processed, create plots.
# for (i in sort(unique(peak_dataset$binomial_species))){
#     taxa_df <- subset(peak_dataset, binomial_species==i)
#
#     x <- taxa_df$HAG_plotmean_of_cellmax_m                                      # Define x for models.
#     y <- taxa_df$AGB_spatially_normalised_g_m2                                  # Define y for models.
#
#     # #  check if some x and y observations exist for this taxa, and exclude 'Mixed Grasses' taxon from plottings.
#     # nam<-!is.na(data.frame(x,y))
#     # num_r<-apply(nam, 1,sum)
#     # valid_observations <- any(num_r==ncol(data.frame(x,y)))
#     # if(valid_observations && i != 'Mixed Grasses'){
#     #     meanHAG_vs_AGB_species <- ggplot(data = taxa_df,
#     #                 aes(x = HAG_plotmean_of_cellmax_m,
#     #                     y = AGB_spatially_normalised_g_m2,
#     #                     colour = ProjectCode)) +
#     #         geom_point(shape = 1, na.rm = TRUE) +
#     #         scale_colour_viridis_d() +
#     #         labs(x = expression("Mean canopy height (m)"),
#     #              y = expression("Dry biomass (g m"^"-2"*")"),
#     #              title = paste(i)) +
#     #         theme_coding() +
#     #         theme(plot.title = element_text(face="italic"),
#     #               legend.position = c(0.35, 0.8)) +
#     #         coord_cartesian(ylim = c(0, max_agb),
#     #                         xlim = c(min_mean_hag, max_mean_hag),
#     #                         expand=FALSE) +
#     #         geom_smooth(method="lmrob",
#     #                     formula= y ~ x-1,
#     #                     aes(group=ProjectCode,
#     #                         colour=ProjectCode),
#     #                     se=TRUE, size=0.5, na.rm = TRUE)
#
#         # # Save species-level plots
#         # outfile <- file.path(loc_species, paste("HAG_Vs_AGB ",i,".png",sep=""))
#         # png(filename=outfile, width = 10, height = 10, units = 'cm', res = 400)
#         # plot(meanHAG_vs_AGB_species)
#         # dev.off()
#
#         # aggregated plots
#         meanHAG_vs_AGB_species_combined <- ggplot(data = taxa_df,
#                     aes(x = HAG_plotmean_of_cellmax_m,
#                         y = AGB_spatially_normalised_g_m2)) +
#             geom_point(shape = 1, na.rm = TRUE) +
#             scale_colour_viridis_d() +
#             labs(x = expression("Mean canopy height (m)"),
#                  y = expression("Dry biomass (g m"^"-2"*")"),
#                  title = paste(i)) +
#             theme_coding() +
#             theme(plot.title = element_text(face="italic"),
#                   legend.position = c(0.35, 0.8)) +
#             coord_cartesian(ylim = c(0, max_agb), xlim = c(min_mean_hag, max_mean_hag), expand=FALSE) +
#             geom_smooth(method="lmrob", formula= y ~ x-1, se=TRUE, size=0.5, na.rm = TRUE)
#
#
#         # Save plots for multipanel figure
#             my_i <- i
#             plotname <- paste("plot", my_i, sep="")
#             list_of_species_plots[[plotname]] <- meanHAG_vs_AGB_species_combined
#
#         # Compute and summarise model statistics
#             # Ordinary Least Squares Regression
#             lms.model <- lm(y ~ x + 0) # Define model: linear with constrained intercept.
#             # summ(model)  # Pretty table summarising standard model output, from the jtools package, but not currently configured to work with rlm objects.
#
#             # Robust Regression - Robustbase package (using MM-estimation)
#             # rb.model <- lmrob(y ~ x + 0, control = lmrob.control(max.it = 50))
#             rb.model <- lmrob(y ~ x + 0, method = 'MM')
#
#             # Summarise parameters for table.
#             # NB. the Broom package might have been a more efficient way to go here, but I didn't know about it when writing this code.
#             model_info <- list()
#             model_info <- c(plant_functional_type = as.character(unique(taxa_df$plant_functional_type)),
#                             order = as.character(unique(taxa_df$PlotOrder)),
#                             family = as.character(unique(taxa_df$PlotFamily)),
#                             binomial_species = as.character(unique(taxa_df$binomial_species)),
#                             nls_slope = round(summary(lms.model)$coefficients[1], digits = 2),
#                             nls_slope_error = round(summary(lms.model)$coefficients[2], digits = 2),
#                             nls_r2 = round(summary(lms.model)$r.squared, digits = 3),
#                             nls_p = round(summary(lms.model)$coefficients[1,4], digits = 5),
#                             rob_slope = round(summary(rb.model)$coefficients[1], digits = 2),
#                             rob_slope_error = round(summary(rb.model)$coefficients[2], digits = 2),
#                             rob_r2 = round(summary(rb.model)$r.squared, digits = 3),
#                             rob_p = round(summary(rb.model)$coefficients[1,4], digits = 5),
#                             observations = min(length(taxa_df$AGB_spatially_normalised_g_m2),length(taxa_df$HAG_plotmean_of_cellmax_m)),
#                             n_of_surveys = length(unique(taxa_df$ProjectCode)),
#                             Koppen_cc = paste(as.character(unique(taxa_df$KoppenCC)), collapse=", "))
#
#         # Convert to dataframe
#             model_info <- data.frame(lapply(model_info, type.convert), stringsAsFactors=FALSE)
#
#         # Add new record to table.
#             species_summary_table <- rbind(species_summary_table, model_info)
#     }
# }

#
# # Create multipanel species plot # -
#     # Tall
#         number_of_col <- 4
#
#         outfile <- file.path(home, "outputs", paste("Figure S1 HAG Vs AGB by taxa.png",sep=""))
#         png(filename=outfile, width = 18, height = 24, units = 'cm', res = 300, bg = 'white')  # NB. This PNG call is key to include a white background.
#         do.call("grid.arrange", c(list_of_species_plots,
#                                   ncol=number_of_col,
#                                   top = "Canopy height predicts biomass strongly at the species level"))
#         dev.off()
#
#     # Wide
#         number_of_col <- 6
#
#         outfile <- file.path(home, "outputs", paste("Figure S1 HAG Vs AGB by taxa - wide.png",sep=""))
#         png(filename=outfile, width = 30, height = 18, units = 'cm', res = 300, bg = 'white')
#         do.call("grid.arrange", c(list_of_species_plots,
#                                   ncol=number_of_col,
#                                   top = "Canopy height predicts biomass strongly at the species level"))
#         dev.off()

# #### Table S1 ###
#     # Order species_summary_table alphabetically by group, order, family and binomial species. NB. tolower deals with case issues.
#     species_summary_table <- species_summary_table[order(tolower(species_summary_table$plant_functional_type),
#                                                          tolower(species_summary_table$order),
#                                                          tolower(species_summary_table$family),
#                                                          tolower(species_summary_table$binomial_species)
#                                                          ),]
#
#     # Remove binomial species = 'Mixed Grasses' prior to exporting the table of model parameters.
#     species_summary_table2 <- subset(species_summary_table, binomial_species !='Mixed Grasses')
#
#     # Export model summary to excel file.
#     model_summary_path <- paste(home,"/outputs/Species summaries.xlsx", sep = "")
#     write.xlsx(species_summary_table2, model_summary_path, row.names=FALSE)
#
#
#
#













#### 5. PFT-level analysis ####
### Plant functional type ###
pft_summary_table <- data.frame(plant_functional_type=character(),
                                ols_slope=double(),
                                ols_error=double(),
                                ols_r2=double(),
                                ols_p=double(),
                                rob_slope=double(),
                                rob_slope_error=double(),
                                rob_r2=double(),
                                rob_p=double(),
                                df=integer(),
                                number_of_species=character(),
                                stringsAsFactors=FALSE)                         # Create dataframe for model parameter summary.



peak_dataset %>%
    filter(AGB_spatially_normalised_g_m2 > 0 & HAG_plotmean_of_cellmax_m > 0) %>%  # Filter incomplete observations.
    group_by(plant_functional_type) %>%
    do({plot <- ggplot(., aes(x = HAG_plotmean_of_cellmax_m,
                              y = AGB_spatially_normalised_g_m2,
                              colour = binomial_species)) +
        geom_point(shape = 1, na.rm = TRUE) +
        scale_colour_viridis_d() +
        labs(x = expression("Mean canopy height (m)"),
             y = expression("Dry biomass (g m"^"-2"*")"),
             title = paste(.$plant_functional_type)) +
        theme_coding() +
        theme(legend.position = c(0.35, 0.8)) +
        coord_cartesian(ylim = c(0, max_agb),
                        xlim = c(0, max_mean_hag),
                        expand = FALSE) +
        geom_smooth(method="lmrob",
                    formula= y ~ x-1,
                    aes(group=binomial_species),
                    se=TRUE, size=0.5, na.rm = TRUE)            
        ggsave(paste0(loc_pft, "/", unique(.$plant_functional_type),            # Save plot 
                  ".png", sep = ''), width = 10, height = 10,
           units = 'cm', plot = plot)                           

    # Fit and summarise PFT-level models
    model.lms <- lm(.$AGB_spatially_normalised_g_m2 ~ .$HAG_plotmean_of_cellmax_m + 0)  # Fit linear model with OLS and constrained intercept.
    model.rb <- lmrob(.$AGB_spatially_normalised_g_m2 ~ .$HAG_plotmean_of_cellmax_m + 0, method = 'MM')  # Fit linear model with robust regression (preferentially via MM-estimation).

    # Isla's advice needed ####
    # Need to expand this pipe to return parameters of PFT-level fitted models (incorporating this functionality from the below for loop)

    model_info <- list()                                                        # Reset model info. But is this necessary?
    model_info <- c(plant_functional_type = as.character(unique(.$plant_functional_type)),
                    ols_slope = round(summary(model.lms)$coefficients[1], digits = 2),
                    ols_error = round(summary(model.lms)$coefficients[2], digits = 2),
                    ols_r2 = round(summary(model.lms)$r.squared, digits = 3),
                    ols_p = round(summary(model.lms)$coefficients[1,4], digits = 5),
                    rob_slope = round(summary(model.rb)$coefficients[1], digits = 2),
                    rob_slope_error = round(summary(model.rb)$coefficients[2], digits = 2),
                    rob_r2 = round(summary(model.rb)$r.squared, digits = 3),
                    rob_p = round(summary(model.rb)$coefficients[1,4], digits = 5),
                    df = summary(model.rb)$df[2],
                    number_of_species = length(unique(.$binomial_species)))
# 
#     # Convert to dataframe
#     model_info <- data.frame(lapply(model_info, type.convert), stringsAsFactors=FALSE)
# 
#     # Add new record to table.
#     pft_summary_table <- rbind(pft_summary_table, model_info)
    
    
    })
# Isla's advice needed ####
# Isla, have you got any idea what the error thrown by the above pipe is cased by? I tried following the tracebacks, but wasn't able to interpret the output.



# For loop, to iterate through each plant functional type, subset species that have been processed, create plots.
for (i in sort(unique(peak_dataset$plant_functional_type))){
    pft_df <- subset(peak_dataset, plant_functional_type==i)

    # define key variables
    x <- pft_df$HAG_plotmean_of_cellmax_m
    y <- pft_df$AGB_spatially_normalised_g_m2

    #  check if both x and y observations exist for this taxa.
    nam<-!is.na(data.frame(x,y))
    num_r<-apply(nam, 1,sum)
    valid_observations <- any(num_r==ncol(data.frame(x,y)))
    if(valid_observations){
        # p <- ggplot(data = pft_df,
        #             aes(x = HAG_plotmean_of_cellmax_m,
        #                 y = AGB_spatially_normalised_g_m2,
        #                 colour = binomial_species)) +
        #     geom_point(na.rm = TRUE) +
        #     scale_colour_viridis_d() +
        #     labs(x = expression("Mean canopy height (m)"),
        #          y = expression("Dry biomass (g m"^"-2"*")"),
        #          title = paste(i)) +
        #     theme_coding() +
        #     theme(plot.title = element_text(face="italic"),
        #           legend.position = c(0.35, 0.8)) +
        #     coord_cartesian(ylim = c(0, max_agb), xlim = c(min_mean_hag, max_mean_hag), expand=FALSE) +
        #     geom_smooth(method="lmrob", formula= y ~ x-1, aes(group=binomial_species, colour=binomial_species),
        #                 se=TRUE, color=1, size=0.5, na.rm = TRUE)
        #
        # # Save plots for multipanel figure
        # my_i <- i
        # plotname <- paste("plot", my_i, sep="")
        # plot_list[[plotname]] <- p
        #
        # outfile <- file.path(home, "outputs/HAG_Vs_AGB_by_PFT",paste("HAG Vs AGB ",i,".png",sep=""))
        # png(filename=outfile, width = 10, height = 10, units = 'cm', res = 600)
        # plot(p)
        # dev.off()

        # Compute and summarise model statistics
        lsm_model <- lm(y ~ x + 0) # Define model: linear with constrained intercept.
        rob_model <- lmrob(y ~ x + 0)
        model_info <- list()
        model_info <- c(plant_functional_type = as.character(unique(pft_df$plant_functional_type)),
                        slope = round(summary(rob_model)$coefficients[1], digits = 2),
                        slope_error = round(summary(rob_model)$coefficients[2], digits = 2),
                        r2 = round(summary(rob_model)$r.squared, digits = 3),
                        p = round(summary(rob_model)$coefficients[1,4], digits = 5),
                        df = summary(rob_model)$df[2],
                        number_of_species = length(unique(pft_df$binomial_species)))

        # Convert to dataframe
        model_info <- data.frame(lapply(model_info, type.convert), stringsAsFactors=FALSE)

        # Add new record to table.
        pft_summary_table <- rbind(pft_summary_table, model_info)
    }
}


#### Model parameters figure ####
# create theme. (nb. different theme treatment is required to plot the vertical gridlines by avoiding 'panel.grid = element_blank()')
theme_models <- function(){
    theme_bw()+
        theme(axis.text = element_text(size = 6),
              axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
              axis.title = element_text(size = 8),
              plot.margin = unit(c(0.5, 2.5, 0.5, 0.5), units = , "cm"),
              plot.title = element_text(size = 10, vjust = 1, hjust = 0.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 5, face = "italic"),
              legend.key.size = unit(0.9,"line"),
              legend.background = element_rect(color = "black", fill = "transparent", size = 4, linetype="blank"),
              legend.position = c(1.2, 0.6),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())
}


### Model parameters ###
# ceate new dataframe for plotting
    model_summary <- species_summary_table2
    model_summary$binomial_species <- factor(model_summary$binomial_species, levels = model_summary$binomial_species)
    model_summary$plant_functional_type <- factor(model_summary$plant_functional_type, levels = unique(model_summary$plant_functional_type))
    model_summary$nls_slope_min <- model_summary$nls_slope - model_summary$nls_slope_error
    model_summary$nls_slope_max <- model_summary$nls_slope + model_summary$nls_slope_error
    model_summary$rob_slope_min <- model_summary$rob_slope - model_summary$rob_slope_error
    model_summary$rob_slope_max <- model_summary$rob_slope + model_summary$rob_slope_error

# create theme. (nb. different theme treatment is required to plot the vertical gridlines by avoiding 'panel.grid = element_blank()')
    theme_models <- function(){
        theme_bw()+
            theme(axis.text = element_text(size = 6),
                  axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
                  axis.title = element_text(size = 8),
                  plot.margin = unit(c(0.5, 2.5, 0.5, 0.5), units = , "cm"),
                  plot.title = element_text(size = 10, vjust = 1, hjust = 0.5),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 5, face = "italic"),
                  legend.key.size = unit(0.9,"line"),
                  legend.background = element_rect(color = "black", fill = "transparent", size = 4, linetype="blank"),
                  legend.position = c(1.08, 0.6),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank())
    }

# Create plot of r2 values.
species_model_r2 <- ggplot(data = model_summary,
                           aes(x = model_summary$binomial_species, y = model_summary$rob_r2)) +
    geom_point(aes(shape=model_summary$plant_functional_type, col= model_summary$plant_functional_type), size=2) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_models() +
    theme(legend.title = element_text(size=8)) +
    scale_colour_viridis_d() +
    scale_shape_manual(values = pft_shapes) +
    grids(axis = "x", col="lightgrey", size=0.4, linetype = "dashed") +
    labs(x = expression("Species"),
         y = expression("Robust r"^"2"),
         title = expression("Species-level model parameters"),
         shape = "PFT",
         col = "PFT") +
    # without x-axis labels
    # theme(axis.title.x=element_blank(), axis.text.x = element_blank())
    # with x-axis labels
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face="italic"))

# Create plot of model slopes
species_model_slope <- ggplot(data = model_summary,
                              aes(x = model_summary$binomial_species, y = model_summary$rob_slope))+
    geom_point(aes(shape=model_summary$plant_functional_type, col= model_summary$plant_functional_type), size=2) +
    geom_linerange(aes(x = model_summary$binomial_species, ymin = model_summary$rob_slope_min, ymax = model_summary$rob_slope_max, col= model_summary$plant_functional_type)) +
    scale_y_continuous(limits = c(1, (1.15*max(model_summary$rob_slope))), breaks = scales::pretty_breaks(n = 6)) +
    theme_models() +
    scale_colour_viridis_d() +
    scale_shape_manual(values = pft_shapes) +
    grids(axis = "x", col="lightgrey", size=0.4, linetype = "dashed") +
    labs(x = expression("Species"),
         y = expression("model slope")) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face="italic"))

# Combine plots with ggarrange()[in ggpubr]
model.parameters <- ggarrange(species_model_r2, species_model_slope,
                              heights = c(10,10),
                              labels = c("(a)", "(b)"),
                              ncol = 1, nrow = 2,
                              align = "v")

# Export figure
png(filename="outputs/Figure 1 Species-level model parameters.png", width=14, height=18, units="cm", res=500)
plot(model.parameters)
dev.off()

# Create and export multipanel plot
n <- length(plot_list)
# nCol <- floor(sqrt(n))
nCol <- 3
multipanel <- do.call("grid.arrange", c(plot_list,
                                        ncol=nCol,
                                        top = "(mean of maximum) height as predictor of aboveground biomass by plant functional type"
)
)
outfile <- file.path(home, "outputs",paste("HAG_Vs_AGB by PFT.png",sep=""))
png(filename=outfile, width = 24, height = 18, units = 'cm', res = 600)
plot(multipanel)
dev.off()

# Export model summary to excel file.
model_summary_path <- paste(home,"/outputs/model_summary_PFT.xlsx", sep = "")
write.xlsx(pft_summary_table, model_summary_path, row.names=FALSE)


### Model parameter figure ###
# ceate new dataframe for plotting
pft_model_summary <- pft_summary_table
pft_model_summary$plant_functional_type <- factor(pft_model_summary$plant_functional_type, levels = unique(pft_model_summary$plant_functional_type))
pft_model_summary$slope_min <- pft_model_summary$slope - pft_model_summary$slope_error
pft_model_summary$slope_max <- pft_model_summary$slope + pft_model_summary$slope_error


# Create plot of pft r2 values.
pft_model_r2 <- ggplot(data = pft_model_summary,
                       aes(x = pft_model_summary$plant_functional_group, y = pft_model_summary$r2))+
    geom_point(aes(shape=pft_model_summary$plant_functional_type, col= pft_model_summary$plant_functional_type), size=2) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_models() +
    theme(legend.position = "none") +
    scale_colour_viridis_d() +
    scale_shape_manual(values = pft_shapes) +
    grids(axis = "x", col="lightgrey", size=0.4, linetype = "dashed") +
    labs(x = expression("Plant Functional Type"),
         y = expression("Robust r"^"2"),
         shape = "Plant functional type",
         col = "Plant functional type") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face="italic"))

# Create plot of model slopes
pft_model_slope <- ggplot(data = pft_model_summary,
                          aes(x = pft_model_summary$plant_functional_type, y = pft_model_summary$slope))+
    geom_point(aes(shape=pft_model_summary$plant_functional_type, col= pft_model_summary$plant_functional_type), size=2) +
    geom_linerange(aes(x = pft_model_summary$plant_functional_type, ymin = pft_model_summary$slope_min, ymax = pft_model_summary$slope_max, col= pft_model_summary$plant_functional_type)) +
    scale_y_continuous(limits = c(1, (1.15*max(pft_model_summary$slope))), breaks = scales::pretty_breaks(n = 5)) +
    theme_models() +
    scale_colour_viridis_d() +
    scale_shape_manual(values = pft_shapes) +
    grids(axis = "x", col="lightgrey", size=0.4, linetype = "dashed") +
    labs(x = expression("Plant Functional Type"),
         y = expression("model slope")) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face="italic"))


# Boxplots
# NB. before producing these boxplots, we need to reflect further on whether pooling data is approrpaite.
# Boxplot of pft r2 values.
# Boxplot of pft slopes values.









    # Combine plots with (nested) ggarrange()[in ggpubr]
    model.parameters <- ggarrange(pft_model_r2,
                                  pft_model_slope,
                                  heights = c(10,10),
                                  labels = c("(a)", "(b)"),
                                  ncol = 1, nrow = 3,
                                  align = "v")

    # Combine plots with (nested) ggarrange()[in ggpubr]
    # NB there is a unresovled problem here with the aethetic fill...
    # I'm waiting to confirm that this is the plot we want before spending more time perfecting this.
    # all_model_parameters <- ggarrange(ggarrange(pft_model_r2,
    #                                             pft_model_slope,
    #                                             ncol=2,
    #                                             labels =c("(a)", "(b)"),
    #                                             align = "v",
    #                                             widths = c(1,1)),
    #                                   species_model_r2,
    #                                   species_model_slope,
    #                                   heights = c(10,10),
    #                                   nrow = 3,
    #                                   labels = c("(c)", "(d)")
    #                                   # align = "v"
    #                                   )

    all_model_parameters_temp1 <- ggarrange(ggarrange(pft_model_r2,
                                                pft_model_slope,
                                                ncol=2,
                                                labels =c("(a)", "(b)"),
                                                align = "v",
                                                widths = c(1,1)),
                                      heights = c(10,10),
                                      nrow = 3,
                                      labels = c("(c)", "(d)")
                                      # align = "v"
                                    )

    # Export figures
        png(filename="outputs/Figure 2 all model parameters-t2.png", width=14, height=18, units="cm", res=400)
        plot(all_model_parameters_temp1)
        dev.off()






#### Sevilleta Analysis ----
# working with: sev_dataset
                                                                                # Sevilleta species sampled include:
                                                                                #  c("Larrea tridentata", "Bouteloua eriopoda", "Gutierrezia sarothrae", "Yucca elata")
                                                                                # and also several one off species (huniper, pinion, cane cholla cacti, prickely pear cacti)
# Compute scaling parameters from minimum and maximum values
max_mean_hag <- max(sev_dataset$HAG_plotmean_of_cellmax_m, na.rm = TRUE)
max_agb <- 1.35*max(sev_dataset$AGB_spatially_normalised_g_m2, na.rm = TRUE)

# Mean HAG Vs. AGB grouped by species
# (mean of maximum) canopy height as predictor of aboveground biomass by binomial taxa. Creates:
# (i) individual plots,
# (ii) multipanel figure,
# (iii) model summary statistics (as Excel sheet)

# Create dataframe to collate model summary statistics.
sev_summary_table <- data.frame(binomial_species = character(),
                                project_code = character(),
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

# For loop, to iterate through each individual dataset, subset speceis that have been processed, create plots.
for (i in sort(unique(sev_dataset$binomial_species))){
    taxa_df <- subset(sev_dataset, binomial_species==i)

    x <- taxa_df$HAG_plotmean_of_cellmax_m                                      # Define x for models.
    y <- taxa_df$AGB_spatially_normalised_g_m2                                  # Define y for models.

    #  check if both x and y observations exist for this subset
    nam<-!is.na(data.frame(x,y))
    num_r<-apply(nam, 1,sum)
    valid_observations <- any(num_r==ncol(data.frame(x,y)))
    if(valid_observations){
        # plots
            meanHAG_vs_AGB_species <- ggplot(data = taxa_df,
                                             aes(x = HAG_plotmean_of_cellmax_m,
                                                 y = AGB_spatially_normalised_g_m2,
                                                 colour = ProjectCode)) +
            geom_point(shape = 1, na.rm = TRUE) +
            scale_colour_viridis_d() +
            labs(x = expression("Mean canopy height (m)"),
                 y = expression("Dry biomass (g m"^"-2"*")"),
                 title = paste(i)) +
            theme_coding() +
            theme(plot.title = element_text(face="italic"),
                  legend.position = c(0.35, 0.8)) +
            coord_cartesian(ylim = c(0, max_agb), xlim = c(0, max_mean_hag), expand=FALSE) +
            geom_smooth(method="lmrob", formula= y ~ x-1, aes(group=ProjectCode, colour=ProjectCode), se=TRUE, size=0.5, na.rm = TRUE)

            # Save dataset-level plots
            outfile <- file.path(loc_sevilleta, paste("/HAG Vs AGB ",i,".png",sep=""))
            png(filename=outfile, width = 10, height = 10, units = 'cm', res = 400)
            plot(meanHAG_vs_AGB_species)
            dev.off()

            # Analysis subset by species and survey
            for (i in sort(unique(taxa_df$ProjectCode))){
                taxa_time_df <- subset(taxa_df, ProjectCode==i)

                # define key variables
                x <- taxa_time_df$HAG_plotmean_of_cellmax_m
                y <- taxa_time_df$AGB_spatially_normalised_g_m2

                #  check if both x and y observations exist for this subset
                nam<-!is.na(data.frame(x,y))
                num_r<-apply(nam, 1,sum)
                valid_observations <- any(num_r==ncol(data.frame(x,y)))
                if(valid_observations){

                # Fit and summarise models
                # Ordinary Least Squares Regression
                lms.model <- lm(y ~ x + 0) # Define model: linear with constrained intercept.
                # summary(model)
                # summ(model)  # Pretty table summarising standard model output, from the jtools package, but not currently configured to work with rlm objects.

                # Robust Regression - Robustbase package (using MM-estimation)
                # rb.model <- lmrob(y ~ x + 0, control = lmrob.control(max.it = 50))
                rb.model <- lmrob(y ~ x + 0, method = 'MM')
                # summary(rb.model)

                # Summarise parameters for table.
                # NB. the Broom package might have been a more efficient way to go here, but I didn't know about it when writing this code.
                model_info <- list()
                model_info <- c(binomial_species = as.character(unique(taxa_time_df$binomial_species)),
                                project_code = as.character(unique(taxa_time_df$ProjectCode)),
                                nls_slope = round(summary(lms.model)$coefficients[1], digits = 2),
                                nls_slope_error = round(summary(lms.model)$coefficients[2], digits = 2),
                                nls_r2 = round(summary(lms.model)$r.squared, digits = 3),
                                nls_p = round(summary(lms.model)$coefficients[1,4], digits = 5),
                                rob_slope = round(summary(rb.model)$coefficients[1], digits = 2),
                                rob_slope_error = round(summary(rb.model)$coefficients[2], digits = 2),
                                rob_r2 = round(summary(rb.model)$r.squared, digits = 3),
                                rob_p = round(summary(rb.model)$coefficients[1,4], digits = 5),
                                observations = min(length(taxa_time_df$AGB_spatially_normalised_g_m2),length(taxa_time_df$HAG_plotmean_of_cellmax_m))
                                )

                tidy.lmRob(rb.model)

                # Convert to dataframe
                model_info <- data.frame(lapply(model_info, type.convert), stringsAsFactors=FALSE)

                # Add new record to table.
                sev_summary_table <- rbind(sev_summary_table, model_info)
                }
                }

            }
        }

    # Export Sevilleta IAV model summary
    model_summary_path <- paste(loc_sevilleta, "/Sevilleta model summaries.xlsx", sep = "")
    write.xlsx(sev_summary_table, model_summary_path, row.names=FALSE)


# check whether the unwanted file exists and remove it
if (file.exists("Rplots.pdf")){
    file.remove("Rplots.pdf")
    }
