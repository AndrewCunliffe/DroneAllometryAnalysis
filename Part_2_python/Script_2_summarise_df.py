# Script to summarise database

# Import modules
import sys
import pandas as pd
from datetime import date
import astral
from astral import Observer
from astral.sun import sun


def main(df, loc_main_outputs):
    print("starting script 2 (Database summary)")
    today = str(date.today())

    # Create new pandas data frame for processed output.
    df_pro = pd.DataFrame(df)

    # Add new columns, populated with 'nan'.
    df_pro = pd.concat([df_pro,
                        pd.DataFrame(columns=['binomial_species',
                                              'species_short',
                                              'plot_area_from_field_length',
                                              'solar_elevation',
                                              'solar_noon_elevation',
                                              'solar_elevation_offset',
                                              'solar_noon_utc',
                                              'minutes_offset_from_solar_noon'
                                              ])],
                       sort=False)

    # Extract three-character site code from survey_code and add new column.
    df_pro['site_code'] = df_pro['survey_code'].str[-3:]

    # Concatenate genus and species into binomial species and species_short.
    df_pro['binomial_species'] = df_pro['plot_genus'] + " " + \
        df_pro['plot_species']

    df_pro['species_short'] = df_pro['plot_genus'].str[0] + ". " + \
        df_pro['plot_species']

    # Calculate harvest plot area from side length
    df_pro['plot_area_from_field_length'] = (df_pro['plot_side_length'] ** 2)

    # Calculate solar parameters (elevation and timing) using the Astral library
    for index_label, row in df_pro.iterrows():
        latitude = row['SiteLatitude']  # Define latitude parameter for observation.
        longitude = row['SiteLongitude']  # Define longitude parameter for observation.
        elevation = row['SiteElevation']  # Define elevation parameter for observation (supports more advanced sun calculations in future) # noqa
        survey_datetime = row['SurveyDateTimeUTC']

        location = Observer(latitude, longitude, elevation)  # specify observer location for Astral.

        solar_noon = astral.sun.noon(location, survey_datetime)  # Calculate time of solar noon.

        solar_elevation = astral.sun.elevation(location, survey_datetime)  # Calculate solar elevation during survey.
        solar_elevation = round(solar_elevation, 2)  # Tidy elevation value.

        solar_noon_elevation = astral.sun.elevation(location, solar_noon)  # Calculate solar elevation at noon.
        solar_noon_elevation = round(solar_noon_elevation, 2)

        solar_elevation_offset = solar_noon_elevation - solar_elevation
        solar_elevation_offset = round(solar_elevation_offset, 2)

        # Determine the absolute number of seconds between the survey and solar noon, and convert into minutes.
        time_offset = abs(survey_datetime - solar_noon).seconds / 60
        time_offset = round(time_offset, 0)

        # Write new values to dataframe
        df_pro.at[index_label, 'solar_elevation'] = solar_elevation
        df_pro.at[index_label, 'solar_noon_elevation'] = solar_noon_elevation
        df_pro.at[index_label, 'solar_elevation_offset'] = solar_elevation_offset
        df_pro.at[index_label, 'solar_noon_utc'] = solar_noon
        df_pro.at[index_label, 'minutes_offset_from_solar_noon'] = time_offset

    # Create new dataframe of observations sampled at peak biomass
    df_pro_peak = pd.DataFrame(df_pro[df_pro['PeakBiomass'] > 0])

    # Summarize database
    n_of_surveys = df['survey_code'].nunique()  # Returns the number of unique survey codes.
    list_of_surveys = list(set(df['survey_code']))  # Returns a list of unique survey codes.
    list_of_surveys.sort()  # Sorts list into ascending order
    list_of_surveys_sep = '\n'.join(list_of_surveys)  # Add line returns to list of surveys.
    n_of_sites = df_pro['SiteName'].nunique()  # Returns the number of unique sites.

    n_of_surveys_peak = df_pro_peak['survey_code'].nunique()  # Returns n of unique survey codes.
    # list_of_surveys_peak = list(set(df_pro_peak['survey_code']))  # Returns list of unique survey codes.
    # list_of_surveys_peak.sort()  # Sorts list into ascending order
    # list_of_surveys_peak_sep = '\n'.join(list_of_surveys_peak)  # Add line returns to list of surveys.

    n_of_contributing_teams = df['Investigators'].nunique()  # Returns the number of contributing teams.
    n_of_observations = df['RecordID'].nunique()  # Returns n of observations in the data set.
    n_of_observations_peak = sum(df_pro_peak['PeakBiomass'])  # Returns n of observations in the data set.
    sum_of_harvested_biomass = round((df['AGB'].sum()) / 1000, 2)  # Returns sum of harvested biomass in g.
    sum_of_harvested_peak_biomass = round(df.loc[df['PeakBiomass'] > 0, ['AGB'][0]].sum() / 1000, 2)
    n_of_sites_peak = df_pro_peak['site_code'].nunique()  # Returns the number of sites with peak observations.
    mean_offset_from_solar_noon = df_pro_peak['minutes_offset_from_solar_noon'].sum() / len(df_pro_peak['minutes_offset_from_solar_noon'])  # noqa
    mean_offset_from_solar_noon = round(mean_offset_from_solar_noon, 0)
    max_offset_from_solar_noon = df_pro_peak['minutes_offset_from_solar_noon'].max()

    # n_of_edited_plots = database['manual_filtering_required'].values.sum()  # noqa # Count the number of edited point clouds
    n_of_edited_plots_peak = df_pro_peak['manual_filtering_required'].values.sum()  # noqa # Count the number of edited point clouds

    n_of_igbp = df['IGBP_class'].nunique()  # count number of IGBP classes sampled
    n_of_igbp_peak = df_pro_peak['IGBP_class'].nunique()  # count number of IGBP classes sampled

    # Plant Functional_Type summaries.
    # list_of_groups = list(set(database_pro['plant_functional_type'].dropna()))
    n_of_graminoid_plots = len(df_pro[(df_pro['plant_functional_type'] == 'Graminoid')])  # noqa
    n_of_shrub_plots = len(df_pro[(df_pro['plant_functional_type'] == 'Shrub')])
    n_of_forb_plots = len(df_pro[(df_pro['plant_functional_type'] == 'Forb')])
    n_of_tree_plots = len(df_pro[(df_pro['plant_functional_type'] == 'Tree')])
    n_of_fern_plots = len(df_pro[(df_pro['plant_functional_type'] == 'Fern')])
    n_of_succulent_plots = len(df_pro[(df_pro['plant_functional_type'] == 'Succulent')])  # noqa

    n_of_graminoid_plots_peak = len(df_pro_peak[(df_pro_peak.AGB > 0) & (df_pro_peak.plant_functional_type == 'Graminoid')])  # noqa
    n_of_shrub_plots_peak = len(df_pro_peak[(df_pro_peak.AGB > 0) & (df_pro_peak.plant_functional_type == 'Shrub')])  # noqa
    n_of_forb_plots_peak = len(df_pro_peak[(df_pro_peak.AGB > 0) & (df_pro_peak.plant_functional_type == 'Forb')])  # noqa
    n_of_tree_plots_peak = len(df_pro_peak[(df_pro_peak.AGB > 0) & (df_pro_peak.plant_functional_type == 'Tree')])  # noqa
    n_of_fern_plots_peak = len(df_pro_peak[(df_pro_peak.AGB > 0) & (df_pro_peak.plant_functional_type == 'Fern')])  # noqa
    n_of_succulent_plots_peak = len(df_pro_peak[(df_pro_peak.AGB > 0) & (df_pro_peak.plant_functional_type == 'Succulent')])  # noqa
    n_of_bryophyte_plots_peak = len(df_pro_peak[(df_pro_peak.AGB > 0) & (df_pro_peak.plant_functional_type == 'Bryophyte')])  # noqa

    # Summarise sampled families
    list_of_families = sorted(list(set(df['plot_family'].dropna())))  # Returns a list of unique families.
    remove_list = ['TBC', 'Unknown']  # Create list of 'species' to remove.
    list_of_families = [i for i in list_of_families if i not in remove_list]  # Remove invalid 'families' from list
    n_of_families = len(list_of_families)  # Returns the number of unique records.
    list_of_families_sep = '\n'.join(list_of_families)  # Add line returns to list.

    # Summarise sampled families for peak biomass
    list_of_families_peak = sorted(list(set(df_pro_peak['plot_family'].dropna())))
    remove_list = ['TBC', 'Unknown']  # Create list of 'species' to remove.
    list_of_families_peak = [i for i in list_of_families_peak if i not in remove_list]
    n_of_families_peak = len(list_of_families_peak)  # Returns the number of unique records.
    list_of_families_peak_sep = '\n'.join(list_of_families_peak)  # Add line returns to list.

    # Summarise sampled species
    list_of_species = sorted(list(set(df_pro['binomial_species'].dropna())))  # noqa # Returns list of unique species/groups.
    remove_list = ['TBC TBC', 'Unknown spp.']  # Create list of 'species' to remove.
    list_of_species = [i for i in list_of_species if i not in remove_list]  # Remove invalid 'species' from list
    n_of_species = len(list_of_species)  # Returns the number of unique records.  # noqa
    list_of_species_sep = '\n'.join(list_of_species)  # Add line returns to list.

    # Summarise sampled species for peak biomass
    list_of_species_peak = sorted(list(set(df_pro_peak['binomial_species'].dropna())))  # noqa # Returns list of unique species/groups.
    remove_list = ['TBC TBC', 'Unknown spp.']  # Create list of 'species' to remove.
    list_of_species_peak = [i for i in list_of_species_peak if i not in remove_list]  # noqa # Remove invalid 'species' from list
    n_of_species_peak = len(list_of_species_peak)  # Returns the number of unique records.
    list_of_species_peak_sep = '\n'.join(list_of_species_peak)  # Add line returns to list.

    # Summary of wind conditions.
    wind_all_mean = round(sum(df['wind_speed']) / len(df['wind_speed']), 2)
    wind_all_max = round(max(df['wind_speed']), 2)
    wind_all_min = round(min(df['wind_speed']), 2)
    wind_all_mean_peak = round(sum(df_pro_peak['wind_speed']) / len(df_pro_peak['wind_speed']), 2)
    wind_all_max_peak = round(max(df_pro_peak['wind_speed']), 2)
    wind_all_min_peak = round(min(df_pro_peak['wind_speed']), 2)

    # Summary of species that were sub-sampled for determination of moisture content, for reporting in the methods.
    # Filter dataset by peak biomass = True and 'EntirePlantDried' = False
    temp = df_pro[(df_pro['PeakBiomass'] > 0) &
                  (df_pro['EntirePlantDried'] == 0)]
    # Extract list of species, sorted alphabetically.
    species_dry_subsample = sorted(list(set(temp['binomial_species'].dropna())))
    # Insert 'and', if needed, and remove quotation marks from list.
    if len(species_dry_subsample) > 1:
        species_dry_subsample.insert(-1, "and")
        species_dry_subsample = (', '.join(species_dry_subsample[:-2]) + ' ' + ' '.join(species_dry_subsample[-2:]))
    else:
        species_dry_subsample = ', '.join(list(species_dry_subsample))

    # Summarise drones used for sampling.
    n_of_drones = df['drone_platform'].nunique()
    n_of_drones_peak = df_pro_peak['drone_platform'].nunique()
    list_of_drones = sorted(list(set(df['drone_platform'].dropna())))
    list_of_drones = ', '.join(list(list_of_drones))  # Remove quotation marks from list.
    list_of_drones_peak = sorted(list(set(df_pro_peak['drone_platform'].dropna())))
    list_of_drones_peak = ', '.join(list(list_of_drones_peak))  # Remove quotation marks from list.

    # Summarise cameras used for sampling.
    n_of_cameras = df['camera_sensor'].nunique()
    n_of_cameras_peak = df_pro_peak['camera_sensor'].nunique()
    list_of_cameras = sorted(list(set(df['camera_sensor'].dropna())))
    list_of_cameras = ', '.join(list(list_of_cameras))  # Remove quotation marks from list.
    list_of_cameras_peak = sorted(list(set(df_pro_peak['camera_sensor'].dropna())))
    list_of_cameras_peak = ', '.join(list(list_of_cameras_peak))  # Remove quotation marks from list.

    # Output summary report.
    outf = open(loc_main_outputs + "Report 3 - project summary.txt", "w+")
    outf.write("Drone Allometry Experiment: Summary Report" + '\n')
    outf.write("Andrew Cunliffe <andrewmcunliffe@gmail.com>" + '\n')
    outf.write("Generated on " + today + '\n' + '\n')
    outf.write('---------------------------------------------------------------------------------' + '\n')
    outf.write('For the drone allometry project, focussing on seasonal biomass peaks, we have sampled ' +
               str(n_of_observations_peak) + ' individual harvest plots, from ' + str(n_of_sites_peak) +
               ' sites with ' + str(n_of_surveys_peak) + ' individual photogrammetric reconstructions' + '\n' + '\n')
    outf.write(str(sum_of_harvested_peak_biomass) + ' kg of aboveground biomass has been measured.' + '\n' + '\n')
    outf.write('Sampling was undertaken using ' + str(n_of_drones_peak) + ' types of drone: ' +
               str(list_of_drones_peak) + '.' + '\n' + '\n')
    outf.write('Sampling was undertaken using ' + str(n_of_cameras_peak) + ' types of camera: ' +
               str(list_of_cameras_peak) + '.' + '\n' + '\n')
    outf.write('For the largest taxa (' + str(species_dry_subsample) + '), freshly harvested biomass was weighed ' +
               'in the field and representative sub-samples were then dried to determine the moisture content ' +
               'for each partition (Cunliffe et al., 2020).' + '\n' + '\n')
    outf.write('In a few instances where plot infrastructure (e.g., posts or flags) were visible in the point cloud ' +
               '(n=' + str(n_of_edited_plots_peak) + ' plots), these points were manually assigned to a noise class ' +
               'and excluded from canopy height calculations.' + '\n' + '\n')
    outf.write('On average (across all harvest plots), surveys were conducted ' + str(mean_offset_from_solar_noon) +
               ' minutes from solar noon. The maximum gap between the survey and solar noon was ' +
               str(max_offset_from_solar_noon) + ' minutes.' + '\n' + '\n')
    outf.write('Across peak biomass surveys, the mean wind speed was ' + str(wind_all_mean_peak) +
               ' m s-1 (ranging from ' + str(wind_all_min_peak) + ' to ' + str(wind_all_max_peak) + ' m s-1).' +
               '\n' + '\n')

    outf.write('Sampling has occurred in ' + str(n_of_igbp_peak) + ' IGBP classes. ')
    outf.write('Sampled taxa include ' + str(n_of_species_peak) + ' species:' + '\n')
    outf.write(str(n_of_shrub_plots_peak) + ' shrub plots' + '\n')
    outf.write(str(n_of_graminoid_plots_peak) + ' graminoid plots' + '\n')
    outf.write(str(n_of_succulent_plots) + ' succulent plots' + '\n')
    outf.write(str(n_of_forb_plots_peak) + ' forb plots' + '\n')
    outf.write(str(n_of_tree_plots_peak) + ' tree plots' + '\n')
    outf.write(str(n_of_succulent_plots_peak) + ' succulent plots' + '\n')
    outf.write(str(n_of_bryophyte_plots_peak) + ' bryophyte plots' + '\n')
    outf.write(str(n_of_fern_plots_peak) + ' fern plots' + '\n' + '\n')
    outf.write('Families: (N = ' + str(n_of_families_peak) + ')' + '\n')
    outf.write(str(list_of_families_peak_sep) + '\n' + '\n')
    outf.write('Species (N = ' + str(n_of_species_peak) + '):' + '\n')
    outf.write(str(list_of_species_peak_sep) + '\n' + '\n')
    outf.write('---------------------------------------------------------------------------------' + '\n')
    outf.write('In total (including samples from non-seasonal biomass peak), we have sampled ' +
               str(n_of_observations) + ' individual harvest plots, from ' + str(n_of_sites) +
               ' sites, consisting of ' + str(n_of_surveys) +
               ' surveys contributed by ' + str(n_of_contributing_teams) + ' teams.' + '\n')
    outf.write('Sampling has occurred in ' + str(n_of_igbp) + ' IGBP classes.')
    outf.write('Sampled taxa include ' + str(n_of_species) + ' species:' + '\n')
    outf.write('In total ' + str(sum_of_harvested_biomass) +
               ' kg of dry biomass has been surveyed and then harvested.' + '\n')
    outf.write('Across all harvest plots and surveys, the mean wind speed was ' + str(wind_all_mean) +
               ' m s-1 (ranging from ' + str(wind_all_min) + ' to ' + str(wind_all_max) + ' m s-1).' + '\n')
    outf.write('Sampling was undertaken using ' + str(n_of_drones) + ' types of drone/sensor. (' +
               str(list_of_drones) + ')' + '\n')
    outf.write('\n')
    outf.write('Sampled taxa include:' + '\n')
    outf.write(str(n_of_shrub_plots) + ' shrub plots' + '\n')
    outf.write(str(n_of_graminoid_plots) + ' graminoid plots' + '\n')
    outf.write(str(n_of_succulent_plots) + ' succulent plots' + '\n')
    outf.write(str(n_of_forb_plots) + ' forb plots' + '\n')
    outf.write(str(n_of_fern_plots) + ' fern plots' + '\n')
    outf.write(str(n_of_tree_plots) + ' tree plots' + '\n')
    outf.write('\n')
    outf.write('Species (N = ' + str(n_of_species) + '):' + '\n')
    outf.write(str(list_of_species_sep) + '\n' + '\n')
    outf.write('Families: (N = ' + str(n_of_families) + ')' + '\n')
    outf.write(str(list_of_families_sep) + '\n' + '\n')
    outf.write('\n')
    outf.write("---------------------------------------------------------------------------------" + '\n')
    outf.write('There are ' + str(n_of_surveys) + ' survey codes in use.' + '\n')
    outf.write('(Date of survey (YYYYMMDD), Investigator initials (_XX), and three-digit site code (_XXX)), these are:'
               + '\n')
    outf.write(list_of_surveys_sep + '\n' + '\n')
    outf.write("---------------------------------------------------------------------------------" + '\n')
    outf.write('END OF REPORT')
    outf.close()

    # Generate survey description table:
    # Subset only peak biomass surveys
    survey_summary = df[df.PeakBiomass]

    # Subset columns to keep.
    col_keep_list = [
        'survey_code',
        'SiteLatitude',
        'SiteLongitude',
        'SiteElevation',
        'MAT',
        'MAP',
        'KoppenCC',
        'IGBP_class',
        'wind_speed',
        'sky_conditions_code',
        'drone_platform',
        'camera_sensor'
    ]

    survey_summary = pd.DataFrame(survey_summary[col_keep_list])

    # Drop duplicate rows from dataframe.
    survey_summary = survey_summary.drop_duplicates(['survey_code'], keep='last')

    # Export summary of surveys.
    survey_summary.to_csv(loc_main_outputs + 'summary of surveys.csv', na_rep='NA', index=False)  # noqa

    return df_pro


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
