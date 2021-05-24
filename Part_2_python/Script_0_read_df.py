# Script to perform quality assurance and quality control on the input datasets

# Import modules
import sys
import pandas as pd


def main(input_spreadsheet_loc):
    print("starting script 0 (read database)")

    # Read in summary database as a pandas dataframe.
    df = pd.read_excel(input_spreadsheet_loc + "Summary_Database.xlsx",
                       skiprows=range(1, 3),
                       quotechar='"',
                       parse_dates=['DateSurvey', 'SurveyDateTimeUTC', 'DateHarvest'],
                       dtype={'survey_code': str,
                              'PlotID': str,
                              'RecordID': int,
                              'contributor_assigned_plot_id': str,
                              'email': str,
                              'Investigators': str,
                              'SevilletaIAV': bool,
                              'SuitableForParamSenExp': bool,
                              'SiteName': str,
                              'Country': str,
                              'SiteLatitude': float,
                              'SiteLongitude': float,
                              'SiteElevation': float,
                              'MAT': float,
                              'MAP': float,
                              'KoppenCC': str,
                              'KoppenCC_long': str,
                              'IGBP_class': int,
                              'IGBP_class_long': str,
                              'community_description': str,
                              'community_species': str,
                              'phenology': str,
                              'plant_functional_type': str,
                              'plot_order': str,
                              'plot_family': str,
                              'plot_genus': str,
                              'plot_species': str,
                              'plot_subspecies': str,
                              'plot_species_common_name': str,
                              'photosynthetic_pathway': str,
                              'life_cycle_strategy': str,
                              'additional_species_in_plot': str,
                              'soil': str,
                              'disturbance': str,
                              'PeakBiomass': bool,
                              'EntirePlantDried': bool,
                              'SubsampleDried': bool,
                              'AveragedMoistureContent': bool,
                              'EPSG': str,
                              'UTM_zone': str,
                              'Corner1_X': float,
                              'Corner1_Y': float,
                              'Corner1_Z': float,
                              'Corner2_X': float,
                              'Corner2_Y': float,
                              'Corner2_Z': float,
                              'Corner3_X': float,
                              'Corner3_Y': float,
                              'Corner3_Z': float,
                              'Corner4_X': float,
                              'Corner4_Y': float,
                              'Corner4_Z': float,
                              'Harvest_frame_used': bool,
                              'plot_side_length': float,
                              'AGB': float,
                              'Comp1': str,
                              'Comp1_Mass': float,
                              'Comp2': str,
                              'Comp2_Mass': float,
                              'Comp3': str,
                              'Comp3_Mass': float,
                              'Comp4': str,
                              'Comp4_Mass': float,
                              'Field_max_height': float,
                              'canopy_diameter_max': float,
                              'canopy_diameter_perpendicular': float,
                              'Biomass_Subsample_Archived': str,  # NB. really this should be bool.
                              'CarbCon': float,
                              'Comp1_CarbCon': float,
                              'Comp2_CarbCon': float,
                              'Comp3_CarbCon': float,
                              'Comp4_CarbCon': float,
                              'NitCon': float,
                              'Comp1_NitCon': float,
                              'Comp2_NitCon': float,
                              'Comp3_NitCon': float,
                              'Comp4_NitCon': float,
                              'Notes_on_survey_harvest': str,
                              'Notes_on_plot': str,
                              'Acknowledgements': str,
                              'Acknowledgements_Funding': str,
                              'Acknowledgements_Permission': str,
                              'Acknowledgements_Assistance': str,
                              'Agreement': str,
                              'Protocol_version': str,
                              'wind_speed': float,
                              'sky_conditions_code': int,
                              'sky_conditions': str,
                              'drone_platform': str,
                              'camera_sensor': str,
                              'CameraShutterType': str,
                              'minimum_image_sharpness': float,
                              'Notes_from_processing_and_review': str,
                              'point_cloud_quality': 'Int64',
                              'corner_apparent_bias': float,
                              'manual_filtering_required': bool
                              }
                       )

    # Remove leading and trailing whitespace from all non-object dtype columns.
    # df = df.apply(lambda x: x.str.strip() if x.dtype == "object" else x)  # This check conflicts with the new datetime handling # noqa

    # Round variables (e.g. plot coordinates to 3 dp (i.e. 1 mm))
    df = df.round({'SiteLatitude': 3,
                   'SiteLongitude': 3,
                   'SiteElevation': 0,
                   'MAT': 1,
                   'MAP': 0,
                   'Corner1_X': 3,
                   'Corner1_Y': 3,
                   'Corner1_Z': 3,
                   'Corner2_X': 3,
                   'Corner2_Y': 3,
                   'Corner2_Z': 3,
                   'Corner3_X': 3,
                   'Corner3_Y': 3,
                   'Corner3_Z': 3,
                   'Corner4_X': 3,
                   'Corner4_Y': 3,
                   'Corner4_Z': 3
                   })

    return df


if __name__ == "__main__":
    main(sys.argv[1])
