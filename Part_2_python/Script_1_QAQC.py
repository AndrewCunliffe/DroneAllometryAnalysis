# Script to perform quality assurance and quality control on the input datasets

# Import modules
import sys
import pandas as pd
from datetime import date
from shapely.geometry import LinearRing


def main(df, loc_main_outputs):
    print("starting script 1 (Quality assurance and quality control)")
    today = str(date.today())

    # Quality control checks. NA values will return 'False/Implausible' unless .dropna() is used.
    qc_messages = []  # initialise QC message list.

    # Check for missing strings.
    if df['PlotID'].isnull().any():
        qc_messages.append('WARNING: missing entry in PlotID')
    if df['plot_species'].dropna().isnull().any():
        qc_messages.append('WARNING: missing entry in plot_species')
    if df['EPSG'].isnull().any():
        qc_messages.append('WARNING: missing entry in EPSG')
    if df['UTM_zone'].isnull().any():
        qc_messages.append('WARNING: missing entry in UTM_zone')
    if df['AGB'].isnull().any():
        qc_messages.append('WARNING: missing entry in AGB')

    # QC checks on 'life_cycle_strategy'
    # Check for missing strings
    if df['life_cycle_strategy'].isnull().any():
        qc_messages.append('WARNING: missing entry in life cycle strategy')
    # check for unexpected entries in the list
    expected_lcs = ['perennial', 'annual', 'biennial']  # List expected entries
    lcs_entries = list(df.life_cycle_strategy.dropna().unique())  # Return observed entries
    if not all(elem in expected_lcs for elem in lcs_entries):
        qc_messages.append('WARNING: unexpected entry in life cycle strategy')

    # QC MAT. Range limits derived from extreme values observed globally.
    if not df['MAT'].between(-60, 40, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in MAT')

    # QC MAP
    if not df['MAP'].between(10, 12000, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in MAP')

    # QC Site Coordinates
    elevation_limit_low = -500  # This value is the minimum possible Elevation on the Earth's surface.
    elevation_limit_high = 9000  # This value is the maximum possible Elevation on the Earth's surface.
    if not df['SiteElevation'].between(elevation_limit_low, elevation_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in SiteElevation')
    if not df['SiteLatitude'].between(-90, 90, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in SiteLatitude')
    if not df['SiteLongitude'].between(-180, 180, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in SiteLongitude')

    # QC check for unexpected entries in the list of Plant Functional Type.
    expected_pft = ['Shrub', 'Graminoid', 'Succulent', 'Forb', 'Fern', 'Tree', 'Bryophyte']
    pft_entries = list(df.plant_functional_type.unique())
    if not all(elem in expected_pft for elem in pft_entries):
        qc_messages.append('WARNING: unexpected entry in Functional Group')

    # QC reported survey and harvest dates
    first_survey = '2016-07-24'  # Date for first 'legitimate' survey in dataset.
    first_harvest_date = min(df['DateHarvest'])
    last_harvest_date = max(df['DateHarvest'])
    if not df['DateSurvey'].between(first_survey, today, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in DateSurvey')
    # if not database['DateHarvest'].between(first_survey, today, inclusive=True).all():
    #     qc_messages.append('WARNING: implausible value(s) in DateHarvest')
    if not df['DateHarvest'].between(first_survey, today, inclusive=True).all():
        qc_messages.append('WARNING: missing or implausible value(s) in DateHarvest. ' +
                           'The earliest harvest date is ' + str(first_harvest_date) +
                           ' and the last harvest date is ' + str(last_harvest_date))
    if ((df['DateHarvest'] - df['DateSurvey']) < pd.Timedelta('0 days')).any():
        qc_messages.append('WARNING: date of harvest precedes date of survey')
    if ((df['DateHarvest'] - df['DateSurvey']) > pd.Timedelta('20 days')).any():
        qc_messages.append('WARNING: long delay between survey and harvest (>20 days)')
    if ((df['DateHarvest'] - df['DateSurvey']) > pd.Timedelta('30 days')).any():
        qc_messages.append('WARNING: long delay between survey and harvest (>30 days)')

    # QC UTM Coordinates
    # Missing value check
    if df['Corner1_X'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner1_X')
    if df['Corner1_Y'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner1_Y')
    if df['Corner1_Z'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner1_Z')
    if df['Corner2_X'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner2_X')
    if df['Corner2_Y'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner2_Y')
    if df['Corner2_Z'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner2_Z')
    if df['Corner3_X'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner3_X')
    if df['Corner3_Y'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner3_Y')
    if df['Corner3_Z'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner3_Z')
    if df['Corner4_X'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner4_X')
    if df['Corner4_Y'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner4_Y')
    if df['Corner4_Z'].isnull().any():
        qc_messages.append('WARNING: missing value(s) in Corner4_Z')

    # Eastings range check
    utm_easting_limit_low = 167000  # This value is the minimum possible Easting in the UTM system.
    utm_easting_limit_high = 833000  # This value is the maximum possible Easting in the UTM system.
    if not df['Corner1_X'].dropna().between(utm_easting_limit_low, utm_easting_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Corner1_X')
    if not df['Corner2_X'].dropna().between(utm_easting_limit_low, utm_easting_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Corner2_X')
    if not df['Corner3_X'].dropna().between(utm_easting_limit_low, utm_easting_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Corner3_X')
    if not df['Corner4_X'].dropna().between(utm_easting_limit_low, utm_easting_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Corner4_X')

    # Northings range check
    utm_northing_limit_low = 0  # This value is the minimum possible Northing in the UTM system.
    utm_northing_limit_high = 10000000  # This value is the maximum possible Northing in the UTM system.
    if not df['Corner1_Y'].dropna().between(utm_northing_limit_low, utm_northing_limit_high, inclusive=True).all():  # noqa
        qc_messages.append('WARNING: implausible value(s) in Corner1_Y')
    if not df['Corner2_Y'].dropna().between(utm_northing_limit_low, utm_northing_limit_high, inclusive=True).all():  # noqa
        qc_messages.append('WARNING: implausible value(s) in Corner2_Y')
    if not df['Corner3_Y'].dropna().between(utm_northing_limit_low, utm_northing_limit_high, inclusive=True).all():  # noqa
        qc_messages.append('WARNING: implausible value(s) in Corner3_Y')
    if not df['Corner4_Y'].dropna().between(utm_northing_limit_low, utm_northing_limit_high, inclusive=True).all():  # noqa
        qc_messages.append('WARNING: implausible value(s) in Corner4_Y')

    # QC Plot Corner Elevation
    if not df['Corner1_Z'].dropna().between(elevation_limit_low, elevation_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Corner1_Z')
    if not df['Corner2_Z'].dropna().between(elevation_limit_low, elevation_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Corner2_Z')
    if not df['Corner3_Z'].dropna().between(elevation_limit_low, elevation_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Corner3_Z')
    if not df['Corner4_Z'].dropna().between(elevation_limit_low, elevation_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Corner4_Z')

    # QC AGB
    if not df['AGB'].dropna().between(0, 2000000, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in AGB')

    # QC Carbon Concentrations
    carbon_concentration_limit_low = 25
    carbon_concentration_limit_high = 75
    if not df['CarbCon'].dropna().between(carbon_concentration_limit_low, carbon_concentration_limit_high,
                                          inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in CarbCon')
    if not df['Comp1_CarbCon'].dropna().between(carbon_concentration_limit_low,
                                                carbon_concentration_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Comp1_CarbCon')
    if not df['Comp2_CarbCon'].dropna().between(carbon_concentration_limit_low,
                                                carbon_concentration_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Comp2_CarbCon')
    if not df['Comp3_CarbCon'].dropna().between(carbon_concentration_limit_low,
                                                carbon_concentration_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Comp3_CarbCon')
    if not df['Comp4_CarbCon'].dropna().between(carbon_concentration_limit_low,
                                                carbon_concentration_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Comp4_CarbCon')

    # QC Nitrogen Concentrations
    nitrogen_concentration_limit_low = 0.02
    nitrogen_concentration_limit_high = 20
    if not df['NitCon'].dropna().between(nitrogen_concentration_limit_low,
                                         nitrogen_concentration_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in NitCon')
    if not df['Comp1_NitCon'].dropna().between(nitrogen_concentration_limit_low,
                                               nitrogen_concentration_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Comp1_NitCon')
    if not df['Comp2_NitCon'].dropna().between(nitrogen_concentration_limit_low,
                                               nitrogen_concentration_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Comp2_NitCon')
    if not df['Comp3_NitCon'].dropna().between(nitrogen_concentration_limit_low,
                                               nitrogen_concentration_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Comp3_NitCon')
    if not df['Comp4_NitCon'].dropna().between(nitrogen_concentration_limit_low,
                                               nitrogen_concentration_limit_high, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Comp4_NitCon')

    # Quality control checks on coordinates, verifying polygons are (i) linear rings, and (ii) counter-clockwise
    # Initialise lists of errors
    list_of_cw_polygons = []
    list_of_self_intersecting_polygons = []

    # For loop to iterate across all rows and verify each polygon.
    for index, row in df.iterrows():
        record_id = row['RecordID']  # Update RecordID for each new row.
        geometry = LinearRing([(row['Corner1_X'],
                                row['Corner1_Y'],
                                row['Corner1_Z']),
                               (row['Corner2_X'],
                                row['Corner2_Y'],
                                row['Corner2_Z']),
                               (row['Corner3_X'],
                                row['Corner3_Y'],
                                row['Corner3_Z']),
                               (row['Corner4_X'],
                                row['Corner4_Y'],
                                row['Corner4_Z'])
                               ])

        # Verify LinearRing is a linear ring (and not self-intersecting). If it fails, add RecordID to list for report.
        if not geometry.is_simple:
            list_of_self_intersecting_polygons.append(record_id)

        # Verify LinearRing is counter-clockwise (i.e. face up). If it fails, add RecordID to list for report.
        if not geometry.is_ccw:
            list_of_cw_polygons.append(record_id)

    # count number of problematic polygons
    n_of_list_of_self_intersecting_polygons = len(list_of_self_intersecting_polygons)
    n_of_list_of_cw_polygons = len(list_of_cw_polygons)
    # Add messaged to qc_messages list
    if n_of_list_of_self_intersecting_polygons > 0:
        qc_messages.append('CRITICAL WARNING: invalid polygon(s), due to inconsistent coordinate sequence. '
                           + str(n_of_list_of_self_intersecting_polygons) + ' polygons are affected')
    if n_of_list_of_cw_polygons > 0:
        qc_messages.append('WARNING: non-ideal polygon(s), due to clockwise coordinate sequence. '
                           + str(n_of_list_of_cw_polygons) + ' polygons are affected')

    # QC of wind values.
    wind_limit_min = 0
    wind_limit_max = 15
    if not df['wind_speed'].dropna().between(wind_limit_min, wind_limit_max, inclusive=True).all():
        qc_messages.append('WARNING: implausible value(s) in Wind_speed')

    # Count number of warning and error messages
    n_of_warnings = len(qc_messages)
    qc_messages_sep = '\n'.join(qc_messages)

    # Output QAQC report
    outf = open(loc_main_outputs + "Report 1 - QAQC.txt", "w+")
    outf.write("Drone Allometry Experiment: QAQC Report" + '\n')
    outf.write("Andrew Cunliffe <andrewmcunliffe@gmail.com>" + '\n')
    outf.write("Generated on " + today + '\n' + '\n')
    outf.write("---------------------------------------------------------------------------------" + '\n')
    outf.write('Quality Control Check Summary' + '\n')
    outf.write('Number of QC warnings found = ' + str(n_of_warnings) + '\n' + '\n')
    outf.write(str(qc_messages_sep) + '\n' + '\n')
    if n_of_list_of_self_intersecting_polygons > 0:
        outf.write('RecordIDs of self-intersecting polygons:' + '\n')
        outf.write(str(list_of_self_intersecting_polygons) + '\n' + '\n')
    if n_of_list_of_cw_polygons > 0:
        outf.write('RecordIDs of clockwise (incorrect) polygons:' + '\n')
        outf.write(str(list_of_cw_polygons) + '\n' + '\n')
    outf.write('---------------------------------------------------------------------------------' + '\n')
    outf.write('END OF REPORT')
    outf.close()

    # Terminate program if polygon coordinates are self-intersecting.
    if n_of_list_of_self_intersecting_polygons > 0:
        print('FATAL ERROR: Self-intersecting polygons found. ACTION: Program terminated. '
              'SOLUTION: Clean input data - see summary report for problematic record ID(s)')
        exit()


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
