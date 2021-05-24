# Geospatial processing script for the Drone Allometry project.
# Andrew Cunliffe <andrewmcunliffe@gmail.com>.
# Point cloud processing in PDAL.

# Import modules
import os
import sys
import pdal
import json
from rasterstats import zonal_stats
import geopandas as gpd
import pandas as pd
import numpy as np
from shapely.geometry import Polygon


def main(df_pro, loc_pc_plot_rev, loc_pc_plot_hag, loc_pc_plot_hag_clean, loc_rast_hag, loc_geojson,
         loc_proc_data, loc_geojson_hag):
    print("starting script 10 (HAG derivation)")

    # Derive height above ground (HAG) relative to a Delaunay TIN, overwriting Z dimension.
    print("10.1 - Extracting height above ground")
    for file in os.listdir(loc_pc_plot_rev):
        input_file = (loc_pc_plot_rev + file)  # Specify input filename for point cloud
        outfile = (loc_pc_plot_hag + file[:-16] + '_HAG.laz')
        hag_pipeline = [
                {
                    "type": "readers.las",
                    "filename": input_file
                },
                {
                    "type": "filters.hag_delaunay",
                    "allow_extrapolation": 'true'  # true = points outside TIN HAG relative to nearest ground point
                },
                {
                    "type": "filters.ferry",
                    "dimensions": "HeightAboveGround=Z"  # overwrite absolute Z with relative Z.
                },
                {
                    "type": "writers.las",
                    "filename": outfile,
                    "forward": "all"  # pass header information from input to output
                }
            ]
        hag_pipeline = pdal.Pipeline(json.dumps(hag_pipeline))  # convert pipeline into JSON format
        hag_pipeline.validate()  # validate the PDAL pipeline
        hag_pipeline.execute()  # run the PDAL pipeline

    # Clean point cloud, by removing negative HAGs, ground points (class 2) and noise (class 7)
    print("10.2 - Cleaning point clouds")
    for file in os.listdir(loc_pc_plot_hag):
        input_file = (loc_pc_plot_hag + file)  # Specify filename for input point cloud
        outfile = (loc_pc_plot_hag_clean + file[:-4] + 'clean.laz')
        hag_clean_pipeline = [
                {
                    "type": "readers.las",
                    "filename": input_file
                },
                {
                    "type": "filters.range",
                    "limits": "Classification![2:2]"  # remove ground points.
                },
                {
                    "type": "filters.range",
                    "limits": "Classification![7:7]"  # remove noise points.
                },
                {
                    "type": "filters.assign",
                    "assignment": "Z[:0]=0"  # Force points with Z<0 to 0.
                },
                {
                    "type": "writers.las",
                    "filename": outfile,
                    "forward": "all"  # pass header information from input to output
                }
            ]
        hag_clean_pipeline = pdal.Pipeline(json.dumps(hag_clean_pipeline))  # convert pipeline into JSON format
        hag_clean_pipeline.validate()  # validate the PDAL pipeline
        hag_clean_pipeline.execute()  # run the PDAL pipeline

    # Produce rasters summarising HAG (height above ground) point clouds.
    print("10.3 - Creating HAG rasters")
    for file in os.listdir(loc_pc_plot_hag_clean):
        input_file = (loc_pc_plot_hag_clean + file)  # Specify input filename for point cloud.
        outfile = (loc_rast_hag + file[:-9] + '.tif')
        hag_summary_pipeline = [
                {
                    "type": "readers.las",
                    "filename": input_file
                },
                {
                    "type": "writers.gdal",
                    "dimension": "Z",
                    "resolution": 0.01,
                    "radius": 0.01,
                    "window_size": 3,  # Nonzero value enables secondary interpolation to reduce nodata holes.
                    "output_type": "all",
                    "filename": outfile
                }
            ]
        hag_summary_pipeline = pdal.Pipeline(json.dumps(hag_summary_pipeline))  # convert pipeline into JSON format
        hag_summary_pipeline.validate()  # validate the PDAL pipeline
        hag_summary_pipeline.execute()  # run the PDAL pipeline

    print("10.4 - Summarising HAG rasters")
    # Add new results columns to processed output dataframe, populated with 'nan'.
    df_pro = pd.concat([df_pro,
                        pd.DataFrame(columns=['plot_area_from_coordinates_m2',
                                              'AGB_g_m2',
                                              'HAG_plotmin_of_cellmax_m',
                                              'HAG_plotmax_of_cellmax_m',
                                              'HAG_plotmean_of_cellmax_m',
                                              'HAG_plotmedian_of_cellmax_m',
                                              'HAG_plot90percentile_of_cellmax_m',
                                              'Count_of_nodata_cells_in_plot',
                                              'Count_of_points_cells_in_plot'
                                              ])],
                       sort=False)

    # Extract summary statistics from HAG rasters with 'rasterstats'. Percentile values can be adjusted as desired.
    for file in os.listdir(loc_rast_hag):
        outlist_project = []  # initialise list.
        outlist_plot = []  # initialise list.
        outlist_stats = []  # initialise list.
        input_raster = (loc_rast_hag + file)
        input_polygon_all = (loc_geojson + file[:15] + '.geojson')                                                      # retain only first 15 characters of file name. # noqa
        survey_code = (file[:15])                                                                                       # extract survey_code from raster filename. # noqa
        plot_id = (file[16:-8])                                                                                         # extract plot_id from raster filename. # noqa
        input_polygon = gpd.read_file(input_polygon_all, driver='GeoJSON')
        input_polygon = input_polygon[input_polygon['PlotID'] == plot_id]                                               # subset by plotID # noqa

        # Extract and save new geoJSONs with height above ground statistics in properties.
        # NB. the code will fail here if '_1' has been left on the end of a filename in the loc_pc_plot_rev directory.
        json_stats = zonal_stats(input_polygon, input_raster,
                                 band_num=2,  # specify raster band (1=min, 2=max, 3=mean, 4=IDW, 5=count, 6=StDev)
                                 stats=['min', 'mean', 'max', 'count', 'median', 'nodata', 'percentile_90'],
                                 prefix="cellmax_",
                                 geojson_out=True
                                 )

        outfile = os.path.join(loc_geojson_hag, survey_code + '_' + plot_id + '_with_height_stats.geojson')
        with open(outfile, 'w') as outfile:
            json.dump(json_stats, outfile)

        # Extract and save statistics to dataframe_pro
        outlist_project.append(survey_code)  # Update list.
        outlist_plot.append(plot_id)  # Update list.
        stats = zonal_stats(input_polygon, input_raster,
                            band_num=2,  # specify raster band: 1=min, 2=max, 3=mean, 4=count, 5=IDW, 6=StDev.
                            stats=['min', 'mean', 'max', 'count', 'median', 'nodata', 'percentile_90'],
                            prefix="cellmax_")
        outlist_stats.append(stats)  # Update list.
        stats_df = pd.DataFrame(stats)  # Convert stats object to a pandas dataframe.

        # Extract new statistics.
        hag_plotmin_of_cellmax_m = stats_df['cellmax_min'][0]
        hag_plotmax_of_cellmax_m = stats_df['cellmax_max'][0]
        hag_plotmean_of_cellmax_m = round(stats_df['cellmax_mean'][0], 4)  # rounded to n dp.
        count_of_points_cells_in_plot = stats_df['cellmax_count'][0]
        hag_plotmedian_of_cellmax_m = stats_df['cellmax_median'][0]
        hag_plotpercentile_of_cellmax_m = stats_df['cellmax_percentile_90'][0]
        count_of_nodata_cells_in_plot = stats_df['cellmax_nodata'][0]

        # Add new statistics to dataframe.
        df_pro.loc[df_pro.survey_code.str.match(survey_code) &
                   df_pro.PlotID.str.match(plot_id),
                   ['HAG_plotmin_of_cellmax_m',
                    'HAG_plotmax_of_cellmax_m',
                    'HAG_plotmean_of_cellmax_m',
                    'HAG_plotmedian_of_cellmax_m',
                    'HAG_plot90percentile_of_cellmax_m',
                    'Count_of_nodata_cells_in_plot',
                    'Count_of_points_cells_in_plot'
                    ]] = [hag_plotmin_of_cellmax_m,
                          hag_plotmax_of_cellmax_m,
                          hag_plotmean_of_cellmax_m,
                          hag_plotmedian_of_cellmax_m,
                          hag_plotpercentile_of_cellmax_m,
                          count_of_nodata_cells_in_plot,
                          count_of_points_cells_in_plot
                          ]

    # Derive area of each plot based on reported coordinates, and add this to the 'database_pro' dataframe.
    for row in df_pro.itertuples():
        survey_code = row.survey_code  # extract survey_code.
        plot_id = row.PlotID  # extract PlotID code.
        # Calculate plot area from coordinates.
        geometry = Polygon([(row.Corner1_X,
                             row.Corner1_Y,
                             row.Corner1_Z),
                            (row.Corner2_X,
                             row.Corner2_Y,
                             row.Corner2_Z),
                            (row.Corner3_X,
                             row.Corner3_Y,
                             row.Corner3_Z),
                            (row.Corner4_X,
                             row.Corner4_Y,
                             row.Corner4_Z)
                            ])
        plot_area_from_coordinates = round(geometry.area, 4)  # derive area of polygon, rounded to n dp.

        # Use 'Harvest_frame_used' bool to select the most appropriate area for each plot:
        if row.Harvest_frame_used:
            best_plot_area = row.plot_area_from_field_length
        else:
            best_plot_area = plot_area_from_coordinates

        # Calculate spatially normalised AGB for each plot, and add this to the 'database_pro' data frame.
        agb_norm = row.AGB / best_plot_area

        df_pro.loc[df_pro.survey_code.str.match(survey_code) &
                   df_pro.PlotID.str.match(plot_id),
                   ['plot_area_from_coordinates_m2',
                    'AGB_g_m2'
                    ]] = [plot_area_from_coordinates,
                          agb_norm
                          ]

    # Replace fields that are empty of entirely spaces with NaN.
    df_pro = df_pro.replace(r'^\s*$', np.nan, regex=True)

    # Duplicate df to pass onwards in the pipeline
    df_pro2 = df_pro

    # Strip timezone information from datetime, changing it from timezone aware to naive, to allow export to Excel.
    # (because Excel cannot handle timezone information)
    df_pro['SurveyDateTimeUTC'] = pd.DatetimeIndex(df_pro.SurveyDateTimeUTC.dt.tz_localize(tz=None)).astype(str)
    df_pro['solar_noon_utc'] = pd.DatetimeIndex(df_pro.solar_noon_utc.dt.tz_localize(tz=None)).astype(str)

    # Export 'dataframe_pro' as a new file for subsequent analysis.
    df_pro.to_csv(loc_proc_data + 'processed_database.csv', na_rep='NA', index=False)                                # Primary file used for R analysis

    df_pro.to_excel(loc_proc_data + 'processed_database.xlsx', na_rep='NA', index=False,
                    freeze_panes=(1, 2),
                    engine=None)                                                                                        # Secondary Excel file for easy viewing

    return df_pro2


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5], sys.argv[6], sys.argv[7], sys.argv[8])
