# Geospatial processing script for the Drone Allometry project.
# Andrew Cunliffe <andrewmcunliffe@gmail.com>.
# Point cloud clipping processing in PDAL.

# Import modules
import os
import sys
import pdal
import json
import shapely.wkt
from shapely.geometry import Polygon
import geopandas as gpd
import psutil


# This script checks whether any plot-level point clouds have been extracted for all point clouds present, and
# extracts any that have not yet been processed. Implemented over the network, as full point clouds are >200 GB.
def main(input_pointcloud_loc, loc_geojson, loc_pc_all_plots, loc_pc_plot, df):
    print("starting script 8 (point cloud clipping)")

    # Create list of processed (clipped) survey_code codes
    list_of_processed_survey_codes = []
    for subsetPC in os.listdir(loc_pc_all_plots):
        survey = subsetPC[:15]
        list_of_processed_survey_codes.append(survey)
    list_of_processed_survey_codes = list(set(list_of_processed_survey_codes))  # Returns a list of unique codes.

    # NB. Ideally there should be a check included here that only runs the following memory checks if there are actually files to clip # noqa

    # Check available memory
    mem = psutil.virtual_memory()  # Summarise system memory
    mem_gb_tot = round(mem.total / (1024. ** 3), 1)
    mem_gb_avil = round(mem.available / (1024. ** 3), 1)
    print(str(mem_gb_tot) + " GB of RAM installed, of which " + str(mem_gb_avil) + " is available.")
    if mem_gb_tot > 256:
        print(str(mem_gb_tot) + " GB of memory should be enough!")
    elif mem_gb_tot > 128:
        print(str(mem_gb_tot) + " GB of memory should be enough!")
    elif mem_gb_tot > 64:
        print(str(mem_gb_tot) + " GB of memory should be enough!")
    elif mem_gb_tot > 32:
        print(str(mem_gb_tot) + " GB of memory might not be enough to read in very large point clouds!")
    elif mem_gb_tot > 15:
        print("WARNING: " + str(mem_gb_tot) + " GB of memory will not be enough to read in very large clouds.")
    else:
        print("WARNING very limited RAM. This won't be enough to read large point clouds into memory")

    for file in os.listdir(input_pointcloud_loc):
        survey = file[:15]  # Specify survey_code under consideration
        # check if this point cloud has been already been processed
        if survey not in list_of_processed_survey_codes:
            # check geoJSON file exists
            if not os.path.isfile(loc_geojson + survey + '.geojson'):
                print('WARNING: .geoJSON NOT AVAILABLE for an unclipped point cloud')
            else:
                print('Unclipped point cloud found - extracting plots from ' + survey +
                      ', this can take ca. 10-75 minutes depending on the size of the cloud versus your CPU.' +
                      ' Go for a walk...')
                # Load geojson to geopandas df
                geojson_main = gpd.read_file(loc_geojson + survey + '.geojson', driver="GeoJSON")
                # Specify file path to selected full point cloud
                input_pointcloud = input_pointcloud_loc + file
                # Create list of plots in the survey under consideration
                list_of_plots = list(df[df['survey_code'] == survey]['PlotID'].unique())

                # Create intermediary point cloud clipped to all harvest plots, to accelerate subsequent extraction.
                # Create list of 2D coordinates polygons for all plots
                polygeomlist = []
                for plot in list_of_plots:
                    plot_gpd = geojson_main[geojson_main["PlotID"] == plot]  # Extract coordinates of plot from geojson.
                    plot_polygon_3d = plot_gpd["geometry"].iloc[0]  # Extract coordinates from  geometry object.
                    poly_3d = shapely.wkt.loads(str(plot_polygon_3d))  # NB. poly3D contains Z values.
                    polygon_2d = Polygon([xy[0:2] for xy in list(poly_3d.exterior.coords)])
                    plot_polygon_2d = polygon_2d.wkt
                    polygeomlist.append(plot_polygon_2d)
                # define output file name
                all_plots_pc = loc_pc_all_plots + survey + "_all_plots.laz"
                # define pipeline for clipping the point cloud to all harvest plot
                clipping_pipeline_all = [
                        {
                            "type": "readers.las",
                            "filename": input_pointcloud
                        },
                        {
                            "type": "filters.crop",
                            "polygon": polygeomlist
                        },
                        {
                            "type": "writers.las",
                            "forward": "all",  # Use forward option to pass header information from input to output(s)
                            "filename": all_plots_pc
                        }
                    ]
                clipping_pipeline_all = pdal.Pipeline(json.dumps(clipping_pipeline_all))  # noqa convert pipeline into JSON format
                clipping_pipeline_all.validate()  # validate the PDAL pipeline. # NB. if this fails with
                clipping_pipeline_all.execute()  # run the PDAL pipeline
                # NB. An exception of 'NULL source SRS' here might indicate that the point cloud is in local rather
                # than absolute coordinates.

                # Function to read in *all_plots.laz and export point clouds for individual harvest plots.
                print('Finished reading entire point cloud. Subsetting point clouds for individual plots.')
                for plot in list_of_plots:
                    plot_gpd = geojson_main[geojson_main["PlotID"] == plot]  # Extract coordinates of plot from geojson.
                    plot_polygon_3d = plot_gpd["geometry"].iloc[0]  # Extract coordinates from geometry object.
                    poly_3d = shapely.wkt.loads(str(plot_polygon_3d))  # NB. poly3D contains Z values.
                    polygon_2d = Polygon([xy[0:2] for xy in list(poly_3d.exterior.coords)])  # remove z values.
                    plot_polygon_2d = polygon_2d.wkt  # convert object into well known text.
                    out_filename = loc_pc_plot + survey + '_' + plot + '.laz'  # define output file name

                    # define pipeline for clipping the point cloud to selected harvest plot
                    clipping_pipeline = [
                            {
                                "type": "readers.las",
                                "filename": all_plots_pc
                            },
                            {
                                "type": "filters.crop",
                                "polygon": [plot_polygon_2d]
                            },
                            {
                                "type": "filters.ferry",
                                "dimensions": "=> Classification"  # add classification dimension.
                            },
                            {
                                "type": "filters.assign",
                                "assignment": "Classification[:]=0"  # where 0 = 'never classified'.
                            },
                            {
                                "type": "writers.las",
                                "forward": "all",  # pass header information from input to output
                                "filename": out_filename
                            }
                        ]
                    clipping_pipeline2 = pdal.Pipeline(json.dumps(clipping_pipeline))  # noqa convert pipeline into JSON format
                    clipping_pipeline2.validate()  # validate the PDAL pipeline
                    clipping_pipeline2.execute()  # run the PDAL pipeline


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5])
