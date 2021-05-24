# Geospatial processing script for the Drone Allometry project
# All datasets (features, point clouds, rasters, etc. are already in project-consistent coordinate reference systems.

# Import modules
import os
import sys
import pandas as pd
import geopandas as gpd
from shapely.geometry import Polygon


def main(df, loc_geojson):
    print("starting script 5 (create geoJSON)")

    list_of_projects = list(set(df['survey_code']))  # Returns a list of unique project codes.

    # Function to generate one geoJSON for each project code, containing all of the harvest plots in that project.
    for project in list_of_projects:

        # Create new data frame, subset by project code.
        project_df = pd.DataFrame(df[df['survey_code'] == project])

        # Round coordinates to 3 dp (i.e. 1 mm).
        project_df['Corner1_X'] = project_df['Corner1_X'].astype(float).round(3)
        project_df['Corner1_Y'] = project_df['Corner1_Y'].astype(float).round(3)
        project_df['Corner1_Z'] = project_df['Corner1_Z'].astype(float).round(3)
        project_df['Corner2_X'] = project_df['Corner2_X'].astype(float).round(3)
        project_df['Corner2_Y'] = project_df['Corner2_Y'].astype(float).round(3)
        project_df['Corner2_Z'] = project_df['Corner2_Z'].astype(float).round(3)
        project_df['Corner3_X'] = project_df['Corner3_X'].astype(float).round(3)
        project_df['Corner3_Y'] = project_df['Corner3_Y'].astype(float).round(3)
        project_df['Corner3_Z'] = project_df['Corner3_Z'].astype(float).round(3)
        project_df['Corner4_X'] = project_df['Corner4_X'].astype(float).round(3)
        project_df['Corner4_Y'] = project_df['Corner4_Y'].astype(float).round(3)
        project_df['Corner4_Z'] = project_df['Corner4_Z'].astype(float).round(3)

        # Subset fields to include in geoJSON properties.
        col_keep_list = ['survey_code', 'PlotID', 'EPSG',
                         'Corner1_X', 'Corner1_Y', 'Corner1_Z',
                         'Corner2_X', 'Corner2_Y', 'Corner2_Z',
                         'Corner3_X', 'Corner3_Y', 'Corner3_Z',
                         'Corner4_X', 'Corner4_Y', 'Corner4_Z']
        project_df_short = pd.DataFrame(project_df[col_keep_list])

        # Create a list of unique plot codes.
        list_of_plots = list(set(project_df_short['PlotID']))

        # Extract coordinates of each plot.
        gdflist = []
        for plot in list_of_plots:
            plot_properties_df = project_df_short[project_df_short['PlotID'] == plot]
            epsg = project_df['EPSG'].values[0]  # Extract EPSG code from database
            crs = ('EPSG:' + str(epsg))
            geometry = Polygon([(plot_properties_df['Corner1_X'].values[0],
                                 plot_properties_df['Corner1_Y'].values[0],
                                 plot_properties_df['Corner1_Z'].values[0]),
                                (plot_properties_df['Corner2_X'].values[0],
                                 plot_properties_df['Corner2_Y'].values[0],
                                 plot_properties_df['Corner2_Z'].values[0]),
                                (plot_properties_df['Corner3_X'].values[0],
                                 plot_properties_df['Corner3_Y'].values[0],
                                 plot_properties_df['Corner3_Z'].values[0]),
                                (plot_properties_df['Corner4_X'].values[0],
                                 plot_properties_df['Corner4_Y'].values[0],
                                 plot_properties_df['Corner4_Z'].values[0])
                                ])

            gdf = gpd.GeoDataFrame(plot_properties_df, crs=crs, geometry=[geometry])
            gdflist.append(gdf)

        # Convert 'gdflist' list to geopandas dataframe.
        coordinates_df = gpd.GeoDataFrame(pd.concat(gdflist, ignore_index=True))

        # Remove unnecessary fields from properties.
        coordinates_df_stripped = coordinates_df.drop(['Corner1_X', 'Corner1_Y', 'Corner1_Z',
                                                       'Corner2_X', 'Corner2_Y', 'Corner2_Z',
                                                       'Corner3_X', 'Corner3_Y', 'Corner3_Z',
                                                       'Corner4_X', 'Corner4_Y', 'Corner4_Z'],
                                                      axis=1)

        # Output a geoJSON file for each project.
        outfile = os.path.join(loc_geojson, project + ".geojson")
        coordinates_df_stripped.to_file(outfile, driver="GeoJSON")

        # Output an ESRI shapefile for each project (if required).
        # outfile = os.path.join(export_loc, project_for_processing+".shp")
        # rdf.to_file(outfile, driver="ESRI Shapefile")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
