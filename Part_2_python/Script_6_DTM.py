# Geospatial processing script for the Drone Allometry project
# This script takes in corner coordinates, and outputs DTMs and .las for each individual harvest plot.

# Import modules
import os
import sys
import json
import pdal
# from raster2xyz.raster2xyz import Raster2xyz
# import pandas as pd
# from rasterio.crs import CRS


def main(df, loc_corner_coords, loc_corner_coords_las, loc_plot_dtm):
    print("starting script 6 (DTM)")
    # Create new .txt file of corner coordinates for each individual harvest plot.
    # NB. the classification = 2 denotes 'ground' in the las format.
    for index, row in df.iterrows():
        # Specify outfile name
        outfile = os.path.join(loc_corner_coords, row['survey_code'] + '_' + row['PlotID'] + '_corners.txt')
        # Extract EPSG code
        epsg = ("EPSG:" + row['EPSG'])
        # print(epsg)
        # extract corner coordinates
        c1 = (str(row['Corner1_X']), str(row['Corner1_Y']), str(row['Corner1_Z']), str(2))
        c2 = (str(row['Corner2_X']), str(row['Corner2_Y']), str(row['Corner2_Z']), str(2))
        c3 = (str(row['Corner3_X']), str(row['Corner3_Y']), str(row['Corner3_Z']), str(2))
        c4 = (str(row['Corner4_X']), str(row['Corner4_Y']), str(row['Corner4_Z']), str(2))
        # write 'point-cloud' to file
        outf = open(outfile, "w+")
        outf.write(epsg + '\n')
        outf.write('X,Y,Z,Classification' + '\n')
        outf.write(", ".join(c1) + '\n')
        outf.write(", ".join(c2) + '\n')
        outf.write(", ".join(c3) + '\n')
        outf.write(", ".join(c4) + '\n')
        outf.close()

    # Create DTM rasters for individual harvest plots
    for file in os.listdir(loc_corner_coords):
        input_file = (loc_corner_coords + file)  # Specify input filename
        epsg = open(input_file).readline()  # EPSG code
        dtm_outfile = os.path.join(loc_plot_dtm, file[:-12] + '_DTM.tif')  # Specify outfile, excluding 12 characters.
        # Define PDAL pipeline for DTM generation. To interpolate a continuous surface across the entire plot,
        # the window_size parameter must be high enough (window_size is a multiplier on the resolution parameter).
        dtm_pipeline = [
                    {
                        "type": "readers.text",
                        "filename": input_file,
                        "skip": 1,
                        "spatialreference": epsg
                    },
                    {
                        "type": "writers.gdal",
                        "dimension": "Z",
                        "output_type": "idw",  # specify inverse distance weighting
                        "power": 2,  # specify the power term used in the IDW
                        "resolution": 0.05,  # spatial resolution of grid, in m.
                        "radius": 0.05,  # radius in which to search for other points, in m.
                        "window_size": 500,  # Nonzero value enables secondary interpolation to reduce nodata holes.
                        "gdaldriver": "GTiff",
                        "filename": dtm_outfile
                    }
                ]
        dtm_pipeline = pdal.Pipeline(json.dumps(dtm_pipeline))  # convert pipeline into JSON format
        dtm_pipeline.validate()  # validate the PDAL pipeline
        dtm_pipeline.execute()  # run the PDAL pipeline

    # Convert corner coordinates from .txt into .las.
    for file in os.listdir(loc_corner_coords):
        input_file = loc_corner_coords + file  # Specify input filename
        epsg = open(input_file).readline()  # Read EPSG code from the first line of the file
        outfile = os.path.join(loc_corner_coords_las, file[:-12] + '_corners.las')  # Specify outfile
        conversion_pipeline = [
                {
                    "type": "readers.text",
                    "filename": input_file,
                    "skip": 1,
                    "spatialreference": epsg
                },
                {
                    "type": "writers.las",
                    "filename": outfile
                }
            ]
        conversion_pipeline = pdal.Pipeline(json.dumps(conversion_pipeline))  # convert pipeline into JSON format
        conversion_pipeline.validate()  # validate the PDAL pipeline
        conversion_pipeline.execute()  # run the PDAL pipeline


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4])
