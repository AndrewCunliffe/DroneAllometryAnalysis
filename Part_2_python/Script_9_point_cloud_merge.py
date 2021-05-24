# Geospatial processing script for the Drone Allometry project.
# Andrew Cunliffe <andrewmcunliffe@gmail.com>.
# Point cloud processing in PDAL.

# Import modules
import os
import sys
import pdal
import json


def main(loc_corner_coords_las, loc_pc_plot, loc_pc_plot_cor):
    print("starting script 9 (merge ground points into point cloud)")

    # Merge plot-level point clouds with 'clouds' of ground points.
    for file in os.listdir(loc_pc_plot):
        input_file1 = (loc_pc_plot + file)  # Specify input filename for plot point cloud
        input_file2 = (loc_corner_coords_las + file[:-4] + '_corners.las')  # Specify input filename for ground
        outfile = (loc_pc_plot_cor + file[:-4] + '_with_ground.laz')
        merge_pipeline = [
                input_file1,
                input_file2,
                {
                    "type": "filters.merge"
                },
                {
                    "type": "writers.las",
                    "filename": outfile
                }
            ]
        merge_pipeline = pdal.Pipeline(json.dumps(merge_pipeline))  # convert pipeline into JSON format
        merge_pipeline.validate()  # validate the PDAL pipeline
        merge_pipeline.execute()  # run the PDAL pipeline


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3])
