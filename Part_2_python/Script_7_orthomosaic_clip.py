# Geospatial processing script for the Drone Allometry project
# All datasets (features, point clouds, rasters, etc. are already in project-consistent coordinate reference systems.

# Import modules
import os
import sys
import rasterio
import rasterio.mask
import rasterio.warp
import fiona
# from rasterio.plot import show  # enable if show is required.


def main(input__orthomosaic_loc, loc_geojson, loc_plot_orthophotos):
    print("starting script 7 (orthomosaic)")
    # Create list of processed (clipped) project codes
    list_of_processed_project_codes = []
    for file in os.listdir(loc_plot_orthophotos):
        project_code = file[:15]
        list_of_processed_project_codes.append(project_code)
    list_of_processed_project_codes = list(set(list_of_processed_project_codes))  # Returns a list of unique codes.

    for file in os.listdir(input__orthomosaic_loc):
        project_code = file[:15]  # Define file paths.
        input_raster = (input__orthomosaic_loc + file)  # Define file paths.
        input_geojson = (loc_geojson + file[:15] + '.geojson')  # Define file paths.

        # check if this point cloud has been already been processed
        if project_code not in list_of_processed_project_codes:
            # Load geoJSON, iterate through applying clipping function, save outputs.
            with fiona.open(input_geojson, "r") as json:
                plots = [feature["geometry"] for feature in json]
                items = [feature['properties']['PlotID'] for feature in json]
                merged_list = [(plots[i], items[i]) for i in range(0, len(plots))]
            for plot, item in merged_list:
                with rasterio.open(input_raster) as src:
                    out_image, out_transform = rasterio.mask.mask(src, [plot], crop=True)
                    out_meta = src.meta
                out_meta.update({"driver": "GTiff",
                                 "height": out_image.shape[1],
                                 "width": out_image.shape[2],
                                 "transform": out_transform})
                with rasterio.open(loc_plot_orthophotos + project_code + "_" + item + ".tif", "w", **out_meta) as dest:
                    dest.write(out_image)

            # function to load and display orthomosaic
            # orthomosaic = rasterio.open(input_raster)
            # show(input_raster)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3])
