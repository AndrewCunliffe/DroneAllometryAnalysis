# DroneAllometryAnalysis

This repository contains the code and summary outputs for "Global application of a drone photogrammetry protocol for predicting aboveground biomass in non-forest ecosystems" by Cunliffe _et al_., 2022 Remote Sensing in Ecology and Conservation  8(1):57-71. DOI: 10.1002/rse2.228.

The repository is split into three parts: (1) photogrammetric processing in Agisoft PhotoScan (now Metashape), (2) processing point clouds in Python  and (3) data analysis and visualisation in R

Raw data for this study are archived with the NERC Environmental Information Data Centre: "Cunliffe, A., et al., (2020). Allometric modelling of plant biomass from drone-acquired photographs: drone images, ground control marker coordinates and biomass data from 36 sites, 2016-2020". DOI:10.5285/1ec13364-cbc6-4ab5-a147-45a103853424.


## Part 1 - photogrammetric processing in Agisoft PhotoScan (now Metashape)
_NB. This code will need to be updated to work with the updated python API in MetaShape 1.6.0!_


**1. Interactive: Collate & Prepare Datasets**
- Create project directory, add an ‘Input_Data’ subdirectory, containing:
	- ‘photos’ subdirectory.
	- correctly formatted marker coordinate file (refer to example below!)
	- Other relevant datasets (e.g. ground-based photographs, biomass harvest data sheets, elemental composition datasheets, etc.)
- Determine the EPSG code of the desired coordinate reference system, and for the supplied marker coordinates (e.g. ‘WGS84 UTM 7N / EPSG:32607”, or “NAD83 UTM 7N / EPSG:26907”. Enter this into the processing log.
- Set up ‘input_file.csv’ parameters: mapping all file paths, specifying quality settings and desired output files and resolutions.

**2. Script 1: Camera Alignment**
- Run ‘PhSc_Part1_SPC.py’

**3. Interactive**
- Review the ‘XXX_project_settings.csv’ file, to assess the proportion of aligned images, and the number of tie points excluded by the reprojection error filter.
- Consider whether it is necessary to review image quality manually (e.g. water, etc.).
- Review plausibility of sparse point cloud, and remove obvious outliers.
- Review plausibility of camera positions (Show Cameras), check for no large gaps in coverage . 
- Implement ten placements of all available markers. Place markers on the first five images (those where camera position is closest to the marker), and then another five images (ideally photographs displaying multiple markers).
- Deselect markers for independent accuracy assessment.

**4. Script 2: Dense Point Cloud**
- Run ‘PhSc_Part2_DPC.py’

**5. Interactive**
- Review plausibility of dense point cloud.

**6. Script 3: Export**
- Run ‘PhSc_Part3_Exp.py’

**7. Interactive**
- Review processing report:
	-Inspect all graphics
	-Inspect marker error values
	-Inspect mean reprojection errors
- Check all required files were generated:
	- .laz
	- Orthomosaic

## Part 2 - processing point clouds in python
- Script_0_read_df.py
- Script_1_QAQC.py
- Script_2_summarise_df.py
- Script_4_site_map.py
- Script_5_geoJSONs.py
- Script_6_DTM.py
- Script_7_orthomosaic_clip.py
- Script_8_point_cloud_clip.py
- Script_9_point_cloud_merge.py
- Script_10_HAG.py


## Part 3 - data analysis and visualisation in R




