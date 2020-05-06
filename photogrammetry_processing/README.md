# PhotoScan_Auto_Workflow
An automated workflow for processing drone imagery in Agisoft PhotoScan

Developed by Hugh Graham and Andrew Cunliffe

## NB. This code will need to be updated to work with the updated python API in Agisoft MetaShape >1.6.0!


**1. Interactive: Collate & Prepare Datasets**
- Create project directory (e.g. ‘20180531_AC_SEG’).
- Within the project directory, add an ‘input_data’ subdirectory, containing:
	- ‘aerial_photos’ subdirectory.
	- correctly formatted marker coordinate file (refer to example below!)
- Determine the EPSG code of the desired coordinate reference system, and for the supplied marker coordinates (e.g. ‘WGS84 UTM 7N / EPSG:32607”, or “NAD83 UTM 7N / EPSG:26907”.
- Review and specify ‘input_file.csv’ parameters: mapping all file paths, specifying quality settings and desired output files and resolutions.

**2. Script 1: Camera Alignment**
- Run ‘PhSc_Part1_SPC.py’

**3. Interactive**
- Review Console output for error and/or warning messages 
- Review the ‘XXX_project_settings.csv’ file, to assess the proportion of aligned images, and the number of tie points excluded by the reprojection error filter.
- Consider whether it is necessary to review image quality manually (e.g. water, etc.).
- Review plausibility of sparse point cloud, and remove obvious outliers.
- Review plausibility of camera positions (Show Cameras), check for no large gaps in coverage . 
- Implement ten placements of all available markers. Place markers on the first five images (those where camera position is closest to the marker), and then another five images (ideally photographs displaying multiple markers).
- Deselect markers for independent accuracy assessment.

**4. Script 2: Dense Point Cloud**
- Run ‘PhSc_Part2_DPC.py’

**5. Interactive**
- Review console output for errors.
- Review plausibility of dense point cloud (& remove obvious outliers).

**6. Script 3: Export**
- Run ‘PhSc_Part3_Exp.py’ 

**7. Interactive**
- Review console output for errors.
- Review processing report:
	-Inspect all graphics
	-Inspect marker error values
	-Inspect mean reprojection errors
- Check all required files were generated:
	-	.laz
	- Orthomosaic
- Download/backup data.
