#use latitude/longitude to extract 30-year climate normals from PRISM data products (1981-2010).
#PRISM products are 800m resolution. 

require(raster)
require(rgdal)

#load data that contains latitude and longitude coordinates.
trees.soils <- read.csv('analysis_data/Trees.Soils.csv')

#30 year normal rainfall data from from PRISM
#NOTE: When loading .bil files, the corresponding .hdr needs to be in the same directory. 
#NOTE: the filenames require the format: "cool_file.bil" and "cool_file.hdr"
map.PRISM <- raster('required_products_utilities/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil')
mat.PRISM <- raster('required_products_utilities/PRISM_tmean_30yr_normal_800mM2_annual_bil.bil')


#extract data from climate layers
points<- cbind(trees.soils$longitude,trees.soils$latitude)
trees.soils$map   <- extract(map.PRISM, points)
trees.soils$mat   <- extract(mat.PRISM, points)


#write file output
write.csv(trees.soils, file='analysis_data/Trees.Soils.csv')