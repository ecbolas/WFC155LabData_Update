#### Welcome to your eighth lab for WFC 155! ####
## Lab 8: Downloading and processing open source data ##

## Lab 8 OBJECTIVES ##
# 1. Learn to download and process open source animal location data from Movebank
# 2. Learn to download and process DEMs and NLCD data

# We need a lot of new packages today! You can use this shortcut to see which packages 
#   that you need are not already on your machine, then install them
packages_needed <- c("maptools","move", "amt","randomcoloR","here",
                     "devtools","elevatr","rgdal")
new_packages <- packages_needed[!(packages_needed %in% 
                                    installed.packages()[,"Package"])]
if(length(new_packages)) 
  install.packages(new_packages, repos = "https://cloud.r-project.org")

# Now let's load our many packages for today
library(lubridate)
library(maptools)
library(move)
library(amt) 
library(tidyverse)
library(ggplot2)
library(sf)
library(raster)
library(rasterVis)
library(randomcoloR)
library(here)
library(elevatr)
library(rgdal)
library(here)

## ------------------------------##
#### DOWNLOADING MOVEBANK DATA ####
## ------------------------------##

# We'll start today by downloading a dataset from Movebank. 
# Remember the Utah pumas we looked at in lab 6? Today, you'll be able to download that dataset
#   directly from MoveBank
# But first, you need a Movebank login! So open a browser and go to the following address:
#   https://www.movebank.org/cms/movebank-main
# Select "Login/Register" and make a personal login

# Now you can login directly through RStudio:
loginStored <- movebankLogin(username="yourusername", password="yourpassword")

# Next, we need to find a study we want to explore in R
# To do that, go back to the Movebank website and navigate to Data -> Maps from the menu
#   You should see a map! Click the box in the top left that says "Only studies where I can 
#     see data" and click "Search"
#   These are the studies that have data available for public use
# We know the study is in Utah, so type in "Utah" and Search again
# Now click the box next to "Site fidelity in cougars and coyotes". You should be able to see
#   some movement data in Utah and Idaho if you zoom in.
# Next, click the box with an "i" in it next to the study name and select "Open in studies page"
# Here is all the information you need to load this dataset!
#   Find the "Movebank ID" and enter it in the command below next to "study = "

getMovebankStudy(study = , login = loginStored) 

# There's all the study info! Including citation for data, species IDs, start and end date 
#   of study, etc
# Now we can bring the data into RStudio. Again, enter the study number in the command below

utah_move <- getMovebankData(study = , login = loginStored)

# Dang! Didn't work? Go back to the Movebank study page, click "Download data", and agree to the
#   terms and conditions. Once you've agreed, don't proceed to download the data from your
#   browser. Just come back here and try the above function again.
# Because most Movebank datasets want you to agree to terms of use, you will often have to 
#   do this step when importing Movebank data in R

# Always check out the data first before proceeding
str()
# What is the class of data? 

# Let's transform the data from a MoveStack object into a basic dataframe
utah_dat <- as(utah_move, "data.frame")

head(utah_dat)
# What information is here? What is missing that you might want? What is extraneous?

# Your turn! 
# We can use the select function to pull out just the variables we are planning on using,
#   like tag_local_identifier (which identifies the animal), latitude, longitude, and timestamp
# Use "select" to create a new "utah_dat" dataframe with only those four columns


# Check out your new dataframe
head(utah_dat)

# Want to know how many animals are in the dataset? Or how many locations there are per animal?
#   The top values are the tag IDs and the bottom numbers are the number of locations:
table(utah_dat$tag_local_identifier)

# We still don't know which tags are associated with which species or other variables like sex and age
# To get that info, we need to download one more file - the reference data
# Again, enter your study ID in the script below:
utah_ref <- getMovebankReferenceTable(study = , 
                          login = loginStored, 
                          allAttributes = FALSE)

head(utah_ref)

# Notice anything weird about the data? This dataframe should just tell us which tags are associated
#   with a particular species, sex, age, study area, etc.
# We have duplicate rows per tag. That's because some animals had multiple collars over the 
#   course of the study.
# Since we don't need to worry about the collar IDs, we can simplify our reference dataframe
#   by saving only rows that have a distinct animal
utah_ref <- utah_ref %>% distinct(tag_local_identifier, .keep_all= TRUE)


# Your turn!
# Just like we did with the location dataframe, let's pull out just the columns that might
#   be of interest to us. This time, make a new version of utah_ref with just the columns
#   for tag_local_identifier, sex, taxon, life stage, reproductive contition, and study site

head(utah_ref)
# Do you notice anything incomplete about the available data?
# It looks like if we are interested in reproduction, life stage, or sex, we only have those
#   data for a subset of animals

# This study is an odd one because there are two different study areas! How many of these 
#   animals are in Idaho vs. Utah?
utah_ref %>% group_by(study_site) %>% 
  summarise(n())

#** QUESTION 1: It is important to make sure you know which study site an animal is 
#   from before performing a habitat selection analysis. If a habitat type is common in one site
#   and rare in another, how might habitat selection by the same species differ between the sites?

# Ok, now we have our location data and our reference data! We can use our join function
#   to bring them together
utah_dat_complete <- inner_join(utah_dat, utah_ref, by = "tag_local_identifier") 

head(utah_dat_complete)

# What else do we need to finish processing our data?
#   We need to make sure the time zone is correctly formatted. Otherwise, if we end up comparing
#   space use between day and night, we will have the times all wrong!

# Our study takes place in the US Mountain time zone. Therefore, we need to find the code for 
#   that time zone. You can look up time zone codes here:
#     https://docs.trifacta.com/display/DP/Supported+Time+Zone+Values
#     Scroll down to the table called "Global Time Zone Values"
#     Our code will be "US/Mountain". For your final projects, you may need to go back to the
#       site to find the correct code for your chosen study.
# Below is some code to turn your timestamp column into a time object (ymd_hms) and to assign
#   the local time its correct zome zone (force_tz)
utah_dat_complete %>% 
  mutate(timestamp = force_tz(ymd_hms(timestamp),"US/Mountain")) -> utah_dat_complete

# For your final projects, you may want to subset your data to only look at specific 
#   subsets of the population or to isolate individual animals.

# Your turn!
# Let's take a look at our data by study site using the "filter" function
#   Make sure to include the full study site names in your functions.
# Make one new dataframe called "INEL_data" with only the Idaho animals, and one called 
#   "FNF_data" with only the Utah animals

# We can plot the location data for each dataset using the following ggplot commands:
ggplot(INEL_data, aes(location_long, location_lat, color = tag_local_identifier, 
           group = tag_local_identifier))+
  geom_point() + 
  coord_equal() +
  theme(legend.position = "bottom")
ggplot(FNF_data, aes(location_long, location_lat, color = tag_local_identifier, 
           group = tag_local_identifier))+
  geom_point() + 
  coord_equal() +
  theme(legend.position = "bottom")

# Finally, you might want to compare how animals use space between day and night
# To do this, we can make our data into a "track", and then use the time_of_day() function to 
#   create a day/night column
# Let's use only the INEL data:
INEL_track <- make_track(INEL_data, .x = location_long, .y = location_lat, 
                  .t = timestamp, id = tag_local_identifier, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
INEL_track <- INEL_track %>% time_of_day()

# Then we turn it back into a dataframe
INEL_track <- as(INEL_track, "data.frame")

# And use the join function to bring that column into our INEL dataframe
INEL_data <- inner_join(INEL_data, INEL_track, by = c("tag_local_identifier"="id","timestamp"="t_"))

# Do we have a time of day column now?
head(INEL_data)

#** QUESTION 2: Make two dataframes with the INEL dataframe, one for day points and one for night
#     Plot the locations for day and night separately and export the plots for your lab report.


## -------------------------------------##
#### DOWNLOADING ENVIRONMENTAL LAYERS ####
## -------------------------------------##

# Whew, we have our animal movement data all set up! But we need environmental layers to say
#   anything about habitat use and selection.
# We will need to jump back and forth between latlongs and UTMs today, so let's start by 
#   saving both coordinate reference systems for later

utah_CRS_latlong <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
utah_CRS_utm <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"

# We also need to create a spatial extent that we want to use for compiling layers
# That's because it is intensive (time, processing power, storage space) to download all the available
#   environmental data - sometimes datasets are available for the whole world! Or they are
#   at high resolution, which also eats up a lot of storage space and time to process.
# For now, we're only going to extract data for animal F53. In your own project, make sure
#   you have a big enough area defined to encompass all the animals you intend to analyze.

# Our data are in latlongs, but we want them in UTMs. So we use st_as_sf to define the current
#   projection and st_transform to change the projection. 
# Fill in the correct crs names from above in the two empty crs commands in the pipe below
utah_dat_complete %>% 
  filter(tag_local_identifier == "F53") %>%  
  st_as_sf(coords = c("location_long","location_lat"),crs = ) %>% 
  st_transform(.,crs = )-> utah_F53_utm

# Now our points are in the right projection! But we also need to define an extent if we are
#   going to download raster files
# The below function lets us save a box around all the location data in our sf object for F53
F53_box <- st_as_sfc(st_bbox(utah_F53_utm))

# Is our box around all of F53's locations?
plot(F53_box)
plot(st_geometry(utah_F53_utm), add=T)

# Let's think about this though. How do we normally assess habitat selection? By sampling
#   available points in a home range.
# If we use MCP home ranges, that home range will sit within our box. But if we use KDE home
#   ranges, it will be too big for our box! So we should define an extent that's bigger.
# We can do that by creating a buffer around our bounding box.
#   Because we're in UTMs, the unit is in meters. How many meters should we put around our box?
# For now, we'll try 5 km, or 5000 m. You might need a larger buffer for your own project.
F53_buffer <- st_buffer(F53_box, 5000)

# Is our new box larger?
plot(F53_buffer, col = "blue")
plot(F53_box, add=T, col = "yellow")
plot(st_geometry(utah_F53_utm), add=T)

# Now we are ready to download some environmental variables within our study extent.

#### Elevation ####

# With the elevatr package, we can get elevation data in two ways. We can directly extract
#   elevation values at points, or we can download a whole raster
# If you are doing a habitat selection analysis and you want to just extract the values
#   at points, you will want to create your full dataset of used and available locations first.
# Then, extracting elevation can be done in only one line of code! 
#   (It takes a little while. While you're waiting, skip to line ___ to start your request for
#     your NLCD data.)
utah_F53_elev_points <- get_elev_point(utah_F53_utm, prj = utah_CRS_utm)

# How did our dataframe change?
head(utah_F53_elev_points)

#** QUESTION 3: Make a histogram of the elevation data at F53's points. At which elevation does
#     F53 spend the most time?

# If you want to extract other variables from the DEM, like slope or aspect, or if you want to 
#   create a map, it's better just to download the raster.
# To do so, we need to make our F53_buffer an sp object, because that's what the elevatr package uses.
F53_buffer_sp <- as_Spatial(F53_buffer)

# Then, again we only need one line of code to download the raster. Awesome!
utah_F53_DEM_raster <- get_elev_raster(locations = F53_buffer_sp, z = 9, clip = "locations")

plot(utah_F53_DEM_raster)
plot(F53_buffer, add = T)
plot(F53_box, add=T)
plot(st_geometry(utah_F53_utm), add=T)

# Your turn!
## QUESTION 4: Make a new slope raster from your DEM raster. (Hint, use the terrain function
#     we used in Lab 4.) Plot the buffer box, the original bounding box, and F53's locations
#     over the slope raster. Does it seem like F53 selects for flatter or steeper slopes? You may
#     want to use the "zoom" button in the plots quadrant to get a closer look at your map.
#   Export the plot for your lab report.


#### National Land Cover Database (NLCD) ####

# Lastly, we'll go over downloading NLCD data like you used in Lab 6.
# Unfortunately, we need to download NLCD data from a web browser rather than in R. Bummer!
# Start by navigating to https://www.mrlc.gov/viewer
# Check the box on the left for the data type you want. Since our data are from 2013, check
#   "2013 CONUS Land Cover" and un-check everything else.
# Find the approximate location of our latlongs for our F53 dataset:
utah_dat_complete %>% 
  filter(tag_local_identifier == "F53") %>%  
  st_as_sf(coords = c("location_long","location_lat"),crs = utah_CRS_latlong) -> utah_dat_latlong
st_bbox(utah_dat_latlong)

# Zoom in to that area. You can see the coordinates of your mouse in the bottom left of the map
# Draw a box around the area you want to download by pressing the downward-facing arrow from 
#   the top toolbar above the map. Use the values from the bounding box as a guide. 
#   Remember to give some wiggle room around the extent of your points!
# Under "Data Download" on the right, click "Land Cover" and adjust the coordinates if you want.
#   Enter your email and click "Download"
# You may get an email right away with the data. It could also take up to 24 hours. I've included
#   the file I downloaded in case you don't get yours in time do analyze it in lab.

# Read in the raster. You will change the name of this file for your own project
NLCD_Utah <- raster("NLCD_2013_Land_Cover_L48_20210604_Hr9OYDRCRL0eKOlaS7zu.tiff")

# Project the raster to be in our UTM crs
NLCD_Utah <- projectRaster(NLCD_Utah, crs = utah_CRS_utm, method = "ngb")

# What do you notice about our NLCD raster?
plot(NLCD_Utah)

# It's way bigger than our extent! That's because when we define the area we need, the MRLC
#   website actually downloads the whole tile that our study area falls within. So, we need 
#   to crop it to our area.

# Unfortunately we need a different file type to define our study extent with raster data,
#   so we'll just put a buffer around our whole F53 movement dataset. Let's keep the 5km buffer size.
F53_buffer_2 <- st_buffer(utah_F53_utm,5000)

# Time to crop!
NLCD_Utah <- crop(NLCD_Utah,extent(F53_buffer_2))

# Check it out again. Now we're back to the buffered extent of our data.
plot(NLCD_Utah)

# Before we can extract habitat values, we probably want to know what all these colors mean.
# For starters, we can check out the raster values within our layer
table(NLCD_Utah@data@values)

# What are all these numbers?? We need to name the values ourselves.
# To do this, we have to write out which habitat names correspond with the values in our raster

# We can pull out the numbers of each habitat class in our raster:
level_numbers <- sort(unique(NLCD_Utah@data@values))
level_numbers

# Now we need to match the numbers to names. To do that, go to: 
#   https://www.mrlc.gov/data/legends/national-land-cover-database-2019-nlcd2019-legend
# Type out the names, in order, that match your level numbers. We don't have numbers 12,51,
#   72,73, or 74 in our raster, so we'll just name the other categories that we do have.
nlcdclass <- c("Open Water", "Developed, Open Space", "Developed, Low Intensity",
               "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land",
               "Deciduous Forest","Evergreen Forest","Mixed Forest","Shrub/Scrub","Herbaceuous",
               "Hay/Pasture","Cultivated Crops","Woody Wetlands","Emergent Herbaceuous Wetlands")

# Next, we make a dataframe matching the level numbers to the NLCD classes
NLCD <- data.frame(ID = level_numbers, landcover = nlcdclass)

# And finally, we assign the levels to our raster
NLCD_Utah <- subs(NLCD_Utah, NLCD)
levels(NLCD_Utah)

# Lastly, we can automatically create the number of colors we need to represent all 
#   the different classes using the randomcoloR package:
palette <- unname(distinctColorPalette(15))

# And finally, we can plot our map! 
levelplot(NLCD_Utah, col.regions=palette, margin = FALSE)

#** QUESTION 5: Make an observation about the distribution of habitat types within our
#     study area. Export the plot for your lab report. 


## This lab is designed to be finished a little early so you can practice downloading other
#   datasets. Next week, you will be downloading data for your own independent project, so
#   you want to make sure that you've got it! I highly encourage you to stick around and 
#   try to go through this process on your own.


## Want to try to find your own environmental data to download? Check out the following resources:
#   USGS Earth Explorer: https://earthexplorer.usgs.gov/
#   rspatialdata collection: https://rspatialdata.github.io/index.html

#-------------------------------------------#
#### QUESTIONS FROM LECTURE AND READINGS ####
#-------------------------------------------#

#** QUESTION 6: Give one example of how habitat can influence carnivore-livestock conflict.
#
#** QUESTION 7: Did Middleton and Brashares (2020) suggest that a "carrot" or "stick" approach 
#     would be most effective to achieve the 30 by 30 goals? What specific conservation
#     method did they advocate?

#--------------#
#### CONGRATS! ####
# You've learned how download and process spatial data in R!
# Please reach out to your TA, Julianne Pekny (jpekny@ucdavis.edu) for questions about this lab