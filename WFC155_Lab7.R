#### Welcome to your seventh lab for WFC 155! ####
## Lab 7: Behavioral states and habitat selection ##

# Today we are going to work through some new methods to identify behavioral states from 
#   animal movement data
# We will also combine the skills you've learned up to this point to look at how animals 
#    select habitat differently based on their behavioral state

# NOTE on the data we will be using today (accessed from MoveBank, an open source animal movement repository):
# Study License Information valid at 2020-11-13 20:45:45 Name: Site fidelity in cougars and coyotes, Utah/Idaho USA (data from Mahoney et al. 2016) 
# Citation: Mahoney PJ, Young JK (2016) Uncovering behavioural states from animal activity and site fidelity patterns. Methods in Ecology and Evolution 8(2): 174–183. doi:10.1111/2041-210X.12658 
# Acknowledgements: We would like to thank M. Ebinger and M. Jaeger for access to the Idaho coyote dataset (all animal IDs beginning with 'IdCoy_'). 
# Grants Used: Utah Division of Wildlife Resources and USDA-WS-NWRC Predator Research Center License Type: Custom License Terms: These data have been published by the Movebank Data Repository with DOI 10.5441/001/1.7d8301h2. 
# Principal Investigator Name: Julie Young, PhD

## Lab 7 OBJECTIVES ##
# 1. Learn how to separate animal behavioral states using hidden Markov models (HMMs)
# 2. Merge your understanding of animal movement parameters, home ranges, and habitat selection
#     to investigate how animals select habitat differently by behavioral state

# New packages for today if outside of lab
#install.packages("moveHMM", dependencies = TRUE)

# Load in your packages 
library(tidyverse)
library(raster)
library(sf)
library(sp)
library(adehabitatHR)
library(ggplot2)
library(rasterVis)
library(lubridate)
library(moveHMM)

# Like usual, make sure you set your working directory to the folder where you 
#   downloaded the files for lab this week
setwd("/Users/justinesmith/WFC155/WFC155_labs")

#-----------------------------------#
#### LOADING THE DATA ####

# We're going to use the same data from last lab, with the exception of adding an elevation,
#   or DEM layer for the study area as well.
# Let's start by loading in our DEM and NLCD rasters, as well as our land cover key from last time
utah_DEM <- raster("DEM_Utah.tif")
utah_NLCD <- raster("utah_NLCD.tif")
landcoverkey <- read_csv("landcoverkey.csv")

# You've already checked out the NLCD layer, but let's take a look at our DEM
crs(utah_DEM)
levelplot(utah_DEM, margin = FALSE)

# Today we will only be analyzing puma F53's data.
# Your turn!
# Modify the below code to only store the data from puma F53 (column name: "individual")
read_csv("Utah_carnivores.csv") %>% 
  mutate(timestamp = with_tz(timestampUTC,tz = "us/mountain")) -> puma_F53

#-----------------------------------#
#### DIFFERENTIATING BEHAVIORAL STATES ####

# Remember how we had to swich back and forth between sf and sp before? Now, we have to do
#   a similar thing to analyze behavioral states by switching from a tibble to a dataframe
# Our package that identifies behaviors from animal movement data doesn't like tibbles, so 
#   we have to turn our tibble into just a dataframe using as.data.frame()
puma_F53.df <- as.data.frame(puma_F53)
class(puma_F53)
class(puma_F53.df)

# In Lab 3, we used the amt package to calculate step lengths and turn angles
# The moveHMM package can calculate those parameters too, and they store them in a specific
#   class that the package uses to reference them (so we have to calculate them using 
#   prepData(), the designated command in the moveHMM package)
# All we need to tell prepData is the name of our dataframe, if we are using UTMs or latlongs,
#   and the names of our coordinates columns
F53_prep <- prepData(puma_F53.df,type="UTM",coordNames=c("UTMeasting","UTMnorthing"))

# What new class has been assigned to our new object? This is a class specific to moveHMM
class(F53_prep)

# Let's take a look at the data. What two movement parameters have been added to our dataframe?
head(F53_prep)

# prepData() also creates some plots so we can see visualizations of our movement parameters
# There are two plots that come up in sequence, so make sure you follow the instructions in 
#   the console to see the next plot
# The first plot shows us our animal track. The second has four panels: the step length and
#   turn angles over time (top), and the distribution (histogram) of step lengths and turn
#   angles within the entire dataset (bottom)
plot(F53_prep,compact=T)

# Now, we need to get into the weeds a bit. Think back to our discussion of state space models
#   in Module 1, lecture 6. State space models predict an animal's behavioral state from
#   movement parameters, like step length and turn angle. 
#     Step length tells us the speed and distance traveled between two consecutive animal 
#     locations. Turn angle tells us if the animal is moving straight in one direction, or
#     if it is turning more often.
# When we predict two simple behavioral states from animal movement data, we generally are 
#   interested in differentiating directed travel (also known as exploratory or moving) from
#   resting (also known as encamped or stationary). We had an example of this kind of analysis
#   by Dr. Juan Morales looking at elk behaviors in Lecture 6.
# A hidden Markov model (HMM) is a specific kind of state space model (SSM) that identifies
#   discrete states (like behaviors), rather than modeling a continuous process. So, even
#   though today you are being introduced to a new term (HMM), we are ultimately implementing
#   a similar approach to the SSMs we discussed in class
# Unfortunately, we don't have time to get into the details of the statistics of the model.
#   Therefore, we'll just walk through the parameters together and instead focus on the 
#   interpretation at the end.

# First off, we have to give the HMM a starting point to fit the model. Basically, what this
#   means is that we tell is what we think the mean step lengths and turn angles will be
#   for our two behavioral states. We also give it an approximate variation around the mean
#   for each state.
# You should still have a plot up showing our distribution of step lengths and turn angles.
#   based on these distributions and what we assume about our two behavioral states (traveling
#   and resting), what do you think our means might be for each one's step length and turn angle?
# Well, we want "resting" to have a step length close to zero. And it looks like a mean step
#   length for longer steps might fall around 1000 m. So we can start by setting the mean and
#   standard deviation starting points for our behavioral states at 100 m and 1000 m. Then
#   we combine the mean and SD into one object of our step length parameters:
mu0 <- c(100,1000) 
sigma0 <- c(100,1000) 
step_parameters <- c(mu0,sigma0)

# Next we do the same thing for turn angles. For a traveling animal, we should assume that 
#   the mean turn angle should be close to 0 (not very much turning). Resting animals should
#   have more variable turn angles, so we can use a starting mean of pi (see bottom right
#   panel of your last plot). The concentration around each turn angle means can be set to 
#   start at 1. Then as above, we make a single object of our angle parameters.
angleMean0 <- c(pi,0) 
kappa0 <- c(1,1) 
angle_parameters <- c(angleMean0,kappa0)

# Now we're ready to fit our model! We enter in our data from prepData(), the number of states
#   we'd like to differentiate (2), our step and angle parameters, and a generic "formula".
#   (If we wanted to, we could include an environmental variable in the formula command, but
#     for now we'll keep it simple and just use the step lengths and turn angles to fit
#     our model)
F53_behav <- fitHMM(data = F53_prep, nbStates = 2, stepPar0 = step_parameters,
                    anglePar0 = angle_parameters, formula = ~1)

# Now we have a moveHMM object, complete with behavioral states for each location!
class(F53_behav)

# We started with guesses for the mean parameters for each state, but now we can see what
#   the actual parameters ended up being:
F53_behav$mle$stepPar
F53_behav$mle$anglePar

# It seems like our initial estimates were pretty close to the final parameters. What would 
#   happen in we changed the initial values?
# Your turn! 
#   Change the initial estimated parameter values for step length to see if they affect the 
#     final step length parameter estimates. Use whatever values you want, as long as they 
#     are within the range of step lengths in the dataset.
#** QUESTION 1: How much did the mean parameter values for step length change when you used
#     different initial values? What does that tell you about the sensitivity of the model
#     to the initial values? Or to put it another way, does changing the starting point 
#     strongly affect the assignment of behavioral states? In your answer, please include
#     what you changed the starting parameters to.

# Let's go back to our original model.
# Like our last plot, this next one has many plots embedded in it.
# The first one shows the distribution of step lengths for each state, overlaid on the histogram
#   of all the location data combined. The second one is the same, but for turn angles
# The last one shows the distribution of the different behaviors: traveling in blue and
#   resting in orange.
plot(F53_behav)

#** QUESTION 2: We expected that the traveling state would have turn angles strongly concentrated
#     around zero. Looking at the second plot above, do you think our results support that
#     assumption about the traveling state? Provide an explanation for why or why not you 
#     thing this assumption was supported.
# Export the plot for your lab report.

# Back in lecture 6, we talked a lot about how behavioral state can inform habitat selection
#   Now, we actually have the opportunity to look at how habitat selection varies by 
#   behavioral state! 
# First, we can isolate just the states themselves using viterbi(). This makes our states into
#   a vector rather than a moveHMM object
F53_states<-viterbi(F53_behav)

# Take a look at the data:
class(F53_states)
head(F53_states)

# Since the states are simply a vector now, we can add them back into our original tibble
#   as a column in our data
# Since our vector is describing two discrete states (yet is currently in the form of numbers
#   as state "1" and "2"), we can use "recode()" to rename our behavioral states
F53_states <- recode(F53_states, "1" = "resting", "2" = "traveling")

# Let's also flip the levels so the resting points are plotted on top of the traveling
#   points, which will help us see them better
F53_states <- fct_relevel(F53_states,c("traveling","resting"))

# Once that's done our behavioral data are ready to be added to our puma_F53 tibble!
# Your turn!
#   Use mutate() to make a new column called behav_state with our F53_states vector
# Do you see the new column in your tibble?
view(puma_F53)

# Next, turn puma_F53 into an sf object called puma_F53_sf.
# To do this, first save an object of the crs of the puma GPS data called myCRSutm
#   The crs should be in UTMs, zone 12, datum WGS84 (hint: you can look at the crs from
#   either of our rasters to get the crs arguments)
# Make sure to set the coordinates using the UTM columns rather than latlongs
# As always, check your data!
class(puma_F53_sf)
puma_F53_sf

# Now, plot puma F53's points using ggplot, geom_sf, and coord_sf (hint: revisit Lab 3 if
#   you can't remember how to plot spatial points in ggplot)
# Make the behavioral states different colors (hint: color is a command within "aes()")
#** QUESTION 3: Do you see any general areas that seem to be used for traveling but not 
#     resting? If so, describe where those locations are within the home range. What might 
#     be one reason that this puma would move through those areas but not rest there?
#   Export the plot for your lab report.

# It looks like we have way, way more movement points than resting points. However, that might
#   be a feature of how the behavioral states map spatially. Resting points are often on
#   top of each other, so it looks like there is only one location where there are many.
#     (Think of it this way: if you recorded your location every 2 hours, would there be 
#     multiple locations in exactly the same spot?)
# We can check how many locations we actually have for each behavior using our tally() function:
puma_F53_sf %>% 
  group_by(behav_state) %>% 
  tally()

# It turns out that we actually only have about double the number of traveling points as
#   resting points

#----------------------------------------------------------#
#### INFLUENCE OF BEHAVIORAL STATE ON HABITAT SELECTION ####

# By this point, you all know how to calculate home ranges, simulate avaialble locations 
#   within a home range, and do a habitat selection analysis using selection ratios.
# Since this is our last lab, it's your turn to do the whole thing yourself!
# Perform a habitat selection analysis separately for each behavioral state (resting and
#   traveling) and create a plot of the selection ratios for each behavioral state.
# Make sure to sample available points from the 95% KUD home range of puma F53.
# Use the NLCD raster for the selection ratio analysis, and sample the name number of available
#   points as used points (just like last week). This will mean you have more available points
#   in the traveling dataset than in the resting dataset. 

# For this analysis, make sure to set the following seed before you sample your available 
#   points (so your results match mine)
set.seed(134)

#**QUESTION 4: Are any habitat types selected for in one behavioral state that are not selected
#   for in the other? How does the selection for Developed habitats differ between the behavioral
#   states? Provide an explanation for why you think the relationship to developed habitats
#   differs between resting and traveling states.
# Export both plots for your lab report

# Lastly, we will look at how selection for a continuous habitat variable differs between
#   behavioral states.

# Use the terrain() function to create a raster for slope from our utah_DEM raster 
#   (Hint: see lab 4 for reference)
# Next, use raster::extract (like you did for the NLCD above) to extract slope values for
#   both your resting and traveling datasets (with both used and available locations)
# I've named my two datasets puma_travel_habitat and puma_rest_habitat. If you have different
#   names, feel free to swap out my data name in the plots below with your data names

# While this isn't a formal habitat selection analysis, we can look at the distribution of 
#   slope values in our used and available data for each behavioral state to get an idea
#   of if that variable (slope) is selected for or not.
# Below are density plots, which are basically just smoothed-out histograms, of our slope data:
ggplot(puma_travel_habitat, aes(x = slope, fill = Used)) +
  geom_density(alpha = 0.4)
ggplot(puma_rest_habitat, aes(x = slope, fill = Used)) +
  geom_density(alpha = 0.4)

#** QUESTION 5: Does this puma generally select for higher slopes than are available when 
#     traveling? What about resting? What might this tell us about the value of flat areas 
#     for puma habitat conservation in this area?
# Export both plots for your lab report.

#--------------#
#### QUESTIONS FROM LECTURE AND READINGS ####

#** QUESTION 6: Explain what is meant by "wildlife resources are a public trust" within the 
#       North American model of wildlife conservation.

#** QUESTION 7: 1.  In lecture, we learned that not all wildlife species exhibit phenological shifts 
#       at the same rate or magnitude.  Give an example of one consequence caribou may face 
#       if their annual breeding date does not advance at the same rate as advancing plant emergence dates 
#       in Greenland.  

#--------------#
#### CONGRATS! ####
# You've learned how examine behavioral differences in habitat selection in R
# Please reach out to your TA, Julianne Pekny (jpekny@ucdavis.edu) for questions about this lab