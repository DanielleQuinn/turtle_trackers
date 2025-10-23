### TURTLE TRACKERS CODING WORKSHOP - WORKING CODE ###

## STEP 0 - GENERAL R FUNCTIONS INTRO
# In the space below, type any name (e.g. "bob", "tony", "alexa", etc.) followed by an arrow (<-) or equal sign (=),
# followed by a number of your choice. What we are doing here is assigning a value (the number) to a 
# name. We will continue to do this throughout the code later and use it to track turtles!

bob <- 5

## STEP 1 - LOAD PACKAGES
# In order to use functions in R, we need to load them into the coding environment. The "library" function allows us
# load packages that contain various functions for us to use in different contexts. In this case, most of these
# packages help us make maps and graphs.

library(tidyverse)
library(readr)
library(ggspatial)
library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

## STEP 2 - IMPORT DATA AND VIEW
# We've just done all the work to organize our data into a csv file that we can put into R - now is the time to use it! 

# BLANK TO FILL: In the blanks (__DATA_NAME__), chose a name for our data. We will use this name repeatedly in other code,
# so you want to make it something short but still something that indicates what it is
# (e.g. "turtledata", "trackingdata", etc.). From here-on, whenever there is a blank that looks like so: __DATA_NAME__
# replace it with the name you assigned.

turtledata <- read.csv("turtledata.csv")
view(turtledata)

## STEP 3 - FILTER DATA BY TURTLE ID AND VIEW 
# In order to make a map showing the track of just one turtle, we have to filter our data by the turtle ID.

# BLANKS TO FILL: 1) ___FILTERED_DATA_NAME___: Name your filtered data. The best name is whatever you named 
#                   the unfiltered data above, followed by "filtered" (e.g. "turtledata_filttered")
#                 2)  __DATA_NAME__: Add you data name
#                 3) ____TURTLE_ID_NAME____: Chose the turtle you'd like to track and put it's name in the quotes

turtledata_filtered <- turtledata %>%
  filter(turtle.ID == "squirt")

## STEP 4 - IMPORT MAP DATA
# R needs spatial data to make a map of the area we are interested in. The function below allows R to access that data
# that already exists in one of the packages we loaded earlier. "world" refers to this spatial data.

world <- ne_countries(scale = "medium", returnclass = "sf")

## STEP 4 - SET BOUNDARIES FOR THE MAP 
# The spacial data we loaded above is for the entire globe. All of our turtle pings are in Eastern Canada and 
# the United States. So, we need to tell R what limits we want to set for our maps. In other words, we have to 
# tell R where on the map of the world to zoom in. Each of the number values below refer to longitude and lattitude.
# the "x" values refer to eastern and western limits, and the "y" values refer to Northern and Southern limits. 
# For now, we will keep this numbers as they are because all of our coordinates from our data fit in this map. 
# Later, we will change this to better display your turtle track.

E_limit <- -90  # Eastern limit
W_limit <- -40  # Western limit
S_limit <- 29   # Southern limit
N_limit <- 59   # Northern limit

## STEP 5 - MAKE A MAP OF THE NORTHWESTERN ATLANTIC
# Now that we have told R the limits of our big map, we have to give it more details such as - what colour to make
# the land and water, what labels to use, etc. 

# BLANKS TO FILL: Right now, all the colours are set to white. Use the colours listed at the link below to assign
# appropriate colours to land, water, and text. Look to the comments on the right side of the code to determine
# if that code is for land, water or text. Replace the "white" with the colours you chose in from the link. 
# Just make sure that the colour is in quotes (e.g. "blue")

# Colours link: https://www.datanovia.com/en/blog/awesome-list-of-657-r-color-names/ 

blankmap <- ggplot(data = world) +
  geom_sf(fill = "darkgreen", color = "black") +  #land and land borders
  coord_sf(xlim = c(E_limit, W_limit), ylim = c(S_limit, N_limit), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    panel.background = element_rect(fill = "navy", color = NA),  #ocean
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),  # frame around map
    axis.text = element_text(color = "black"), #text
    axis.title = element_text(color = "black") #text
  )

print(blankmap)

## STEP 6 - ADD TURTLE PINGS TO MAP AND RE-PRINT THE MAP
# Now is finally the time to add our data into our map. This code will use the coordinates from our
# csv file for each of the turtle pings and add a dot to the map with a label of the date that that
# sighting was made. 

# BLANKS TO FILL: 1) ___FILTERED_DATA_NAME___ x2: replace with the name you previously assigned for filtered data
#                 2) "white" x2: Assign a colour to the dots and to the text
markedmap <- blankmap + 
  geom_point(data = turtledata_filtered, aes(x = long, y = lat), color = "red", size = 2) +
  geom_text(
    data = turtledata_filtered,
    aes(x = long, y = lat, label = date),
    nudge_y = 0.75,  # move labels slightly above points
    size = 2,
    color = "red"
  )

print(markedmap)

## STEP 7 - ADJUST THE BOUNDS OF THE MAP TO BETTER VIEW THE TURTLE PING TRACK
# Now that our sightings are on our map, we need to change the bounds of the map to "zoom in" to where the 
# sightings are. To do that, we are going to assign new  xmin/max and ymin/max values (see below). 

# BLANKS TO FILL - Using the current bounds set on our map on the right, change the blank values below 
# to reset the bounds of the map.

new_E_limit <- -70  # Eastern limit
new_W_limit <- -50   # Western limit
new_S_limit <-  40  # Southern limit
new_N_limit <-  60  # Northern limit

## STEP 7 - REPRINT THE MAP WITH THE NEW BOUNDARIES.
# Now if finally the time to put everything together.

# BLANKS TO FILL - 1) "white" x7: Re-assgin colours to the map.
#                   2) ___FILTERED_DATA_NAME___ x2: Replace with the name you previously assigned for filtered data
finalmap <- ggplot(data = world) +
  geom_sf(fill = "darkgreen", color = "black") + # Land and land borders colours
  coord_sf(xlim = c(new_E_limit, new_W_limit), ylim = c(new_S_limit, new_N_limit), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    panel.background = element_rect(fill = "blue", color = NA), #water colour
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA), #map border
    axis.text = element_text(color = "black"), #text
    axis.title = element_text(color = "black") #text
  ) +
  
  geom_point(data = turtledata_filtered, aes(x = long, y = lat), color = "red", size = 3) + #points on the map
  geom_text(
    data = turtledata_filtered,
    aes(x = long, y = lat, label = date),
    nudge_y = 0.5,
    size = 3,
    color = "red", # date labels
    fontface = "bold"
  )

print(finalmap)

## BONUS STEP - ADD A SCALE BAR AND CALCULATE HOW FAR THE TURTLE TRAVELLED
# In order to properly see the distance between each point (the distance traveled by a turtle between sightings)
# we need a scale bar. This is a bar at the bottom of the map that gives you a relative measurement bar that can be
# used to measure distance on a map. 

# Use the below to add a scale bar. If you'd like, you can change the location. To do this, change the letters in
# quotes after "location = " using one of the follow abbreviations .
#         - "br" = bottom right
#         - "bl" = bottom left
#         - "tr" = top right
#         - "tl" = top left

scalemap <- finalmap +
  annotation_scale(location = "br", width_hint = 0.2)

print(scalemap)

# Now use the scale bar to measure the distance between points to see how far your turtle traveled between sightings!
