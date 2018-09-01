# Name: Ajinkya Rajendra Gawali
# Name: Juilee Paranjpe

library(twitteR)
library(sp)
library(maps)
library(maptools)
library('fiftystater')
library(ggplot2)
#---------------------------------------------TWEETS EXTRACTOR---------------------------------------------
#Setting up keys
consumer_key    <- "5RRobOAR92qXK0Oi6ETOoN3Vq"
consumer_secret <- "PZn2pgtfnFkLKPSSEdBQywO6dDhHQDFvO12wwQxiZYN1dNX4Tv"
access_token    <- "836059788832210945-vOMXctZLxPEOhWUw3gtrqhqOIZr0jRS"
access_secret   <- "zoXFGt0zsFsihDi6iJZHWUw8He2c9U4mRODSz4zEB8nFQ"


setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tw <- twitteR::searchTwitter('#flu', n = 20000, retryOnRateLimit = 1e3)
new_tweets <- twitteR::twListToDF(tw)                        # get new tweets

export_file <- "D:\\Academics\\Spring 2018\\Data Intensive Computing\\Lab1\\TwitterApp\\tweets2.csv";
old_tweets <- read.csv(export_file, stringsAsFactors=FALSE)  # read old file

unique_tweets <-unique(rbind(old_tweets[,2:17],new_tweets))  # append non-duplicate tweets to old tweets file
write.csv(unique_tweets,export_file)                         # write appended tweets with original to the file
#-------------------------------------------------------------------------------------------------------------

#------------------------------------------TWEETS HEATMAP----------------------------------------------
#Choose  tweets2.csv
tweets <- read.csv(file.choose(), stringsAsFactors=FALSE)

user_geo_df <- data.frame(tweets$screenName,tweets$longitude,tweets$latitude)
valid_geo <-  user_geo_df[!is.na(user_geo_df$tweets.longitude),]

#Choose StateDatabyWeekforMap_2017-18week4-4.csv

chart7 <- read.csv(file.choose())

names <- tolower(chart7$STATENAME)
statesListCount <- data.frame(names)
statesListCount[,"count"] <- NA


data("fifty_states")

# This function converts lat long to states using the spatial polygons formed by the states.
# This function is refered from Josh O'Brien's answer on stackoverflow
# https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r/8751965#8751965

toState <- function(pointsDF) {
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  split <- strsplit(states$names, ":")
  IDs <- sapply(split, function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,proj4string=CRS("+proj=longlat +datum=wgs84"))
  pointsSP <- SpatialPoints(pointsDF, proj4string=CRS("+proj=longlat +datum=wgs84"))
  indices <- over(pointsSP, states_sp)
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


lat_long <- data.frame(valid_geo$tweets.longitude,valid_geo$tweets.latitude)
states_loc <- toState(lat_long)
states_loc <- states_loc[!is.na(states_loc)]

states_freq <- as.data.frame(table(states_loc))
indices_ <- c()
for (i in 1:length(states_freq$states_loc))
{
  state = as.character(states_freq$states_loc[i])
  (index <- match(state,statesListCount[,'names']) )
  append(indices_,index)
  if(!is.na(index))
  {  statesListCount[index,'count'] = states_freq[i,'Freq']
  }}

( p <- ggplot(statesListCount, aes(map_id = names)) +
    # map points to the fifty_states shape data
    geom_map(aes(fill = count),color="#ffffff", map = fifty_states) +
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    labs(x = "", y = "") +
    theme(legend.position = "bottom",panel.background = element_blank())+
    scale_fill_gradient(low = "#ffff99" ,high = "#B20000",na.value = "grey50")
    )
