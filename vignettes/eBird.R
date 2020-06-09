library(ebirdst)
library(raster)
library(rvest)
library(sp)
library(viridis)

# 1. Get coordinates for wind farms that might be in Bay et al. (2016)
url <- 'https://www.windpowerengineering.com/wind-project-map/'
page <- read_html(url)
tab <- html_node(page, 'table')
links <- html_nodes(tab, 'a')%>%
  html_attr('href')

#' return the latitude and longitude from a wind project's page
#' @description this function is mean to be applied the link(s) for individual wind
#' projects provided by www.windengineering.com
#' @param link string url
get_lat_lon <- function(link){
  page <- read_html(link)%>%
    html_node('body')%>%
    html_text()

  lat <- str_extract(page, 'latitude: [0-9]*.[0-9]*')
  lon <- str_extract(page, 'longitude: -[0-9]*.[0-9]*')
  return(c(lat, lon))
}

test <- sapply(links, get_lat_lon, USE.NAMES = FALSE)

dat$Lat <- str_remove(test[1,], 'latitude: ')
dat$Long <- str_remove(test[2,], 'longitude: ')

dat <- mutate(Lat = str_extract(dat$Lat, '[0-9]*.[0-9]*'),
              Long = str_extract(dat$Long, '-[0-9]*.[0-9]*'))

# read in data from Bay et al. (2016) on site effort and eagle activity
bay_2016 <- readRDS(file = 'vignettes/data/Bay_2016.rds')
bay_2016 <- bay_2016%>%
  mutate(State = str_split_fixed(SITE, ',', n = 2)[,2],
         Site = str_split_fixed(SITE, ',', n = 2)[,1])%>%
  # add latitude and longitude from scraped web data
  left_join(dat[,c(1,6:7)], by = c('Site' = 'Name'))

# need to add some lat, lon data manually
bay_2016[bay_2016$Site == 'Vantage', 22:23] <- c(46.954, -120.186)
bay_2016[bay_2016$Site == 'Vansycle', 22:23] <- c(45.897, -118.579)
bay_2016[bay_2016$Site == 'Klondike', 22:23] <- c(45.584, -120.553)
bay_2016[bay_2016$Site == 'Kittitas Valley', 22:23] <- c(47.154, -120.679)
bay_2016[bay_2016$Site == 'Hopkins Ridge', 22:23] <- c(46.432, -117.818)

bay_2016[bay_2016$Site =='Alta Oak Creek Mojave (Alta I)', 22:23]<- c(35.036, -118.365)
bay_2016[bay_2016$Site =='Alta Oak Creek Mojave (Alta II–V)', 22:23]<- c(35.01, -118.269)
bay_2016[bay_2016$Site =='Campbell Hills', 22:23]<- c(43.02, -105.992)
bay_2016[bay_2016$Site =='Diablo Winds', 22:23]<- c(37.711, -121.64)
bay_2016[bay_2016$Site =='Foote Creek Rim (Phases II and III)', 22:23]<- c(41.647, -106.197)
bay_2016[bay_2016$Site =='Dry Lake I', 22:23]<- c(34.646, -110.29)
bay_2016[bay_2016$Site =='Elkhorn', 22:23]<- c(45.05, -117.804)
bay_2016[bay_2016$Site =='Foote Creek Rim (Phase I)', 22:23]<- c(41.647, -106.197)
bay_2016[bay_2016$Site =='Windy Flats', 22:23]<- c(45.766, -120.652)

# 2. Read eBird raster data
golden_rast <- ebirdst_download('golden eagle')
