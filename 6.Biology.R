library(ggplot2)
library(marmap)
library(rgdal)
library(rgeos)
library(raster)
library(readxl)
library(tidyverse)
library(mapproj)


#read in the position of ISECOLD sites
sites19 <- read_excel("C:/Users/jchawars/OneDrive - Memorial University of Newfoundland/DFO Lab Sea/eDNA/eDNA_field_samples_ISECOLD2019.xlsx")
sites19 <- sites19 %>% group_by(site) %>% summarise(lat =mean(latitude), lon=mean(longitude))

sites18 <- read_excel("C:/Users/jchawars/OneDrive - Memorial University of Newfoundland/DFO Lab Sea/ISECOLD2018_sites.xlsx")
sites18 <- sites18 %>% group_by(site) %>% summarise(lat =mean(lat), lon=mean(lon))

all.sites <- rbind(sites19, sites18)
all.sites <- all.sites[1:17,]
all.sites$transect <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3)

#zooplankton sampling sites Amundsen 2015-2019
Baffin_zoo <- read_excel("C:/Users/jchawars/OneDrive - Memorial University of Newfoundland/DFO Lab Sea/BaffinBay_zooanalysis_2015-2019-Julek20200421.xlsx")

#fish sampling sites Amundsen 2015-2019
baffin_fish <- read_excel("C:/Users/jchawars/OneDrive - Memorial University of Newfoundland/DFO Lab Sea/Fish_adult_2014-2019-20200526.xlsx", sheet = 1)

baffin_sites <- baffin_fish %>% 
  dplyr::filter(`Arctic region` == "Baffin Bay") %>%
  group_by(`Sampling time`) %>% 
  summarise(lat =mean(`Latitude deg in`), 
            lon=mean(`Longitude deg in`),
            stn.depth = unique(`Station Depth (m)`),
            tow.type = unique(`Tow type`),
            gear = unique(`Gear`),
            station = unique(`Station`))                           

baffin_sites <- baffin_sites %>% filter(gear != "Benthic Trawl")
baffin_sites <- baffin_sites %>% filter(stn.depth >= 500)
baffin_sites <- baffin_sites %>% filter(lon >= -77)

baffin_sites$UniqueID <- paste(baffin_sites$`Sampling time`, baffin_sites$station)
baffin_fish$UniqueID <- paste(baffin_fish$`Sampling time`, baffin_fish$Station)

baffin_sub <- baffin_fish %>% filter(baffin_fish$UniqueID %in% baffin_sites$UniqueID) %>%
  filter(Station != "228")


# quick pie plot for frequency of species
sub_freq <- data.frame(table(baffin_sub$`Fish family`))
ggplot(sub_freq, aes(x="", y=Freq, fill=Var1)) + geom_bar(width=1, stat="identity") + coord_polar("y", start=0)

colnames(baffin_sub)[8] <- "lat"
colnames(baffin_sub)[9] <- "lon"

baffin_myc <- baffin_sub %>% filter(`Fish family` == "Myctophidae" & Station != "228")


ggplot(baffin_myc, aes(x=`Standard length (mm)`)) + 
  geom_histogram(binwidth = 2.5, fill="lightsteelblue2", color="slategray4") + 
  geom_vline(aes(xintercept = mean(`Standard length (mm)`)),col='red', linetype="dashed",size=1) + 
  xlim(15,90) +
  theme_bw()

colnames(all.sites)[1] <- "station"
all.site <- Baffin_fish %>% bind_rows(., all.sites)

#read in shape files for bathymetry and coast

bath <- readGEBCO.bathy("GEBCO_LabSea/gebco_2019_n76.3330078125_s53.349609375_w-69.63134765625_e-46.38427734375.nc")  # your GEBCO sourced netcdf - this is a marmap fxn
bath.f <- fortify.bathy(bath) # this is a marmap fxn
brk <- c(-500, -1000, -1500, -2000, -3000)   #define bathymetry breaks

coast <- readOGR("C:/Users/jchawars/OneDrive - Memorial University of Newfoundland/GIS/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp") # your NOAA sourced coastline shapefiles - this is an rgdal fxn
coast.trim <- crop(coast, extent(-72, -45, 55, 80))  # order = xmin (w), xmax(e), ymin(s), ymax(n)

#map all the sites for biological sampling

ggplot(baffin_myc, aes(x=lon, y=lat)) + 
  
  geom_polygon(data= coast.trim,                      #coast - high resolution coastline
               aes(x=long, 
                   y=lat,
                   group=group),
               fill= "lavender", 
               colour ="lightsteelblue1",
               inherit.aes = F) +
  
#  geom_contour(data = bath.f,                       #bathymetry - plot this as the bottom layer... add coast and stations after
  #             aes(x=x, y=y, z=z),
    #           breaks=-1000,
    #           size=c(0.3),
      #         colour="lightsteelblue2") + 
  
  geom_point() + geom_text(aes(label=Station)) +
  
  coord_map("gilbert") + theme_bw()

#cruise tracks for timeseries - Under construction -
files <- list.files(path="C:\\Amundsen Time Series", full.names = T, pattern= "*.csv")  # load files from CTD folder

tracks.all <- lapply(files, function(i) read.csv(i)) %>% 
  lapply(., mutate_if, is.character, as.numeric) %>% 
  mapply(cbind, ., "SampleID"=unique(files), SIMPLIFY=F) %>% # creates a unique id from filenames
  bind_rows() %>%
  mutate(., transect = substr(SampleID, 25, 34)) %>% # selects the character range where stn is saved
  select(-SampleID) %>%
  sample_n(.,200000)



