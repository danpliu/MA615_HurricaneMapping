library(tidyverse)
library(hurricaneexposuredata)
library(maps)
library(sp)
library(sf)
library(leaflet)
library(htmlwidgets)

# Import data from package hurricaneexposuredata
data("hurr_tracks")
data("rain")
f_ht <- select(filter(as.data.frame(hurr_tracks), storm_id=="Floyd-1999"),-c(storm_id, usa_atcf_id, wind))
f_rain <- select(filter(as.data.frame(rain), storm_id=="Floyd-1999"), -c(storm_id, usa_atcf_id))
a_ht <- select(filter(as.data.frame(hurr_tracks), storm_id=="Allison-2001"), -c(storm_id, usa_atcf_id, wind))
a_rain <- select(filter(as.data.frame(rain), storm_id=="Allison-2001"), -c(storm_id, usa_atcf_id))

# Add up precipitation in each county
f_rain_precip <- aggregate(precip~fips, f_rain, sum)
a_rain_precip <- aggregate(precip~fips, a_rain, sum)

# Create new columns for graphing
f_rain_precip$`Rainfall (mm)` <- rep(NA, nrow(f_rain_precip))
a_rain_precip$`Rain > 175 mm` <- rep(NA, nrow(a_rain_precip))

f_rain_precip$`Rainfall (mm)`[f_rain_precip$precip>=0 & f_rain_precip$precip<=25] <- 1
f_rain_precip$`Rainfall (mm)`[f_rain_precip$precip>25 & f_rain_precip$precip<=50]<- 2
f_rain_precip$`Rainfall (mm)`[f_rain_precip$precip>50 & f_rain_precip$precip<=75] <- 3
f_rain_precip$`Rainfall (mm)`[f_rain_precip$precip>75 & f_rain_precip$precip<=100] <- 4
f_rain_precip$`Rainfall (mm)`[f_rain_precip$precip>100 & f_rain_precip$precip<=125] <- 5
f_rain_precip$`Rainfall (mm)`[f_rain_precip$precip>125 & f_rain_precip$precip<=150] <- 6
f_rain_precip$`Rainfall (mm)`[f_rain_precip$precip>150 & f_rain_precip$precip<=175] <- 7
f_rain_precip$`Rainfall (mm)`[f_rain_precip$precip>175 & f_rain_precip$precip<=200] <- 8
f_rain_precip$`Rainfall (mm)`[f_rain_precip$precip>200] <- 9

a_rain_precip$`Rain > 175 mm`[a_rain_precip$precip>=175] <- 1
a_rain_precip$`Rain > 175 mm`[a_rain_precip$precip<175] <- 0

f_rain_precip$`Rainfall (mm)` <- 
  factor(f_rain_precip$`Rainfall (mm)`, levels=c(1:9), 
         labels=c("[0,25]","(25,50]", "(50,75]", 
                  "(75,100]", "(100,125]", "(125,150]", 
                  "(150,175]", "(175,200]", "(200,222]"), ordered=TRUE)

a_rain_precip$`Rain > 175 mm` <- 
  factor(a_rain_precip$`Rain > 175 mm`, levels=c(0:1), 
         labels=c("Unexposed", "Exposed"))

# Turn fips to region(state) and subregion(county) columns
data(county.fips)
f_rain_precip$fips <- as.double(f_rain_precip$fips)
f_rain_precip <- left_join(f_rain_precip, county.fips, by="fips")
f_rain_precip$region <- str_split(f_rain_precip$polyname, ",", simplify=TRUE)[,1]
f_rain_precip$subregion <- str_split(f_rain_precip$polyname, ",", simplify=TRUE)[,2]
f_rain_precip <- select(f_rain_precip, -polyname)

a_rain_precip$fips <- as.double(a_rain_precip$fips)
a_rain_precip <- left_join(a_rain_precip, county.fips, by="fips")
a_rain_precip$region <- str_split(a_rain_precip$polyname, ",", simplify=TRUE)[,1]
a_rain_precip$subregion <- str_split(a_rain_precip$polyname, ",", simplify=TRUE)[,2]
a_rain_precip <- select(a_rain_precip, -polyname)

# Import longitudes and latitudes data
a_statemap <- map_data("state", region=unique(a_rain_precip$region))
a_countymap <- map_data("county", region=unique(a_rain_precip$region))
f_statemap <- map_data("state", region=unique(f_rain_precip$region))
f_countymap <- map_data("county", region=unique(f_rain_precip$region))

a_rain_precip <- left_join(a_countymap, a_rain_precip, by=c("region", "subregion"))
f_rain_precip <- left_join(f_countymap, f_rain_precip, by=c("region", "subregion"))

# Delete NA's
a_rain_precip <- filter(a_rain_precip, !is.na(`Rain > 175 mm`))
f_rain_precip <- filter(f_rain_precip, !is.na(`Rainfall (mm)`))

# Floyd-1999 plot using ggplot2
plot1 <- ggplot()+
  geom_polygon(f_rain_precip,mapping=aes(x=long, y=lat, group=group, fill=`Rainfall (mm)`),color = "lightgrey")+
  scale_fill_brewer(palette="Blues")+
  geom_path(data=f_statemap, aes(x=long, y=lat, group=group), color="black")+
  geom_path(data=f_ht, aes(x=longitude, y=latitude), color="darkred")+
  ggtitle("Floyd-1999")+
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5,face = "bold")
  )
plot1
ggsave(file="Floyd-1999_ggplot.jpg", plot=plot1)

# Allison-2001 plot using ggplot2
plot2 <- ggplot()+
  geom_polygon(a_rain_precip,mapping=aes(x=long, y=lat, group=group, fill=`Rain > 175 mm`),color = "lightgrey")+
  scale_fill_manual(values=c("white","darkblue"))+
  geom_path(data=a_statemap, aes(x=long, y=lat, group=group), color="black")+
  geom_path(data=a_ht, aes(x=longitude, y=latitude), color="darkred")+
  ggtitle("Allison-2001")+
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5,face = "bold")
  )
plot2
ggsave(file="Allison-2001_ggplot.jpg", plot=plot2)

# Convert the data frames into sp
M=st_as_sf(map("county",plot=F,fill=T))
colnames(county.fips)[2]=colnames(M)[1]
M=left_join(M,county.fips,"ID")

f_ht_sp <- cbind(f_ht$longitude,f_ht$latitude)%>%
  Line()%>%Lines(ID="Floyd-1999")%>%
  list()%>%SpatialLines()

a_ht_sp <- cbind(a_ht$longitude,a_ht$latitude)%>%
  Line()%>%Lines(ID="Allison-2001")%>%
  list()%>%SpatialLines()

f_rain_sp <- left_join(M, f_rain_precip, "fips")
f_rain_sp <- filter(f_rain_sp, !is.na(f_rain_sp$precip))
a_rain_sp <- left_join(M, a_rain_precip, "fips")
a_rain_sp <- filter(a_rain_sp, !is.na(a_rain_sp$precip))

# Draw graphs
f_pal <- colorFactor(palette="Blues", domain=f_rain_sp$`Rainfall (mm)`)
plot3 <- leaflet() %>%
  addTiles() %>%
  fitBounds(lng1=-67.00742, lat1=47.48101,
            lng2=-106.6504, lat2=25.12993) %>%
  addPolygons(data=f_rain_sp, 
              stroke=FALSE, smoothFactor=0.2, fillOpacity=1,
              color=~f_pal(`Rainfall (mm)`)) %>%
  addPolylines(data=f_rain_sp, weight=0.3, color="lightgrey") %>%
  addPolylines(data=f_ht_sp, weight=1.5, color="darkred") 

plot3 <- plot3 %>%
  addLegend(data=f_rain_sp, pal=f_pal, 
            values=f_rain_sp$`Rainfall (mm)`, 
            opacity=0.75, position="bottomright")


a_pal <- colorFactor(palette=c("white", "darkblue"), domain=a_rain_sp$`Rain > 175 mm`)
plot4 <- leaflet() %>%
  addTiles() %>%
  fitBounds(lng1=-67.00742, lat1=47.48101,
            lng2=-106.6504, lat2=25.12993) %>%
  addPolygons(data=a_rain_sp, 
              stroke=FALSE, smoothFactor=0.2, fillOpacity=1,
              color=~a_pal(`Rain > 175 mm`)) %>%
  addPolylines(data=a_rain_sp, weight=0.3, color="lightgrey") %>%
  addPolylines(data=a_ht_sp, weight=1.5, color="darkred") 
plot4 <- plot4 %>%
  addLegend(pal=a_pal, 
            values=a_rain_sp$`Rain > 175 mm`,
            opacity=0.75, position="bottomright")

# The leaflets generated are huge and may take a long time to load. 
# Try to load one leaflet everytime
plot3
# plot4

# Since the leaflet object is too large, my Macbook got stuck everytime when I try to save it as HTML. 
# The saving command is commented below. 
# saveWidget(plot3, file="Floyd-1999.html")
# saveWidget(plot4, file="Allison-2001.html")
