#z-shapefiles



# https://www.r-graph-gallery.com/168-load-a-shape-file-into-r.html


library(rgdal)
my_spdf <- readOGR( 
  dsn= "data/Arc" , 
  # layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

summary(my_spdf) 
data.table(my_spdf@data)

dt <- data.table(my_spdf@data)
dt[, OBJECTID:= as.ordered(OBJECTID)]
dt[, PR_HRUID:= as.ordered(PR_HRUID)]
dt %>% summary()



library(broom)
spdf_fortified0 <- tidy(my_spdf0, region = "NAME")


spdf_fortified <- tidy(my_spdf, region = "PR_HRUID")
spdf_fortified <- tidy(my_spdf, region = "OBJECTID")

spdf_fortified <- tidy(my_spdf)

spdf_fortified <- spdf_fortified0 %>% data.table()

spdf_fortified[, id:=as.ordered(id)]

spdf_fortified[, order:=as.ordered(order)]
spdf_fortified[, piece:=as.ordered(piece)]
spdf_fortified[, group :=as.ordered(group )]
spdf_fortified %>% summary()

# Plot it

ggplot(spdf_fortified) +
  #guides(fill="none") +
  theme(legend.position = "bottom") +
  theme_void() +
  # geom_polygon(aes( x = long, y = lat, group = group, fill=id), fill="#69b3a2", color="white")
  geom_polygon(aes( x = long, y = lat, group = group, fill=id))
# geom_polygon(aes( x = long, y = lat, group = group, fill=group)) 




if (F) {
  # Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
  download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="data/z-map/world_shape_file.zip")
  system("unzip DATA/world_shape_file.zip")
  my_spdf0 <- readOGR( 
    dsn= "data/z-map" , 
    layer="TM_WORLD_BORDERS_SIMPL-0.3",
    verbose=FALSE
  )
  
}