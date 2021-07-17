#devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr) #Map Data
library(rebird) #eBird API Client
library(tidyverse) #Data Processing
library(ggplot2) #Plotting
library(extrafont) #Plotting
library(scico) #Plotting

eBird_API_Key <- "Your Key Here" #Key can be obtained at https://ebird.org/api/keygen

#Here we're going to run our first api request, this will return a list of all the US Counties in eBird.
counties <- ebirdsubregionlist("subnational2", "US", key = "scshi43kh3r9")
#Next, were just adding a new column in counties_sf (the county polygon tibble) to match the eBird county codes so we can merge the two later.
counties_sf$merge <- paste("US", counties_sf$state_abbv, substr(counties_sf$county_fips, 3, 5), sep = "-")

# The next line is critical, however, due to the heavy use of the eBird API, 
# please limit the use of this loop as much as possible. You should only need to run it once.
for(i in 1:nrow(counties)){
  counties$Species[i] <- nrow(ebirdregionspecies(counties$code[i], key = "scshi43kh3r9"))
}

# This next loop is due to name change of this county, we need to revise the county FIPS code.
for(i in 1:nrow(counties)){
  if(counties$name[i] == "Oglala Lakota"){
    counties$code[i] <- "US-SD-102"
  }
} 

# Now, the moment of truth, this line is going to join the counties and counties_sf data frames so we can plot our data.
counties_eBird <- inner_join(counties_sf, dplyr::group_by(counties, code) %>%
                             summarise(Species=max(Species)), by=c("merge"="code"))

# Now the plot
p <- ggplot() +
  # We're going to use geom_polygon to render our county shapes, geom_sf may also work, but I've found this to be easier.
  geom_polygon(data = counties_eBird, aes(fill = Species, x = long, y = lat, group = group) , size=0, alpha=0.95) +
  # Adding theme_void is essental for choropleth maps in ggplot2 as we don't need gridlines.
  theme_void()+
  # This will give us the legend and color palette.
  scale_fill_scico(palette = "bilbao", direction = 1, name="Bird Species", guide = guide_colourbar(direction = "horizontal", label.position = "bottom", title.position = 'top', nrow=1)) +
  # Adding labels
  labs(
    title = "Bird Species by County",
    subtitle = "Number of Bird Species by County",
    caption = "Data: eBird & U.S. Census Bureau   "
  ) +
  # Adding the final touches
  theme(
    text = element_text(color = "#22211d", family="Maiandra GD"),
    plot.background = element_rect(fill = "#d1cec5", color = NA),
    panel.background = element_rect(fill = "#d1cec5", color = NA),
    legend.background = element_rect(fill = "#d1cec5", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.03, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.03, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.8, 0.9)
  ) +
  # We need to finally add coord_map to maintain the aspect ratio of the polygons.
  coord_map()
p
