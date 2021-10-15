library(dplyr)
library(sf)
library(ggplot2)
library(sp)
library(cartogram)
library(readr)
library(gganimate)
library(tweenr)
library(broom)
library(extrafont)

# See R-graph gallery for original blog post on how to do it (minor change since to the {gganimate} API):
# https://www.r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram.html

israel_sf <- readRDS("Data/Israel-map/gadm36_ISR_1_sf.Rds")

# Convert Golan + North into one since we'll need it aligned with the population:
israel_sf_joined <- israel_sf %>% 
  mutate(group_agg = NAME_1,
         group_agg = case_when(
         group_agg %in% c("Golan", "HaZafon") ~ "North",
         group_agg == "HaDarom" ~ "South",
         group_agg == "HaMerkaz" ~ "Center",
         TRUE ~ NAME_1)) %>% 
  group_by(group_agg) %>% 
  summarise()
  
# Alternatively, join using aggregate. Thanks to Michael Dorman for the help!
# israel_sf_joined <- aggregate(israel_sf_joined["geometry"], st_drop_geometry(israel_sf_joined["group_agg"]), sum)

# Israel population for 2017 from
# https://data.gov.il/dataset/localauthorities2018
pop <- read_csv("Data/20_Israel-pop.csv")


pop_dat <- pop %>% 
  group_by(district) %>% 
  # Convert to by million
  summarise(pop_sum = sum(population_thousands)/ 1000) %>% 
  # Change names to match the sf object
  mutate(name = case_when(
    district == "הדרום" ~ "South",
    district == "המרכז" ~ "Center",
    district == "הצפון" ~ "North",
    district == "חיפה" ~ "Haifa",
    district == "ירושלים" ~ "Jerusalem",
    district == "תל אביב" ~ "Tel Aviv"
  ))

israel_sf_joined <- left_join(israel_sf_joined, pop_dat, by = c("group_agg" = "name")) %>% 
  # For a cartogram transform coordinates
  st_transform(crs = 23038)

# Make the cartogram
cartogram <- cartogram_cont(israel_sf_joined, "pop_sum", itermax = 7)

isr_carto <- as(st_geometry(cartogram), "Spatial") %>% 
  tidy()

israel <- as(st_geometry(israel_sf_joined), "Spatial") %>% 
  tidy()

isr_carto$id <- seq(1,nrow(isr_carto))
israel$id <- seq(1,nrow(israel))

israel_combined <- rbind(israel, isr_carto,israel)

israel_combined$ease <- "cubic-in-out"
israel_combined$time <- rep(c(1:3), each=nrow(israel))

israel_combined <- tween_elements(israel_combined, time='time', group='id', ease='ease', nframes = 40)

# the geographical regions are saved as ID once we use the tween_elements. I imagine
# you can solve this beforehand if you save the data as a dataframe in the spatial object
israel_combined <- israel_combined %>% 
  mutate(group = as.character(group),
         group = case_when(
           group == "ID1.1" ~ "Center",
           group == "ID2.1" ~ "Haifa",
           group == "ID3.1" ~ "Jerusalem",
           group == "ID4.1" ~ "North",
           group == "ID5.1" ~ "South",
           group == "ID6.1" ~ "Tel Aviv",
           TRUE ~ group
         ))

israel_combined <- israel_combined %>% 
  left_join(pop_dat, by = c("group" = "name"))

# Start with exploring the plot on only 1 frame, adjust any aesthetics and
# then explore with the animation
p <- ggplot() + 
  geom_polygon(data = israel_combined  %>% arrange(order), aes(fill = pop_sum, x = long, y = lat, group = group) , size=0, alpha=0.9, color = "gray35") +
  scale_fill_continuous(trans = "reverse",name="Population (Millions)", guide = guide_legend(keyheight = unit(1, units = "mm"), keywidth=unit(6, units = "mm"), label.position = "bottom", title.position = 'top', ncol=1))+
  labs(title = "Israel's population", subtitle = "Map transitions between a regular map to a cartogram, a map where\nthe geographic size is proportional to the population in that area.\nData is from Israel's CBS 2017 report and is aggregated at the district level", caption = "Data: data.gov.il\n@Amit_Levinson")+
  theme_void() +
  theme(text = element_text(family = "IBM Plex Sans"),
        plot.margin = unit(c(3,3,3,5), "mm"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 8, color = "gray15"),
        legend.title=element_text(size= 7),
        legend.text = element_text(size = 6),
        plot.caption = element_text(size = 6, face = "italic", color = "gray45"))+
  transition_states(.frame)+
  enter_fade() +
  exit_fade()

# The rendering takes some time, so tweak and do any aesthetics on a filtered
# version of the data. Just add %>% filter(.frame == 1) in the data argument above
animate(p, start_pause = 3, width = 4, height = 7, units = "in", res = 150)

anim_save("Code/20_population/20_map.gif")


# Or make both plots just not animated  -----------------------------------

library(patchwork)

p1 <- ggplot() + 
  geom_sf(data = israel_sf_joined, aes(fill = pop_sum) , size=0, alpha=0.9, color = "gray35") +
  scale_fill_gradient(trans = "reverse",name="Population (Millions)",  guide = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(8, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1))+
  labs(title = "Regular")+
  theme_void() +
  theme(plot.title = element_text(size = 14),
        legend.title=element_text(size= 12),
        legend.text = element_text(size = 10),
        legend.spacing.x = unit(3, 'mm'))

p2 <- ggplot() + 
  geom_sf(data = cartogram, aes(fill = pop_sum) , size=0, alpha=0.9, color = "gray35") +
  scale_fill_continuous(trans = "reverse",name="Population (Millions)", guide = NULL)+
  ggtitle("Cartogram")+
  labs(title = "Cartogram", caption = "Data: data.gov.il\n@Amit_Levinson")+
  theme_void() +
  theme(plot.title = element_text(size = 14),
        plot.caption = element_text(size = 10, face = "italic", color = "gray45"))

# Stick plots together
p <- p1+p2+
  plot_layout(guides = 'collect')+
  plot_annotation(title = "Israel's Population", subtitle = "A regular map and a cartogram, a map where the geographic size is proportional\nto the population in that area. Data is from Israel's CBS 2017 report and is\naggregated at the district level",
                  theme = theme(plot.title = element_text(size = 18, face = "bold"),
                                plot.subtitle = element_text(size = 14, color = "gray35")))  &
  theme(text = element_text('IBM Plex Sans'),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        plot.margin = unit(c(2,2,4,2), "mm"))

ggsave("Code/20_population/20_map-img.png", p, height = 13, width = 8, units = "in", dpi = 300)