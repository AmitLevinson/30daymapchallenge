library(ggplot2)
library(readr)
library(maps)
library(cowplot)
library(ggforce)
library(extrafont)
library(emoGG)

us_states <- map_data("state")
dat <- read_csv("data/01_chipotle_stores.csv")

# Highlight specific point
point <- data.frame(lon = -77.1378, lat = 39.07526, label = "My most frequently visited location throughout my stay in the US", title = "865 Rockville Pike, Rockville, MD")

ggplot(data = us_states, mapping = aes(x = long, y = lat, group = group))+
  geom_polygon(fill = "grey85",color = "white", size = 0.5)+
  geom_point(data = dat, aes(x = longitude, y = latitude, group = 1), color = "#451400", size = 0.3)+
  coord_map(projection = 'albers', lat0 = 39, lat1 = 45)+
  theme_map()+
  labs(title = "Chipotle locations across the US", caption = "#30DayMapChallenge | Day: 01\nData: Kaggle | @Amit_Levinson")+
  geom_mark_circle(data = point, mapping = aes(x = lon, y= lat, label = title, description = label, group = 1), radius = unit(0.01, "mm"),
                    label.family = c("Segoe UI Black", "Segoe UI"), label.fontsize = c(10,9), label.colour = c("#451400", "#451400"), con.border = "none",
                   con.colour = "grey55", expand = unit(0.01, "mm"), label.buffer = unit(30, "mm"), label.fill = "transparent", color = "white")+
  geom_emoji(data = point, aes(x = lon, y = lat, group = 1), emoji = "1f4cd", size = 0.02)+
  theme(
    plot.title = element_text(color = "#451400", hjust = 0.5, size = 24, family = "Segoe UI Black"),
    plot.caption = element_text(color = "#451400", hjust = 1, size = 8, family = "Segoe UI Light")
  )

ggsave("01_points/01_points.png", width = 10, height = 8)

#Data set from:
# https://www.kaggle.com/navneethc/chipotle