# Load necessary libraries
# Step 0: HODP Theme
library(tidyverse)
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('hrbrthemes')) install.packages('hrbrthemes'); library(hrbrthemes)
if (!require('magick')) install.packages('magick'); library(magick)
if (!require('plotly')) install.packages('plotly'); library(plotly)
logo <- image_read("logo.png")
# Legend: https://stackoverflow.com/questions/14622421/how-to-change-legend-title-in-ggplot
monochrome <- c('#760000', '#BE1E26', '#D84742', '#FF6B61', '#FF9586')
primary <- c('#EE3838', '#FA9E1C', '#78C4D4', '#4B5973', '#E2DDDB', '#cb2a29')
sidebysidebarplot <- c("#ef3e3e", "#2c3e50")
theme_hodp <- function () { 
  theme_classic(base_size=12, base_family="Helvetica") +
    theme(
      #panel.background  = element_rect(fill="#FFFFFF", colour=NA),
      #plot.background = element_rect(fill="#FFFFFF", colour="#d3d3d3"),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      plot.title = element_text(size=24,  family="Helvetica", face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0)),
      plot.subtitle = element_text(size=18,  family="Helvetica", color="#717171", face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0)),
      plot.caption = element_text(size=8,  family="Helvetica", hjust = 1),
      axis.text.x =element_text(size=10,  family="Helvetica"),
      axis.title.x =element_text(size=14, family="Helvetica", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14, family="Helvetica", angle=90, face ='bold'),
      legend.title=element_text(size=10, family="Helvetica"), 
      legend.text=element_text(size=10, family="Helvetica"),
      legend.position = "bottom",
      axis.ticks = element_blank()
    )
}


energy = read.csv("data/energy.csv")

aggregate(energy, list(energy$year), FUN = mean) %>%
  ggplot()+
  geom_line(aes(x = year, y = Adams), color = "blue", size = 0.4) + 
  geom_line(aes(x = year, y = Cabot), color = "red", size = 0.4) + 
  geom_line(aes(x = year, y = Currier), color = "green", size = 0.4) + 
  geom_line(aes(x = year, y = Dunster), color = "yellow", size = 0.4) + 
  geom_line(aes(x = year, y = Kirkland), color = "`black", size = 0.4) + 
  geom_line(aes(x = year, y = Leverett), color = "darkmagenta", size = 0.4) + 
  geom_line(aes(x = year, y = Lowell), color = "wheat4", size = 0.4) + 
  geom_line(aes(x = year, y = Mather), color = "cadetblue2", size = 0.4) + 
  geom_line(aes(x = year, y = Pforzheimer), color = "red", size = 0.4) +
  geom_line(aes(x = year, y = Quincy), color = "coral", size = 0.4) +
  labs(title = "Upperclassmen House Electricity Usage", 
       x = "Year",
       y = "Electricity (kWh)") + 
  theme_hodp()
