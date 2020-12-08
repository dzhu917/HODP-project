library(stringi)
library(stringr)
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
      panel.background  = element_rect(fill="#FFFFFF", colour=NA),
      plot.background = element_rect(fill="#FFFFFF", colour="#d3d3d3"),
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

# load data
energy = read.csv("data/upperclassmen_houses.csv", stringsAsFactors = FALSE)

# Drop unused columns
energy = energy[,-(1:3)]
energy = energy[,-2]


# Fix House names

house.names = c(
  "Adams", "Cabot", "Currier", "Dunster" ,"Eliot",
  "Kirkland", "Leverett", "Lowell", "Mather",
  "Pforzheimer", "Quincy","Winthrop"
)

for (house in house.names){
  energy[str_detect(energy$X.2, house), 1] = house
}

# Remove cost and only keep energy use

indices = c()
for(i in 1:length(energy)) if(i %% 2==0 | i == 1){
  indices = c(indices, i)
}
energy = energy[indices]
  
# Remove commas in numbers

for (i in 2:length(energy)){
  energy[,i] = as.numeric(gsub(",", "", energy[,i], fixed = TRUE))
}

# drop months with missing data (future months)
energy = energy[,-(257:265)]

# Sum up energy use each month for each house
energy = aggregate(energy[-1], list(energy[,1]), FUN = sum, na.rm = TRUE)

# Transpose data so that we have one column per house
col.names = as.vector(energy[,1])
energy = energy[,-1]
energy <- as.data.frame(t(as.matrix(energy)))
colnames(energy) <- col.names

# Add month column
energy$month = seq(1,nrow(energy))
rownames(energy)<-energy$month

plot(Cabot~month, data = energy)

# do for Leverett, Dunster, Lowell, 
energy %>% 
  ggplot(aes(x = month, y = Leverett)) + 
  geom_line() + 
  labs(title = "Leverett House Electricity Usage", 
       x = "Months Since 2000",
       y = "Electricity (kWh)") + 
  scale_x_continuous(breaks = seq(from = 0, to = 255, by = 50)) + 
  geom_vline(xintercept = 255, 
             lty = "dashed", 
             color = "dodgerblue", 
             label = "Sept 2020") +
  geom_vline(xintercept = 170, 
             lty = "dashed", 
             color = "salmon", 
             label = "Sept 2020") +
  theme_hodp()

# monthly usage for all houses 
energy %>%
  ggplot() + 
  geom_point(aes(x = month, y = Adams), color = "blue", size = 0.2) + 
  geom_point(aes(x = month, y = Cabot), color = "red", size = 0.2) + 
  geom_point(aes(x = month, y = Currier), color = "green", size = 0.2) + 
  geom_point(aes(x = month, y = Dunster), color = "yellow", size = 0.2) + 
  geom_point(aes(x = month, y = Kirkland), color = "black", size = 0.2) + 
  geom_point(aes(x = month, y = Leverett), color = "darkmagenta", size = 0.2) + 
  geom_point(aes(x = month, y = Lowell), color = "wheat4", size = 0.2) + 
  geom_point(aes(x = month, y = Mather), color = "cadetblue2", size = 0.2) + 
  geom_point(aes(x = month, y = Pforzheimer), color = "red", size = 0.2) +
  geom_point(aes(x = month, y = Quincy), color = "coral", size = 0.2) +
  labs(title = "Upperclassmen House Electricity Usage", 
       x = "Months Since 2000",
       y = "Electricity (kWh)") + 
  scale_x_continuous(breaks = seq(from = 0, to = 255, by = 50)) + 
  geom_vline(xintercept = 255, 
             lty = "dashed", 
             color = "dodgerblue", 
             label = "Sept 2020") +
  theme_hodp()

# use the new energy 
energy_yr = read.csv("data/energy_yr.csv")

# yearly usage for all houses 
aggregate(energy_yr, list(energy_yr$year), FUN = mean) %>%
  ggplot()+
  geom_line(aes(x = year, y = Adams), color = "blue", size = 0.4) + 
  geom_line(aes(x = year, y = Cabot), color = "red", size = 0.4) + 
  geom_line(aes(x = year, y = Currier), color = "green", size = 0.4) + 
  geom_line(aes(x = year, y = Dunster), color = "yellow", size = 0.4) + 
  geom_line(aes(x = year, y = Kirkland), color = "black", size = 0.4) + 
  geom_line(aes(x = year, y = Leverett), color = "darkmagenta", size = 0.4) + 
  geom_line(aes(x = year, y = Lowell), color = "wheat4", size = 0.4) + 
  geom_line(aes(x = year, y = Mather), color = "cadetblue2", size = 0.4) + 
  geom_line(aes(x = year, y = Pforzheimer), color = "red", size = 0.4) +
  geom_line(aes(x = year, y = Quincy), color = "coral", size = 0.4) +
  labs(title = "Upperclassmen House Electricity Usage", 
       x = "Year",
       y = "Electricity (kWh)") + 
  theme_hodp()

geom_point(aes(x = month, y = Cabot), color = "red") + write.csv(energy, file = "data/energy.csv")
