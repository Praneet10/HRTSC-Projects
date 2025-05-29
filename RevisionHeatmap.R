# Creating a Haryana Heat Map for number of Revisions raised to the Commission from each district.

# Loading required libraries.

library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(RColorBrewer) # Provides good color combinations.
library(styler)
library(ggtext)
library(showtext)
library(fontawesome)
library(here) #Helps to locate the fonts which R will use.
library(glue) #Creating captions which are a combination of text and images.

# Loading the data frame and shape file.

appeals_data <- read_excel("/Users/praneet97/Desktop/Report.xlsx") # This is the Excel.
head(appeals_data)
haryana_map <- st_read("/Users/praneet97/Desktop/haryana.kml")

# Check if column names are same.
View(haryana_map)
View(appeals_data)

# There are some mismatches in the names. The output should be character(0).

# Better to fix the names in Excel rather than the map.
appeals_data$`District Name` <- recode(appeals_data$`District Name`, "FARIDABAD" = "FAR|DABAD", "JIND" = "J|ND", "MAHENDERGARH" = "MAHENDRAGARH", "NUH" = "MEWAT", "PANIPAT" = "PAN|PAT", "SONIPAT" = "SON|PAT")
appeals_data <- subset(appeals_data, `District Name` != "Haryana HQ")

# Adding the count of Revisions in each district to the shape map.

haryana_map <- haryana_map |> left_join(appeals_data, by = c("Name" = "District Name"))

# Remove the 'Z' dimension if it's causing issues
haryana_map <- st_zm(haryana_map)

#Enabling showtext.

showtext_auto()

#Creating the Heatmap.

heatmap <- ggplot(data = haryana_map) +
  geom_sf(aes(fill = `Revisions Per District`)) +
  scale_fill_gradient(low="White", high="Red") +
  labs(title = "Revisions Per District", 
       fill = "Revisions Per District", 
       caption = paste("Data: AAS Dashboard"))+
  theme(
    axis.title = element_blank(), 
    axis.text= element_blank(), 
    axis.ticks=element_blank(),
    axis.line = element_blank(),
    plot.title=element_text(hjust=0.5, size=14, face="bold", family="Georgia"),
    plot.caption = ggtext::element_markdown(family="Georgia", face="bold", hjust=0.5),
    legend.title=element_blank())

#Display the Heatmap.
heatmap

#Saving the heatmap.

ggsave("haryana_heatmap.png", plot = heatmap, width=10, height=8, dpi=300)
getwd()



