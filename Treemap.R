install.packages("treemap")
library(treemap)

library(tidyverse)
library(tm)
library(ggplot2)
library(RColorBrewer)
data<-read_csv("/Users/joe/Downloads/MN-DS-news-classification.csv")
data <- na.omit(data)
data$date <- as.Date(data$date, format="%Y-%m-%d")
data$year <- year(data$date)
data$month <- month(data$date)
data$year_month <- format(data$date, "%Y-%m")

monthly_counts <- data %>%
  group_by(year_month) %>%
  summarise(article_count = n())

data_summary <- data %>%
  group_by(category_level_1, category_level_2) %>%
  summarise(article_count = n()) %>%
  ungroup()

pdf("/Users/joe/Downloads/treemap.pdf", width = 8, height = 6)  # Set the file name and dimensions
treemap(data_summary,
        index = c("category_level_1", "category_level_2"),  # Layers of categories
        vSize = "article_count",  # Size of the rectangles based on article count
        vColor = "article_count",  # Color based on article count
        draw = TRUE,  # Draw the tree map
        title = "Article Counts by Category",
        palette = colors,  # Apply the color palette
        border.col = "black",  # black borders between boxes
        border.lw = 3,  # Increase the border width
        fontcolor.labels = c("black", "white"),  # Set category_level_1 labels to white, category_level_2 labels to black
        fontsize.labels = c(14,10),  # Set label font size
        position.legend = "none",  # Remove legend
        align.labels = list(c("center", "top"), c("center", "center")),  # Align category_level_1 text to the top of the box
        fontface.labels = c("bold", "plain"),  # Bold for category_level_1 labels
        draw.labels = TRUE  # Draw the labels
)
dev.off()

