install.packages("ggplot2")
install.packages("tm")
install.packages("text2vec")
install.packages("cluster")
library(ggplot2)
library(tidyverse)
library(tm)
library(text2vec)
library(cluster)

data<-read_csv("/Users/joe/Downloads/MN-DS-news-classification.csv")
data <- na.omit(data)

data$date <- as.Date(data$date)
data <- data %>%
  arrange(date) 
data$month <- format(data$date, "%b%y")  # Example: Jan17, Feb17, etc.

# Aggregate the number of articles per month and cluster
aggregated_data <- data %>%
  group_by(month, category_level_1) %>%
  summarise(article_count = n())
aggregated_data <- aggregated_data %>%
  arrange(month)
aggregated_data$month <- as.Date(aggregated_data$month)

customdatacustom_colors <- c(
  "#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", 
  "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", 
  "#ccebc5", "#ffed6f","#CC79A7", "#377eb8", "#4daf4a", "#ff7f00", "#ffff33"
)

# Create the stacked line plot using ggplot2
ggplot(aggregated_data, aes(x = month, y = article_count, fill = category_level_1, group = category_level_1)) +
  geom_area(alpha = 0.6) +  # Stacked line chart using geom_area
  scale_fill_manual(values = custom_colors) +  # Apply a color palette
  labs(title = "Article Distribution by Category Over Time",
       x = "Month",
       y = "Number of Articles",
       fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
