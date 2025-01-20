install.packages("viridis")
install.packages("patchwork")
library(patchwork)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(viridis)
library(gridExtra)
# Read the data
data <- read.csv("/path/to/your/file/MN-DS-news-classification.csv")
data_sorted <- na.omit(newsdata)
# Convert the date to Date format
data_sorted$date <- as.Date(data_sorted$date)


# Aggregate by week and source to calculate publish counts
source_counts_by_date <- data_sorted %>%
  group_by(date, source) %>%
  tally()

top_sources <- source_counts_by_date %>%
  group_by(source) %>%
  summarise(total_publish_count = sum(n)) %>%
  top_n(10, total_publish_count) %>%
  pull(source)

color_palette <- scale_color_manual(values = c(brewer.pal(10, "Paired"), "blue"))
# Plot the scatter plot, adjusting point size based on the publish count
source_counts_by_date$source <- ifelse(source_counts_by_date$source %in% top_sources, source_counts_by_date$source, "Others")

ggplot(source_counts_by_date, aes(x = date, y = n, color = source, size = n)) +
  geom_point(alpha = 0.6) +  # Set transparency
  scale_size_continuous(range = c(1, 18)) +   # Adjust size range
  labs(title = "Source Publish Counts over Time", x = "Date", y = "Publish Count",
       caption = "Multilabeled News Dataset")+
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +  # Monthly intervals on x-axis
  color_palette +  # Apply the custom color palette
  theme(legend.position = "right")  # remove legend to avoid too many colors
ggsave("/Users/joe/Documents/0.Data science/Data visualisation/scatter.png", width = 10, height = 6, units = "in", dpi = 300)  # Save as PNG with 300 dpi

