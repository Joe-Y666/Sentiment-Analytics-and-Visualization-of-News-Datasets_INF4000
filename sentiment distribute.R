# Install necessary libraries if not already installed
install.packages("tidyverse", "textblob", "syuzhet")
install.packages('syuzhet')
install.packages('textblob')
# Load the libraries
library(tidyverse)
library(textblob)
library(syuzhet)
library(RColorBrewer)

# Load the data
data<-read_csv("/Users/joe/Downloads/MN-DS-news-classification.csv")
data <- na.omit(data)
# Perform sentiment analysis using syuzhet package
sentiment_scores <- get_nrc_sentiment(data$content)

# Create a sentiment column (positive, negative, neutral)
data$sentiment <- ifelse(sentiment_scores$positive > sentiment_scores$negative, "positive", 
                         ifelse(sentiment_scores$positive < sentiment_scores$negative, "negative", "neutral"))

# Create a summary table of sentiment distribution by category_level_1
sentiment_counts <- data %>%
  group_by(category_level_1, sentiment) %>%
  summarize(count = n()) %>%
  ungroup()

# Plot the stacked bar chart
pdf("/Users/joe/Downloads/sentiment2.pdf", width = 8, height = 6)
ggplot(sentiment_counts, aes(x = category_level_1, y = count, fill = sentiment)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  labs(title = "Sentiment Distribution by Category (Level 1)", x = "Category Level 1", y = "Number of Articles", caption = "Multilabeled News Dataset") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), 
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 15),
        plot.caption = element_text(size = 11)) +
  scale_fill_manual(values = c("#56B4E9","#009E73","#E69F00"))+# Customize colors
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "white", size = 3.5)
dev.off()
