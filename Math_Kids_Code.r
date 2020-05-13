# Data Cleaning-----------------------------------------------------------------------------------------
# Load Libraries
library(tidyverse)
library(stringr)
library(xts)
library(ggplot2)

# Load main dataset
data <- read.csv("googleplaystore.csv")

# Load subdata dataset
subdata <- read.csv("googleplaystore_user_reviews.csv")

# Create clean dataset
data.clean <- data %>%
  mutate(
    # Eliminate some characters to transform Installs to numeric
    Installs = gsub("\\+", "", as.character(Installs)),
    Installs = as.numeric(gsub(",", "", Installs)),
    # Transform reviews to numeric
    Reviews = as.numeric(as.character(Reviews)),
    # Remove currency symbol from Price, change it to numeric
    Price = as.numeric(gsub("\\$", "", as.character(Price)))
  ) %>%
  filter(
    # Two apps had type as 0 or NA, they will be removed 
    Type %in% c("Free", "Paid")
  )

# Create subdata clean dataset
subdata.clean <- subdata %>%
  filter(
  Sentiment %in% c("Negative", "Neutral", "Positive")
  )

# Rounds reviews from tenths to whole numbers
index_1 <- which(data.clean$Rating %in% c("1.0", "1.1", "1.2", "1.3", "1.4"))
index_2 <- which(data.clean$Rating %in% c("1.5", "1.6", "1.7", "1.8", "1.9", "2.0", "2.1", "2.2", "2.3", 
  "2.4"))
index_3 <- which(data.clean$Rating %in% c("2.5", "2.6", "2.7", "2.8", "2.9", "3.0", "3.1", "3.2", "3.3", 
  "3.4"))
index_4 <- which(data.clean$Rating %in% c("3.5", "3.6", "3.7", "3.8", "3.9", "4.0", "4.1", "4.2", "4.3", 
  "4.4"))
index_5 <- which(data.clean$Rating %in% c("4.5", "4.6", "4.7", "4.8", "4.9", "5.0"))
data.clean$Rating[index_1] <- "1"
data.clean$Rating[index_2] <- "2"
data.clean$Rating[index_3] <- "3"
data.clean$Rating[index_4] <- "4"
data.clean$Rating[index_5] <- "5"
data.clean$Rating <- factor(data.clean$Rating)

# Merge the data set onto the subdata set by app, giving the subdata set information about each app
mergedata.clean <- merge(data.clean, subdata.clean, by = "App")

# Removes duplicate entries from data.clean
data.clean <- data.clean %>%
  distinct()

# Removes duplicate entries from mergedata.clean
mergedata.clean <- mergedata.clean %>%
  distinct()

# Set categories not in the top 10 to 'other' in data.clean
index_nottop10 <- which(data.clean$Category %in% c("ART_AND_DESIGN", 
  "AUTO_AND_VEHICLES", "BEAUTY", "BOOKS_AND_REFERENCE", "BUSINESS", "COMICS", 
  "DATING", "EDUCATION", "ENTERTAINMENT", "EVENTS", "FINANCE", 
  "FOOD_AND_DRINK", "HEALTH_AND_FITNESS", "HOUSE_AND_HOME", 
  "LIBRARIES_AND_DEMO", "LIFESTYLE", "MAPS_AND_NAVIGATION", "MEDICAL", 
  "PARENTING", "PERSONALIZATION", "SHOPPING", "SPORTS", "WEATHER")) 
data.clean$Category[index_nottop10] <- c("OTHER")
data.clean$Category <- factor(data.clean$Category, levels = levels(addNA(data.clean$Category)), 
  labels = c(levels(data.clean$Category), "OTHER"), exclude = NULL)

# Order category factor by highest number of installs to lowest with 'other' at the end in data.clean
data.clean$Category <- factor(data.clean$Category, levels = c("GAME", "COMMUNICATION", 
  "SOCIAL", "PRODUCTIVITY", "TOOLS", "FAMILY", "PHOTOGRAPHY", 
  "TRAVEL_AND_LOCAL", "VIDEO_PLAYERS", "NEWS_AND_MAGAZINES", "OTHER"))

# Set categories not in the top 10 to 'other' in mergedata.clean
index_nottop10 <- which(mergedata.clean$Category %in% c("ART_AND_DESIGN", 
  "AUTO_AND_VEHICLES", "BEAUTY", "BOOKS_AND_REFERENCE", "BUSINESS", "COMICS", 
  "DATING", "EDUCATION", "ENTERTAINMENT", "EVENTS", "FINANCE", 
  "FOOD_AND_DRINK", "HEALTH_AND_FITNESS", "HOUSE_AND_HOME", 
  "LIBRARIES_AND_DEMO", "LIFESTYLE", "MAPS_AND_NAVIGATION", "MEDICAL", 
  "PARENTING", "PERSONALIZATION", "SHOPPING", "SPORTS", "WEATHER")) 
mergedata.clean$Category[index_nottop10] <- c("OTHER")
mergedata.clean$Category <- factor(mergedata.clean$Category, levels = 
  levels(addNA(mergedata.clean$Category)), labels = c(levels(mergedata.clean$Category), "OTHER"), 
  exclude = NULL)

# Order category factor by highest number of installs to lowest with 'other' at the end in mergedata.clean
mergedata.clean$Category <- factor(mergedata.clean$Category, levels = c("GAME", 
  "COMMUNICATION", "SOCIAL", "PRODUCTIVITY", "TOOLS", "FAMILY", "PHOTOGRAPHY", 
  "TRAVEL_AND_LOCAL", "VIDEO_PLAYERS", "NEWS_AND_MAGAZINES", "OTHER"))

# Remove unused levels from Sentiment
mergedata.clean$Sentiment <- factor(mergedata.clean$Sentiment, 
  levels(droplevels(mergedata.clean$Sentiment)))

#Remove unused levels from Type
mergedata.clean$Type <- factor(mergedata.clean$Type, levels(droplevels(mergedata.clean$Type)))


# Graphs Section 2-----------------------------------------------------------------------------------------

# Figure 1: Installs/Category
# Find sum of all installs per category
temp <- data.clean %>% 
  group_by(Category) %>%
  summarize(totalInstalls = sum(Installs))
ggplot(data = temp, aes(x = Category, y = totalInstalls, fill = Category)) + geom_bar(stat = "identity") + 
  xlab("Category") + ylab("Installs") + theme(axis.text.x=element_text(angle=45, hjust=1))

# Figure 2: Installs/Price/Category
ggplot(data = data.clean, aes(x = Price, y = Installs, fill = Category)) + geom_jitter(position = 
  position_jitter(w=0.1, h=0.1)) + geom_smooth() + ggtitle("Installs vs Price") + 
  scale_y_continuous(name = "Installs", trans = 'log10', limits = c(-0.001, 1000000000)) + 
  scale_x_sqrt(limits = c(0,32)) + facet_wrap(data.clean$Category,)

# Figure 3: Installs/Rating/Category
ggplot(data = subset(data.clean, Rating != "NaN"), aes(x = Rating, y = Installs, fill = Category)) + 
  geom_boxplot() + ggtitle("Installs per Rating") + scale_y_continuous(name = "Installs", trans = 'log10') + 
  facet_wrap(subset(data.clean, Rating != "NaN")$Category, )


# Figure 4: Category/Sentiment
ggplot(data = mergedata.clean, aes(x = Category, y = Sentiment_Polarity, fill = Category)) + 
  geom_violin() + theme(axis.text.x=element_text(angle=45, hjust=1)) + ylab("Polarity from -1 to 1")


# Graphs Section 1----------------------------------------------------------------------------------------

# Figure 5: Sentiment Polarity ratio
par(mar = c(4, 4, 4, 7))
barplot(prop.table(table(mergedata.clean$Sentiment, mergedata.clean$Type), 2), col = c("Red", "gray27", 
  "DarkGreen"), ylab = "Sentiment Polarity Percentage", args.legend = list(x = 3.5, y = 1, bty = "n"), 
  legend.text = TRUE)

#Figure 6: Sentiment Polarity per review
ggplot(data = mergedata.clean, aes(x = Type, y = Sentiment_Polarity, fill = Type)) + geom_violin() + 
  ylab("Sentiment Polarity from -1 to 1")

#Figure 7: Review/Type/Category
ggplot(data = subset(data.clean, Rating != "NaN"), aes(x = Rating, y = Reviews, fill = Type)) + 
  geom_boxplot() + ggtitle("Reviews per Rating between Type") + scale_y_continuous(name = 
  "Reviews", trans = 'log10') + facet_wrap(subset(data.clean, Rating != "NaN")$Category, )
