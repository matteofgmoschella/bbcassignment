# Load packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(bbplot)
library(reshape2)

# Import data, fix date format
data <- read.csv("article-Devices.csv")
data$Date <- as.character(data$Date)
data$Date <- str_sub(data$Date, 5, 24)
data$Date <- as_datetime(data$Date, format="%d %b %Y %H:%M:%S")
data$Date <- as.POSIXct(data$Date)

# Question 1:
# Calculate proportion of views from mobile
mob_tablet <- data %>%
  filter(Total != 0) %>% 
  mutate(prop_mobile = (Mobile / Total) * 100) %>% 
  mutate(prop_tablet = (Tablet / Total) * 100)

# Plot mobile and tablet proportions
plotdata <- mob_tablet %>%
  melt(id.vars = "Date")
plotdata <- plotdata %>% 
  filter(variable == "prop_mobile" | variable == "prop_tablet") 

ggplot(plotdata, aes(x = Date, y = value/100, colour = variable)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("#FAAB18", "#1380A1")) +
  bbc_style() +
  labs(title = "Proportion of users on mobile/tablet devices")

# Work out proportion
summary <- data %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  mutate(prop_mobtab = ((Tablet + Mobile) / Total) * 100)
paste("Total number of users on Mobile or Tablet: ",summary$Tablet + summary$Mobile,sep="")
paste("Proportion of users on Mobile or Tablet: ",format(round(summary$prop_mobtab, 2), nsmall = 2),"%",sep="")
  
# Question 2:
# Calculate the mean mobile pageviews
mean <-mean(data$Mobile)

# Filter rows that are greater than mean
textfile <- data %>% 
  filter(Mobile > mean)

# Write to file
write.table(textfile,"mob_exceeds.txt",sep="\t",row.names=FALSE)
