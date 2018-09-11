# Social Security Disability Case Study

# Load the tidyverse
library(tidyverse)
library(lubridate)
library(stringr)

# Read in the coal dataset
ssa <- read_csv("http://594442.youcanlearnit.net/ssadisability.csv")

# Take a look at how this was imported
glimpse(ssa)

# Make the dataset long
ssa_long <- gather(ssa, month, applications, -Fiscal_Year)

# And what do we get?
print(ssa_long, n=20)

# Split the month and application type
ssa_long <- separate(ssa_long, month, c("month", "application_method"), sep="_")

# What does that look like?
print(ssa_long, n=20)

# What values do we have for months?
unique(ssa_long$month)

# Convert month to standard abbreviations
ssa_long$month <- substr(ssa_long$month,1,3)

# What values do we now have for months and years?
unique(ssa_long$month)
unique(ssa_long$Fiscal_Year)

# Convert Fiscal_Year from alphanumeric strings to actual years
ssa_long$Fiscal_Year <- str_replace(ssa_long$Fiscal_Year, "FY", "20")

# What values do we now have for months?
unique(ssa_long$Fiscal_Year)

# Build a date string using the first day of the month
paste('01', ssa_long$month, ssa_long$Fiscal_Year)

ssa_long$Date <- dmy(paste("01", ssa_long$month, ssa$Fiscal_Year))

# What do those look like?
unique(ssa_long$Date)

# Government fiscal years differ from calendar years in that they are named for the
# calendar year where they end.  The government fiscal year begins in October.
# So October 2016 is actually in FY17.
#
# We need to convert these to calendar dates before we try to plot them, so we 
# need to find months >=10 and subtract one year from them

# Let's find the affected rows
advanced_dates <- which(month(ssa_long$Date)>=10)

# And then decerement the years by one
year(ssa_long$Date[advanced_dates]) <- year(ssa_long$Date[advanced_dates]) - 1

# Let's look at where we are
summary(ssa_long)

# Remove Fiscal_Year and month columns
ssa_long$Fiscal_Year <- NULL
ssa_long$month <- NULL

# Convert application_method to a factor
ssa_long$application_method <- as.factor(ssa_long$application_method)

# How does that look
summary(ssa_long)

# Widen the final dataset
ssa <- spread(ssa_long, application_method, applications)

# And take a look
print(ssa,n=20)

# Add a column for the percentage of applications that were online
ssa$online_percentage <- ssa$Internet/ssa$Total*100

# Plot the results
ggplot(data=ssa, mapping=aes(x=Date,y=online_percentage)) +
  geom_point()
