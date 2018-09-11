#Coal Consumption Case Study

# load the tidyverse
library(tidyverse)

# read the coal dataset
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv")
glimpse(coal)

# looking at the original csv file, i can see that the first two rows aren't actually used
# skip first two lines
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv", skip=2)
glimpse (coal)

# rename first column as region
colnames(coal)[1] <- "region"
summary(coal)

# this dataset seems too wide so i'm gonna change it so it only has 3 columns using the gather function
?gather
coal_long <- gather(coal, 'year', 'col_consumption', -region) #-region means i'm gonna be gathering all the columns except the region

# convert years to integers
coal_long$year <- as.integer(coal_long$year)

$ convert coal consumption to numeric
coal_long$coal_consumption <- as.numeric(coal_long$coal_consumption)
summary(coal_long)

# look at region values - they contain both continents and countries
unique(coal_long$region)

# create vector "noncountry" values that appear in the region variable
noncountries <- c("North America", "Central & South America", "Antarctica", "Europe", "Eurasia", 
                  "Middle East", "Africa", "Asia & Oceania", "World")

# look for matches
matches <- which(!is.na(match(coal_long$region, noncountries)))

# create a tibble of country values
coal_country <- coal_long[-matches,]

# create a tibble of regional values
coal_region <- coal_long[matches,]

# check them out
unique(coal_region$region)
unique(coal_country$region)

# now that we've cleaned it a bit, let's see if we can visualize it
# since it's over time we're gonna use a line plot
ggplot(data=coal_region, mapping=aes(x=year, y=coal_consumption)) +
  geom_line()

# let's get a separate line for each region
ggplot(data=coal_region, mapping=aes(x=year, y=coal_consumption)) +
  geom_line(mapping=aes(color=region))

