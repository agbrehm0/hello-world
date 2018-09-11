# Austin Water Quality Case Study

# load libraries that we'll need
library(tidyverse)
library(stringr)
library(lubridate)

# read in the dataset
water <- read_csv('http://594442.youcanlearnit.net/austinwater.csv')

# let's take a look at what we have
glimpse(water)

# First, let's get rid of a lot of columns that we don't need
# I'm going to do that by building a new tibble with just
# siteName, siteType, parameter, result and unit

water <- tibble('siteName'=water$SITE_NAME,
                'siteType'=water$SITE_TYPE,
                'sampleTime'=water$SAMPLE_DATE,
                'parameterType'=water$PARAM_TYPE,
                'parameter'=water$PARAMETER,
                'result'=water$RESULT,
                'unit'=water$UNIT)

glimpse(water)

# Now let's start finding the rows that we need.
# First, we need pH.  I might start by trying to look at all unique parameter names

unique(water$parameter)

# but that's way too long... what if we try searching for names that contain PH?

unique(water[which(str_detect(water$parameter,'PH')),]$parameter)

# still a mess... let's backtrack and look at parameter types

unique(water$parameterType)

# OK, what if I filter this down to look only at parameter types of Alkalinity/Hardness/pH
# and Conventionals

filtered_water <- subset(water,(parameterType=='Alkalinity/Hardness/pH') |
                                  parameterType=='Conventionals')

# Notice that this is much smaller in size.  let's check what parameters we have now

unique(filtered_water$parameter)

# I want only two of these, (discuss PH and temp choices), so let's filter those

filtered_water <- subset(filtered_water, ((parameter=='PH') |
                                            (parameter=='WATER TEMPERATURE')))

glimpse(filtered_water)

# Let's take a look at the data a different way
summary(filtered_water)

# It would be helpful to convert some of these to factors
filtered_water$siteType <- as.factor(filtered_water$siteType)
filtered_water$parameterType <- as.factor(filtered_water$parameterType)
filtered_water$parameter <- as.factor(filtered_water$parameter)
filtered_water$unit <- as.factor(filtered_water$unit)

summary(filtered_water)

# And sampleTime should be a date/time object
filtered_water$sampleTime <- mdy_hms(filtered_water$sampleTime)

summary(filtered_water)

# Why are some of these measurements in feet?
subset(filtered_water,unit=='Feet')

# Looks like that is supposed to be Farenheit
convert <- which(filtered_water$unit=='Feet')
filtered_water$unit[convert] <- 'Deg. Fahrenheit'

summary(filtered_water)

# What about the MG/L?
subset(filtered_water,unit=='MG/L')
subset(filtered_water,unit=='MG/L' & parameter=='PH')

convert <- which(filtered_water$unit=='MG/L' & filtered_water$parameter=='PH')
filtered_water$unit[convert] <- 'Standard units'

subset(filtered_water,unit=='MG/L')
subset(filtered_water,unit=='MG/L' & filtered_water$result>70)
convert <- which(filtered_water$unit=='MG/L' & filtered_water$result>70)
filtered_water$unit[convert] <- 'Deg. Fahrenheit'

subset(filtered_water,unit=='MG/L')
convert <- which(filtered_water$unit=='MG/L')
filtered_water$unit[convert] <- 'Deg. Celsius'

summary(filtered_water)

# Let's just take a quick and dirty look at all of our results
ggplot(filtered_water,mapping=aes(x=sampleTime, y=result)) +
  geom_point()

# There's clearly one large outlier
subset(filtered_water,result>1000000)

# I don't know how to correct that, so I'm going to remove it.  I also want to remove the NA result
remove <- which(filtered_water$result>1000000 | is.na(filtered_water$result))
filtered_water <- filtered_water[-remove,]

summary(filtered_water)

# Still some very high values, so let's repeat
subset(filtered_water,result>1000)

remove <- which(filtered_water$result>1000 | is.na(filtered_water$result))
filtered_water <- filtered_water[-remove,]

summary(filtered_water)

# That looks better.  Let's drill into some boxplots now

ggplot(data=filtered_water, mapping = aes(x=unit,y=result)) + geom_boxplot()

# Those Celsius values over 60 should probably  be Fahrenheit
# Because 60 degrees Celsius is 140 degrees Fahrenheit!

convert <- which(filtered_water$result>60 & filtered_water$unit=='Deg. Celsius')
filtered_water$unit[convert] <- 'Deg. Fahrenheit'

# Let's look at the boxplots again

ggplot(data=filtered_water, mapping = aes(x=unit,y=result)) + geom_boxplot()

# Let's find our Fahrenheit values in the dataset
fahrenheit <- which(filtered_water$unit=='Deg. Fahrenheit')

# And convert them to Celsius
filtered_water$result[fahrenheit] <- (filtered_water$result[fahrenheit] - 32) * (5/9)

# Now how do our boxplots look?
ggplot(data=filtered_water, mapping = aes(x=unit,y=result)) + geom_boxplot()

# We just need to fix up the unit values
filtered_water$unit[fahrenheit] <- 'Deg. Celsius'

# And check the plots again
ggplot(data=filtered_water, mapping = aes(x=unit,y=result)) + geom_boxplot()

# Let's look at a final summary of the data
summary(filtered_water)

# There are some empty factor levels in there, let's get rid of them
filtered_water$unit <- droplevels(filtered_water$unit)
summary(filtered_water)

# Start the spread by looking at the data
summary(filtered_water)

# Get rid of parameterType and unit
filtered_water <- filtered_water[,-c(4,7)]

filtered_water

# Try a spread
filtered_water_wide <- spread(filtered_water,parameter,result)

# What are those duplicate rows all about?
# Let's look at a few
filtered_water[c(49274, 49342,49219,49284),]

# It looks like there are indeed duplicate measurements in the dataset
# Let's find all of them.  I'll start by building a tibble that excludes the result data
dupe_check<-filtered_water[,-5]

# And now lets find which records are duplicates
dupes <- which(duplicated(dupe_check))

# And remove those
filtered_water <- filtered_water[-dupes,]

# and then retry the spread
filtered_water_wide <- spread(filtered_water,parameter,result)
filtered_water_wide

# and i'll just clean up those column names
colnames(filtered_water_wide)[4] <- 'pH'
colnames(filtered_water_wide)[4] <- 'temperature'

