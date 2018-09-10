#Loading the rvest package
library('rvest')

#Specifying the url for desired website to be scrapped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
#head(rank_data)

#convert rankings to numerical
rank_data <- as.numeric(rank_data)

#Using CSS selectors to scrap the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
#head(title_data)

#Using CSS selectors to scrap the description section
description_data_html <- html_nodes(webpage,'p:nth-child(4)')

#Converting the description data to text
description_data <- html_text(description_data_html)

#change to 2:101
description_data <- description_data[2:101]

#data preprocessing: removing \n
description_data <- gsub("\n    ","",description_data)

#Using CSS selectors to scrap the Movie runtime section
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')

#Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)

#Data-Preprocessing: removing mins and converting it to numerical
runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)

#Using CSS selectors to scrap the Movie genre section
genre_data_html <- html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data <- html_text(genre_data_html)

#Data-Preprocessing: removing \n, removing excess spaces, taking only the first genre of each movie
genre_data<-gsub("\n","",genre_data)
genre_data<-gsub(" ","",genre_data)
genre_data<-gsub(",.*","",genre_data)

#Converting each genre from text to factor
genre_data<-as.factor(genre_data)

#Using CSS selectors to scrap the IMDB rating section
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- html_text(rating_data_html)

#Data-Preprocessing: converting ratings to numerical
rating_data<-as.numeric(rating_data)

#Using CSS selectors to scrap the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- html_text(votes_data_html)

#Data-Preprocessing: removing commas, converting votes to numerical
votes_data<-gsub(",","",votes_data)
votes_data<-as.numeric(votes_data)

#Using CSS selectors to scrap the directors section
directors_data_html <- html_nodes(webpage,'p:nth-child(5) a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#Data-Preprocessing: converting directors data into factors
directors_data<-as.factor(directors_data)

#Using CSS selectors to scrap the actors section
##THIS ONE MIGHT NOT WORK
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')

#Converting the gross actors data to text
actors_data <- html_text(actors_data_html)

#Data-Preprocessing: converting actors data into factors
actors_data<-as.factor(actors_data)

##metascore might be more tricky
#Using CSS selectors to scrap the metascore section
metascore_data_html <- html_nodes(webpage,'.metascore')

#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)

#Data-Preprocessing: removing extra space in metascore
metascore_data<-gsub(" ","",metascore_data)
metascore_data<-as.numeric(metascore_data)

#don't have metascore for 48 and 57 so do a for loop i guess
#this is not the best way to do this but i dont know how to make it better so im going with it
for (i in c(48,57)){
	a<-metascore_data[1:(i-1)]
	b<-metascore_data[i:length(metascore_data)]
	metascore_data<-append(a,list("NA"))
	metascore_data<-append(metascore_data,b)
}
#back to numeric
metascore_data <- as.numeric(metascore_data)

#Using CSS selectors to scrap the gross revenue section
gross_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(5)')

#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data<-gsub("M","",gross_data)
gross_data<-substring(gross_data,2,6)
gross_data<-as.numeric(gross_data)

#Filling missing entries with NA
for (i in c(15,48,70,71,86,87,100)){
	a<-gross_data[1:(i-1)]
	b<-gross_data[i:length(gross_data)]
	gross_data<-append(a,list("NA"))
	gross_data<-append(gross_data,b)
	gross_data<-as.numeric(gross_data)
}
#converting gross to numerical
gross_data<-gross_data[1:100]

##COMBINE
movies_df <- data.frame(Rank = rank_data, Title = title_data, Description = description_data,
  Runtime = runtime_data, Genre = genre_data, Rating = rating_data, Metascore = metascore_data,
  Votes = votes_data, GrossEarningsInMil = gross_data, Director = directors_data, Actor = actors_data)

##ANALYZE
library(ggplot2)

qplot(data = movies_df, Runtime, fill = Genre, bins = 30)
#which movie from which genre had the longest runtime?
#genre:
movies_df$Genre[which.max(movies_df$Runtime)]
#title:
movies_df$Title[which.max(movies_df$Runtime)]

ggplot(movies_df,aes(x=Runtime,y=Rating))+
  geom_point(aes(size=Votes,col=Genre))
#which genre has the highest votes in runtime from 130-160?
movieRatingsByGenreWithinSpecificRuntime <- movies_df[movies_df$Runtime > 130 & movies_df$Runtime < 160,] %>%
select(Genre, Rating) %>%
group_by(Genre) %>%
summarise(Total_Rating = sum(Rating))
movieRatingsByGenreWithinSpecificRuntime[order(-movieRatingsByGenreWithinSpecificRuntime$Total_Rating),]

ggplot(movies_df,aes(x=Runtime,y=Gross_Earning_in_Mil))+
  geom_point(aes(size=Rating,col=Genre))
#which genre has the highest average gross earnings?
movieGrossEarningsByGenre <- movies_df %>%
select(Genre, GrossEarningsInMil) %>%
group_by(Genre) %>%
summarise(Average_Earnings = mean(GrossEarningsInMil, na.rm=TRUE))
movieGrossEarningsByGenre[order(-movieGrossEarningsByGenre$Average_Earnings),]