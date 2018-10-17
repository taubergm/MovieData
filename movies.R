if (!require(poweRlaw)) {
  install.packages("VGAM", repos="http://cran.us.r-project.org")
  install.packages("poweRlaw", repos="http://cran.us.r-project.org")
}
library("poweRlaw")
if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
library("ggplot2")
if (!require(igraph)) {
  install.packages("igraph", repos="http://cran.us.r-project.org")
}
library("igraph")
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
library(plyr)


factor2numeric <- function(f)
{
  if(!is.factor(f)) stop("the input must be a factor")
  as.numeric(levels(f))[as.integer(f)]
}


workingDir = '/Users/michaeltauberg/powerlaw/movies'
setwd(workingDir)



csvName = "movies.csv"
data_name = "movies"
dt = read.csv(csvName)
dt = dt[!duplicated(dt[,c('wiki_ref')], fromLast=FALSE),] 
movies = dt[grep("United States", dt$country), ] # US movies only - misses gladiator - fix country?
movies = movies[grep("English", movies$language), ]
movies$gross = as.numeric(as.character(movies$gross))
movies = movies[order(movies$gross, decreasing=TRUE),]
movies = movies[!duplicated(movies[,c('name','gross')], fromLast=FALSE),] 


# set to integer - gross is too big for integers
movies$runtime = strtoi(movies$runtime)

#movies$gross = factor2numeric(movies$gross)
#movies$runtime = factor2numeric(movies$runtime)

# still missing force awakens, infinity war, black panther, deathly hallows, last jedi
# plot top 20
top_movies = movies[1:50,]
top_movies = top_movies[order(top_movies$gross, decreasing=TRUE),]
top_movies$name = factor(top_movies$name, levels = top_movies$name[order(top_movies$gross, decreasing=TRUE)])
p = ggplot(top_movies, aes(x=name, y=gross)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 U.S. Movies by Box Office Gross (dollars)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Movie") + ylab("Box Office") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)


weighted_mean <- function(x, w, ..., na.rm = FALSE){
  if(na.rm){
    df_omit <- na.omit(data.frame(x, w))
    return(weighted.mean(df_omit$x, df_omit$w, ...))
  } 
  weighted.mean(x, w, ...)
}

runtime_stats = ddply(movies, "year", summarise, 
                       wmean=weighted_mean(strtoi(runtime),w=strtoi(gross),na.rm=TRUE), mean=mean(strtoi(runtime),na.rm=TRUE), median=median(strtoi(runtime),na.rm=TRUE), min=min(strtoi(runtime),na.rm=TRUE), max=max(strtoi(runtime),na.rm=TRUE), sd=sd(strtoi(runtime),na.rm=TRUE))
p = ggplot(runtime_stats, aes(x=year, y=wmean)) + geom_line() + geom_point()
ggsave(filename = "./runtime_wmean.png", plot=p, width=5, height=3.5) 

gross_stats = ddply(movies, "year", summarise, 
                    mean=mean(strtoi(gross),na.rm=TRUE), median=median(strtoi(gross),na.rm=TRUE), min=min(strtoi(gross),na.rm=TRUE), max=max(strtoi(gross),na.rm=TRUE), sd=sd(strtoi(gross),na.rm=TRUE))
p = ggplot(gross_stats, aes(x=year, y=mean)) + geom_line() + geom_point()
ggsave(filename = "./gross_mean.png", plot=p, width=5, height=3.5) 


# Now calculate statistics using weeks as a weight for the mean - this gives a better sense of the trends
library(data.table)

movies1 = as.data.table(movies)
movies2 = movies1[,lapply(.SD,mean, na.rm=TRUE),by=year]
movies2 = movies1[,lapply(.SD,weighted_mean, na.rm=TRUE,w=gross),by=year]

# Point 1 - shortening trend
p = ggplot(movies2, aes(x=year, y=runtime)) + geom_line() + ylab("Runtime (minutes)") 
ggsave(filename = "./runtime.png", plot=p, width=5, height=3.5) 


# most money by actor
dt = read.csv("all_actors_movies.csv")
actors = dt[!duplicated(dt[,c('starring','gross', 'name')], fromLast=FALSE),] 
actors$gross = strtoi(actors$gross)
actors$starring = gsub("\'", actors$starring)
#actors[is.na(actors$gross),]$gross = 0  # remove NA entries
#actors = ddply(actors,"starring",numcolwise(sum))

actors = ddply(actors, "starring", summarise, gross = sum(gross, na.rm=TRUE))
actors = actors[order(actors$gross, decreasing=TRUE),]
data_name = "actors"

top_actors = actors[1:30,]
top_actors = top_actors[order(top_actors$gross, decreasing=TRUE),]
top_actors$starring = factor(top_actors$starring, levels = top_actors$starring[order(top_actors$gross, decreasing=TRUE)])
p = ggplot(top_actors, aes(x=starring, y=gross)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 50 U.S. Actors by Box Office Gross (dollars)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Actor") + ylab("Total Box Office") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)

directors = ddply(movies, "director", summarise, gross = sum(gross, na.rm=TRUE))
directors = directors[order(directors$gross, decreasing=TRUE),]
directors$director = gsub("\\[","",directors$director)
directors$director = gsub("\\]","",directors$director)
directors$director = gsub("\\'","",directors$director)
data_name = "directors"
# fix chris columbus
directors$director = gsub("Chris Columbus \\(filmmaker\\)","",directors$director)

top_directors = directors[1:20,]
top_directors = top_directors[order(top_directors$gross, decreasing=TRUE),]
top_directors$director = factor(top_directors$director, levels = top_directors$director[order(top_directors$gross, decreasing=TRUE)])
p = ggplot(top_directors, aes(x=director, y=gross)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 U.S. Directors by Box Office Gross (dollars)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Director") + ylab("Total Box Office") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)


# top movie star name 
# chris, michael, mark, elizabeth, ben, dan, john, tom 
 # 1 - remove last names
 # 2 - same as before
first_name_actors = dt[!duplicated(dt[,c('name','gross','starring')], fromLast=FALSE),]
first_name_actors$gross = strtoi(first_name_actors$gross)
first_name_actors$starring = gsub("(\\w+)\\s*\\w*", "\\1",first_name_actors$starring)
#first_name_actors$starring = gsub("(\\w+)\\.\\s*\\w*", "\\1",first_name_actors$starring)
first_name_actors$starring = gsub("(\\w+)\\s*\\w*", "\\1",first_name_actors$starring)
first_name_actors$gross = strtoi(first_name_actors$gross)
first_name_actors = ddply(first_name_actors, "starring", summarise, gross = sum(gross, na.rm=TRUE))
first_name_actors = first_name_actors[order(first_name_actors$gross, decreasing=TRUE),]
data_name = "actors_first_names"

top_first_name_actors = first_name_actors[1:20,]
top_first_name_actors = top_first_name_actors[order(top_first_name_actors$gross, decreasing=TRUE),]
top_first_name_actors$starring = factor(top_first_name_actors$starring, levels = top_first_name_actors$starring[order(top_first_name_actors$gross, decreasing=TRUE)])
p = ggplot(top_first_name_actors, aes(x=starring, y=gross)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 U.S. Actor Names by Box Office Gross (dollars)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Actor First Name") + ylab("Total Box Office") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)


# what month do movies make the most money?
  # get the months and add a column
# what actors/directors/producers/writers made the most money??
# script to break it up ->
movies$released =  format(as.Date(movies$released), "%Y/%m/%d")
movies$day = substr(movies$released,9,10)
movies$month = substr(movies$released,6,7)

months = ddply(movies, "month", summarise, gross = sum(gross, na.rm=TRUE))
months = months[!is.na(months$month),]
data_name = "months"

km <- kmeans(months$gross,centers=3)
months$cluster <- as.factor(km$cluster)
days$day = as.numeric(days$day)
days$gross = as.numeric(days$gross)
p = ggplot(months, aes(x=month, y=gross, fill=cluster)) + geom_bar(stat="identity") 
p = p + ggtitle("Total Hollywood Box Office Gross by Month")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Month") + ylab("Total Box Office") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
p = p + geom_smooth(method='lm')
#p = p + geom_smooth(aes(x=month,y=gross), geom="line", color="red")
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)

days = ddply(movies, "day", summarise, gross = sum(gross, na.rm=TRUE))
days = days[!is.na(days$day),]
data_name = "days"

km <- kmeans(days$gross,centers=3)
days$cluster <- as.factor(km$cluster)
p = ggplot(days, aes(x=day, y=gross, fill=cluster)) + geom_bar(stat="identity") 
p = p + ggtitle("Total Hollywood Box Office Gross by Day of Month")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Day") + ylab("Total Box Office") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
#p = p + geom_smooth(method="lm", formula=gross ~ (day)^2, data=days)
p = p + stat_density(geom="line", color="red")
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)

# analyze the top movies - runtime, released, etc
top_movies = movies[1:100,]
top_movies = top_movies[order(top_movies$gross, decreasing=TRUE),]
top_movies = top_movies[!is.na(top_movies$month),]
data_name = "top_movies_months"

p = ggplot(top_movies, aes(x=month)) + geom_bar() 
p = p + ggtitle("Total Hollywood Box Office Gross by Month")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Month") + ylab("num films of top 100 ") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
#p = p + geom_smooth(method="lm", formula=gross ~ (day)^2, data=days)
p = p + stat_density(geom="line", color="red")
ggsave(filename = sprintf("./%s_top100.png", data_name) , plot=p, width=8, height=6)

top_movies = movies[1:100,]
top_movies = top_movies[order(top_movies$gross, decreasing=TRUE),]
top_movies = top_movies[!is.na(top_movies$runtime),]
data_name = "top_movies_runtime"
km <- kmeans(top_movies$runtime,centers=3)
top_movies$cluster <- as.factor(km$cluster)
p = ggplot(top_movies, aes(x=runtime, fill=cluster)) + geom_histogram(binwidth=2)
p = p + stat_density(geom="line", color="red")
p = p + ggtitle("Total Hollywood Box Office Gross by Runtime (minutes)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Runtime (minutes)") + ylab("num films of top 100") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
#p = p + geom_smooth(method="lm", formula=gross ~ (day)^2, data=days)
p = p + stat_density(geom="line", color="red")
ggsave(filename = sprintf("./%s_top100.png", data_name) , plot=p, width=8, height=6)

# producers


# writers   


# What movies made the most profit?
# what movies had higest rate of return -> gross-budget/budget