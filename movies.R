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


workingDir = '/Users/michaeltauberg/powerlaw/movies'
setwd(workingDir)



csvName = "movies.csv"
data_name = "movies"
dt = read.csv(csvName)
dt = dt[grep("United States", dt$country), ] # US movies only - misses gladiator - fix country?
dt = dt[grep("English", dt$language), ]
dt$gross = as.numeric(as.character(dt$gross))
movies = dt[order(dt$gross, decreasing=TRUE),]
movies = movies[!duplicated(movies[,c('name','gross')], fromLast=FALSE),] 

# still missing force awakens, infinity war, black panther, deathly hallows, last jedi
# plot top 20
top_movies = movies[1:30,]
top_movies = top_movies[order(top_movies$gross, decreasing=TRUE),]
top_movies$name = factor(top_movies$name, levels = top_movies$name[order(top_movies$gross, decreasing=TRUE)])
p = ggplot(top_movies, aes(x=name, y=gross)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 U.S. Movies by Box Office Gross (dollars)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Movie") + ylab("Box Office") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)
