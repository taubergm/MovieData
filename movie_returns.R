if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
library("ggplot2")
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
library(plyr)
if (!require(data.table)) {
  install.packages("data.table", repos="http://cran.us.r-project.org")
}
library(data.table)
if (!require(grid)) {
  install.packages("grid", repos="http://cran.us.r-project.org")
}
library(grid)

workingDir = '/Users/michaeltauberg/projects/movies'
setwd(workingDir)

csvName = "movies.csv"
dt = read.csv(csvName)
dt = dt[!duplicated(dt[,c('name','year')], fromLast=FALSE),] 
dt = dt[dt$gross != '', ]
dt$gross = as.numeric(as.character(dt$gross))
dt$budget = as.numeric(as.character(dt$budget))
dt$rate_of_return = (dt$gross-dt$budget)/dt$budget
dt$profit = dt$gross-dt$budget

dt = dt[grep("United States", dt$country), ] # US movies only - misses gladiator - fix country?
dt = dt[grep("English", dt$language), ]
dt = dt[!is.na(dt$profit),]

dt$released =  format(as.Date(dt$released), "%Y/%m/%d")
dt$day = substr(dt$released,9,10)
dt$month = substr(dt$released,6,7)

# june movies
june = dt[dt$month == "06",]
june = june[!is.na(june$month),]
june_years = c(table(june$year))
june_years_profit = ddply(june, "year", summarise, profit = sum(profit, na.rm=TRUE))
june_years_profit = june_years_profit[june_years_profit$year != "2018",]
data_name = "june_profit"
p = ggplot(june_years_profit, aes(x=year, y=profit)) + geom_point() 
p = p + geom_line()
p = p + geom_smooth(method='lm', se = FALSE)
p = p + scale_x_discrete(limits = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))
p = p + ggtitle("Profit from Hollywood Movies Released in June (per year)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Year") + ylab("Total Profit of June Movies")
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)

# june movies
june = dt[dt$month == "06",]
june = june[!is.na(june$month),]
june_years = c(table(june$year))
june_years_profit = ddply(june, "year", summarise, gross = sum(gross, na.rm=TRUE))
june_years_profit = june_years_profit[june_years_profit$year != "2018",]
data_name = "june_profit"
p = ggplot(june_years_profit, aes(x=year, y=gross)) + geom_point() 
p = p + geom_line()
p = p + geom_smooth(method='lm', se = FALSE)
p = p + scale_x_discrete(limits = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))
p = p + ggtitle("Gross from Hollywood Movies Released in June (per year)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Year") + ylab("Total Profit of June Movies")
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=8, height=6)



data_name = "profit"
dt = dt[order(dt$profit, decreasing=TRUE),]
#top_profit = dt[1:20,]
top_profit = read.csv("top_profit.csv")
top_profit$name = factor(top_profit$name, levels = top_profit$name[order(top_profit$profit, decreasing=TRUE)])
p = ggplot(top_profit, aes(x=name, y=profit,fill=genre)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Movies by Profit")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Name") + ylab("Profit (Box Office Gross - Budget)") 
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=9, height=8)

top_profit = dt[1:200,]
top_profit = top_profit[!is.na(top_profit$month),]

months = ddply(top_profit, "month", summarise, profit = sum(profit, na.rm=TRUE))
months = months[!is.na(months$month),]
data_name = "months"

km <- kmeans(months$profit,centers=2)
months$cluster <- as.factor(km$cluster)
p = ggplot(months, aes(x=month, y=profit, fill=cluster)) + geom_bar(stat="identity") 
p = p + ggtitle("Total Profit of the Top 200 Movies (by month)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Month") + ylab("Total Box Office") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
p = p + geom_smooth(method='lm')
#p = p + geom_smooth(aes(x=month,y=gross), geom="line", color="red")
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=9, height=8)


top_profit = dt[1:200,]
top_profit = top_profit[!is.na(top_profit$runtime),]
top_profit$runtime = as.numeric(as.character(top_profit$runtime))
data_name = "top_profit_runtime"
km <- kmeans(top_profit$runtime,centers=3)
top_profit$cluster <- as.factor(km$cluster)
p = ggplot(top_profit, aes(x=runtime, fill=cluster)) + geom_histogram(binwidth=2)
p = p + stat_density(geom="line", color="red")
p = p + ggtitle("Runtime Histogram of the top 200 most profitable movies")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Runtime (minutes)") + ylab("num films of top 200") 
p = p + stat_density(geom="line", color="red")
ggsave(filename = sprintf("./%s_top100.png", data_name) , plot=p, width=9, height=8)


# color by season
seasons = c("winter","winter","spring","spring","spring","summer","summer","summer","summer",
            "fall","fall","fall","fall","winter","winter")
# for each mont, label a season
data_name = "profit_months"
dt = dt[order(dt$profit, decreasing=TRUE),]
top_profit = dt[1:200,]
top_profit = top_profit[!is.na(top_profit$month),]
top_profit$month = as.numeric(as.character(top_profit$month))
km <- kmeans(top_profit$month,centers=2)
top_profit$cluster <- as.factor(km$cluster)
top_profit$cluster <- as.factor(km$cluster)
p = ggplot(top_profit, aes(x=month, fill=cluster)) + geom_histogram(binwidth=1)
p = p + ggtitle("Total Profit of the Top 200 Movies with best Return (by month)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Month") + ylab("Total Box Office") 
p = p + scale_x_discrete(limits = seq(1,12))
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=9, height=8)


top_profit = dt[1:200,]
top_profit = top_profit[!is.na(top_profit$runtime),]
top_profit$runtime = as.numeric(as.character(top_profit$runtime))
data_name = "top_profit_runtime"
km <- kmeans(top_profit$runtime,centers=3)
top_profit$cluster <- as.factor(km$cluster)
p = ggplot(top_profit, aes(x=runtime, fill=cluster)) + geom_histogram(binwidth=2)
p = p + stat_density(geom="line", color="red")
p = p + ggtitle("Runtime Histogram of the top 200 most profitable movies")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Runtime (minutes)") + ylab("num films of top 200") 
p = p + stat_density(geom="line", color="red")
ggsave(filename = sprintf("./%s_top100.png", data_name) , plot=p, width=9, height=8)

avg_runtime = mean(top_profit$runtime)
median_runtime = median(top_profit$runtime)

###### Rate of Return ################


data_name = "return"
dt = dt[order(dt$rate_of_return, decreasing=TRUE),]
top_return = dt[1:20,]
top_return$name = factor(top_return$name, levels = top_return$name[order(top_return$rate_of_return, decreasing=TRUE)])
p = ggplot(top_return, aes(x=name, y=rate_of_return)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Movies by Rate of Return")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Name") + ylab("Return (Profit/Budget)") 
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=9, height=8)

data_name = "return2"
#top_return = dt[2:21,]
top_return = read.csv("top_return_movies.csv")

top_return$name = factor(top_return$name, levels = top_return$name[order(top_return$rate_of_return, decreasing=TRUE)])
p = ggplot(top_return, aes(x=name, y=rate_of_return,fill=genre)) + geom_bar(stat="identity") 
p = p + ggtitle("Top Movies by Rate of Return")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Name") + ylab("Return (Gross/Budget)") 
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=9, height=8)

data_name = "return_months"
dt = dt[order(dt$rate_of_return, decreasing=TRUE),]
top_return = dt[1:200,]
top_return = top_return[!is.na(top_return$month),]
top_return$month = as.numeric(as.character(top_return$month))
km <- kmeans(top_return$month,centers=4)
top_return$cluster <- as.factor(km$cluster)

p = ggplot(top_return, aes(x=month, fill=cluster)) + geom_histogram(binwidth=1)
p = p + ggtitle("Histogram of Top 200 Movies with Best Return (by month)")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Month") + ylab("Total Box Office") 
p = p + scale_x_discrete(limits = seq(1,12))
ggsave(filename = sprintf("./%s_top200.png", data_name) , plot=p, width=9, height=8)

data_name = "return_months_runtime"
top_return = dt[1:200,]
top_return$runtime = as.numeric(as.character(top_return$runtime))
top_return = top_return[!is.na(top_return$runtime),]
data_name = "top_profit_runtime"
km <- kmeans(top_return$runtime,centers=3)
top_return$cluster <- as.factor(km$cluster)
p = ggplot(top_return, aes(x=runtime, fill=cluster)) + geom_histogram(binwidth=2)
p = p + ggtitle("Runtime Histogram of the top 200 Movies with Best Rate of Return")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Runtime (minutes)") + ylab("num films of top 200") 
p = p + stat_density(geom="line", color="red")
ggsave(filename = sprintf("./%s_top100.png", data_name) , plot=p, width=9, height=8)

avg_runtime = mean(top_return$runtime)
median_runtime = median(top_return$runtime)

##############
# worst profit
###############

data_name = "worst_profit"
dt = dt[order(dt$profit, decreasing=FALSE),]
bottom_profit = dt[1:20,]
bottom_profit$name = factor(bottom_profit$name, levels = bottom_profit$name[order(bottom_profit$profit, decreasing=FALSE)])
p = ggplot(bottom_profit, aes(x=name, y=profit)) + geom_bar(stat="identity") 
p = p + ggtitle("Movies with Biggest Losses")
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=11), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Name") + ylab("Loss ($)") 
ggsave(filename = sprintf("./%s_top20.png", data_name) , plot=p, width=9, height=8)



