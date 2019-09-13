#Reproducibility Data Challenge

library(readr)
library(ggplot2)
library(dplyr)

mdat <- read.csv("mitchell_weevil_egg_data_1975.csv")
cdat <- read.csv("cole_arthropod_data_1946.csv")

mdat <- as.data.frame(mdat)
cdat <- as.data.frame(cdat)

#ggplot( dpois( x=0:159, lambda=13.33 )) + ggplot(normd)

#scatter
plot(0:30, dpois( x=0:30, lambda=13.33))
a <- aes(x = k_number_of_arthropods , y = C_count_of_boards_with_k_spiders)
p <- ggplot(cdat,a) + geom_point() + plot(0:30, dpois( x=0:30, lambda=13.33))
print(p)

#histogram
a <- aes(C_count_of_boards_with_k_spiders)
p <- ggplot(cdat, a) + geom_histogram()
print(p)
normden <- function(x){dnorm(x, mean=13.33, sd=sqrt(13.33))}
curve(normden, from=0, to=30, add=TRUE, col="red")



p <- ggplot(cdat, a) + geom_point(x = cdat$k_number_of_arthropods, y = cdat$C_count_of_boards_with_k_spiders)
print(p)
normden <- function(x){dnorm(x, mean=13.33, sd=sqrt(13.33))}
curve(normden, from=0, to=30, add=TRUE, col="red")
normden <- function(x){dnorm(x, mean=13.33, sd=sqrt(13.33))}
curve(normden, from=0, to=30, add=TRUE, col="red")

normden <- function(x){dnorm(x, mean=13.33, sd=sqrt(13.33))}
curve(normden, from=0, to=30, add=TRUE, col="red")
#
# ggplot(data.frame(x=c(0:10)), aes(x)) +
#   stat_function(geom="point", n=11, fun=dpois, args=list(1))


#fit poisson distr to tbl$Freq data
poisson = fitdist(cdat$C_count_of_boards_with_k_spiders, 'pois', method = 'mle')
print(poisson)

#plot
plot(cdat$C_count_of_boards_with_k_spiders, cdat$Freq, type = 'h', ylim = c(0,10), ylab = 'No. of years with x events', 
     xlab = 'No. of events in a year', main = 'All 13-day events with Poisson')


dist = dpois(1:10, lambda = 4)
dist = dist * sum(df$Freq)
dist = as.data.frame(dist)
dist$Var1 = df$Var1

lines(dist$Var1, dist$dist, lwd = 2)

#Plotting same graph but with dLGP data
spider_plot <- ggplot(cdat, aes(x=cdat$k_number_of_arthropods, y=p_normspider)) +
  geom_point(size = 3) + xlab("") + ylab("") +
  geom_line(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_dLGP_spider), linetype="dotted", colour='red') +  
  geom_point(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_dLGP_spider),colour='red', shape=0, size = 3)
spider_plot