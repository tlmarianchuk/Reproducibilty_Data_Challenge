
#Reproducibility Data Challenge

library(RMKdiscrete)
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)


mdat <- read.csv("~/Documents/BSD-QBio5/tutorials/reproducibility/data/mitchell_weevil_egg_data_1975.csv")
cdat <- read.csv("~/Documents/BSD-QBio5/tutorials/reproducibility/data/cole_arthropod_data_1946.csv")

head(mdat)
head(cdat)

## Spiders

#Determine and set the number of boards, count the spiders, and get mean of spiders per board
cdat$nspiders <- cdat$k_number_of_arthropods * cdat$C_count_of_boards_with_k_spiders
mean_of_spiders <- sum(cdat$nspiders) / sum(cdat$C_count_of_boards_with_k_spiders)
sum_of_boards <- sum(cdat$C_count_of_boards_with_k_spiders)

#Set up new dataframe that contains the Poisson Distribution numbers and determine probability from data alone
cdat$p_spider <- dpois(cdat$k_number_of_arthropods,lambda = mean_of_spiders)
cdat$p_normspider <- cdat$C_count_of_boards_with_k_spiders / sum(cdat$C_count_of_boards_with_k_spiders)


#dLGP step for setting curve for the additional graph requested
cdat$p_dLGP_spider <- dLGP(cdat$k_number_of_arthropods,theta = mean_of_spiders,lambda = 0,nc=NULL,log=FALSE)


#Plotting first graph with Poisson distribution and data
spider_plot <- ggplot(cdat, aes(x=cdat$k_number_of_arthropods, y=p_normspider)) +
  geom_point(size = 3) + xlab("") + ylab("") + 
  geom_line(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_spider), linetype='dashed', colour='green') +  
  geom_point(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_spider),colour='green', shape=0, size = 3)
spider_plot

#PLotting same graph but with dLGP data
spider_plot <- ggplot(cdat, aes(x=cdat$k_number_of_arthropods, y=p_normspider)) +
  geom_point(size = 3) + xlab("") + ylab("") +
  geom_line(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_dLGP_spider), linetype="dotted", colour='red') +  
  geom_point(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_dLGP_spider),colour='red', shape=0, size = 3)
spider_plot




## Sowbugs



#Determine and set the number of boards, count the sowbugs, and get mean of sowbugs per board
cdat$nsowbugs <- cdat$k_number_of_arthropods * cdat$C_count_of_boards_with_k_sowbugs
mean_of_sowbugs <- (sum(cdat$nsowbugs) / sum(cdat$C_count_of_boards_with_k_sowbugs))*(1-.53214)
sum_of_boards <- sum(cdat$C_count_of_boards_with_k_sowbugs)

#Set up new dataframe that contains the Poisson Distribution numbers and determine probability from data alone
cdat$p_sowbugs <- dpois(cdat$k_number_of_arthropods,lambda = mean_of_sowbugs)
cdat$p_normsowbugs <- cdat$C_count_of_boards_with_k_sowbugs / sum(cdat$C_count_of_boards_with_k_sowbugs)


#dLGP step for setting curve for the additional graph requested
cdat$p_dLGP_sowbugs <- dLGP(cdat$k_number_of_arthropods,theta = mean_of_sowbugs,lambda = .53214,nc=NULL,log=FALSE)


#Plotting first graph with Poisson distribution and data
sowbugs_plot <- ggplot(cdat, aes(x=cdat$k_number_of_arthropods, y=p_normsowbugs)) +
  geom_point(size = 3) + xlab("") + ylab("") + 
  geom_line(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_sowbugs), linetype='dashed', colour='green') +  
  geom_point(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_sowbugs),colour='green', shape=0, size = 3)
sowbugs_plot
