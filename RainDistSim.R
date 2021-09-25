#----SPI builder
#----Abigail Knapp 4.17.2020

# Load in required packages
library(stats)
library(ggplot2)

 #  __ ___                 ___ _    _      ___     
 # (_   |  |\/| | | |   /\  | |_   | \  /\  |  /\  
 # __) _|_ |  | |_| |_ /--\ | |_   |_/ /--\ | /--\ 


# some variables to describe the data
# variables about data length
start.year <- 1961 
end.year <- 2020
no.years <- end.year - start.year + 1

# variables about the gamma distribution
alpha <- 2
beta <- 1/3

# simulate 60 years of precipitation data
# generate 12 gamma distributions with similar alpha, beta for 60 observations
# gamma distribution alpha is shape factor and beta is scale factor

# function to generate a distributions for a month for the whole range of years
# depth column uses randomly generated gamma distributions created based on randomized shape and scale factor (alpha and 1/beta) 
# month column
# year column has sequence of years based on globally defined start and end years

rain.sim <- function(x, month.name){
  x <- data.frame(depth = rgamma(n=no.years, 
                                       shape=sample(1:3), 
                                       scale=sample(1:5)
                                       ), 
                  month = month.name, 
                  year = seq(start.year, end.year))
}

jan <- rain.sim(month.name="jan")
feb <- rain.sim(month.name="feb")
mar <- rain.sim(month.name="mar")
apr <- rain.sim(month.name="apr")
may <- rain.sim(month.name="may")
jun <- rain.sim(month.name="jun")
jul <- rain.sim(month.name="jul")
aug <- rain.sim(month.name="aug")
sep <- rain.sim(month.name="sep")
oct <- rain.sim(month.name="oct")
nov <- rain.sim(month.name="nov")
dec <- rain.sim(month.name="dec")

# Bind all months together into a grouped dataframe
precip <- rbind(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

# To plot a single year out
y1961 <- precip[which(year==1961)]
plot(y1961$month, y1961$depth)

# show probability distribution functions for each month on one plot
#default plot
p <- ggplot(precip, aes(depth, fill=month)) + geom_density(alpha=0.2) + ggtitle('Monthly Rainfall Probability Distribution Functions') 
print(p)


