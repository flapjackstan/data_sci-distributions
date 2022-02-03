# https://www.kaggle.com/hamelg/intro-to-r-part-22-probability-distributions
# https://www.youtube.com/watch?v=Rxe086csxTU
###################################   Setup   ##########################################
library(ggplot2)

# for reproducibility of simulated data
SEED <- 1151
set.seed(SEED)

N <- 50000

###################################   Random Uniform Distribution   ##########################################

# Random Uniform Distribution
# 5k random numbers between 1 and 5
runif_data <- runif(N, 0, 10)
head(runif_data, 25)

mean(runif_data)
sd(runif_data)
var(runif_data)

hist(runif_data, breaks =  51, col = "orange", main = "runif")
plot(runif_data, type = "p")

# good video explaining calculating density/kde https://www.youtube.com/watch?v=x5zLaWT5KPs&t=322s
# kde is alot better with non uniform distributions, but this look pretty
plot(density(runif_data,kernel="gaussian"))

runif_frame = with(density(runif_data,kernel="gaussian"),  # Create data frame density values
                  data.frame(x,y))  


runif_plot <- ggplot(data = runif_frame, aes(x = x, y = y)) +   # Create the plot
  geom_line(color="NA")+
  geom_ribbon(data=subset(runif_frame,x > -1 & x < 11),
              aes(ymax=y, ymin=0),
              fill="skyblue", 
              alpha=0.4)

runif_plot


# getting probability of getting something to the left 
# of quantile/value aka cumulative distribution function
punif(q=4, min=0, max=10) # 40% chance of randomly selecting 4 or less
punif(q=10, min=0, max=10) # 100% chance of randomly select 10 or less

# getting quantile/value of given percentage
qunif(p=.4, min=0, max=10)
qunif(p=1, min=0, max=10)

# probability of picking something less than 6 is 60%
# 60% chance to pick something less than 6

runif_plot_with_prob_cutoffs <- runif_plot  +
  geom_ribbon(data=subset(runif_frame,x > qunif(p=.4, min=0, max=10)),
              aes(ymax=y, ymin=0),
              fill="red", 
              alpha=0.4)+
  geom_text(x=2,y=0.03,label=round(punif(q=4, min=0, max=10),4),size=4) +
  geom_text(x=6.6,y=0.03,label=round(1-punif(q=4, min=0, max=10),4),size=4)

runif_plot_with_prob_cutoffs


# gets you the the density of a value (height of curve)
# will be .1 for all values because all values have an equal
# probability

# this will not look right if youre looking at a density curve with small n
# also not having correct kde parameters (kernel, bandwith, weight, etc..) doesnt help
dunif(x=0, min=0, max=10)
dunif(x=5.2, min=0, max=10)
dunif(x=10, min=0, max=10)
dunif(x=11, min=0, max=10)

###################################   Normal/Gaussian Distribution   ##########################################


