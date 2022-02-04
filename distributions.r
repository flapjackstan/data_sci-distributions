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

# Gaussian dist is characterized by its mean and sd. 
# There is no explicit range of values like in the uniform dist,
# but spread is dictated by the sd you provide it
norm_data <- rnorm(N, mean=0, sd=1)
head(norm_data,25)

min(norm_data)
max(norm_data)
mean(norm_data)
sd(norm_data)
var(norm_data)

hist(norm_data, breaks =  51, col = "orange", main = "runif")
plot(norm_data, type = "p")

plot(density(norm_data,kernel="gaussian"))

norm_frame = with(density(norm_data,kernel="gaussian"),  # Create data frame density values
                   data.frame(x,y))  


norm_plot <- ggplot(data = norm_frame, aes(x = x, y = y)) +   # Create the plot
  geom_line(color="NA")+
  geom_ribbon(data=subset(norm_frame,x > -5 & x < 5),
              aes(ymax=y, ymin=0),
              fill="skyblue", 
              alpha=0.4)+
  xlim(-4,4)


norm_plot

# bulk of observations are clustered around the mean making a bell shape curve
# roughly 68% of obs lie within 1 sd, 95% within 2 sd, and 99.7% within 3

# visually seeing this

norm_mean <- mean(norm_data)
norm_sd <- sd(norm_data)

plus_one_sd <-  norm_mean+norm_sd
minus_one_sd <-  norm_mean-norm_sd

plus_two_sd <- norm_mean+norm_sd*2
minus_two_sd <- norm_mean-norm_sd*2

plus_three_sd <- norm_mean+norm_sd*3
minus_three_sd <- norm_mean-norm_sd*3

norm_plot_w_m_and_sd <- norm_plot + 
    geom_ribbon(data=subset(norm_frame,x > minus_three_sd & x < plus_three_sd),
              aes(ymax=y, ymin=0),
              fill="red", 
              alpha=0.4)+
  geom_ribbon(data=subset(norm_frame,x > minus_two_sd & x < plus_two_sd),
              aes(ymax=y, ymin=0),
              fill="yellow", 
              alpha=0.4)+
  geom_ribbon(data=subset(norm_frame,x > minus_one_sd & x < plus_one_sd),
              aes(ymax=y, ymin=0),
              fill="green", 
              alpha=0.4)+
  geom_vline(xintercept = norm_mean, color="black")
  
norm_plot_w_m_and_sd

# calculating this, note abs(q) is the sd to make it easier

# probability of getting a value less than -1
pnorm(q=-1, mean=0, sd=1)
# probability of getting a value more than -1
1-pnorm(q=-1, mean=0, sd=1)

# probability of getting a value less than 1
pnorm(q=1, mean=0, sd=1)
# probability of getting a value more than 1
1-pnorm(q=1, mean=0, sd=1)

# Get prob of observing a value less than -1
prob_under_minus1 <- pnorm(q=-1, mean=0, sd=1)

# Get prob of observing a value over 1
prob_over_1 <-  1-pnorm(q=1, mean=0, sd=1)

# Prob between -1 and 1, aka 1 sd
between_prob <- 1-(prob_under_minus1+prob_over_1)  

norm_plot_w_calc <- norm_plot + 
  geom_ribbon(data=subset(norm_frame,x < minus_one_sd),
              aes(ymax=y, ymin=0),
              fill="red", 
              alpha=0.4)+
  geom_ribbon(data=subset(norm_frame,x > minus_one_sd),
              aes(ymax=y, ymin=0),
              fill="red", 
              alpha=0.4)+
  geom_ribbon(data=subset(norm_frame,x > minus_one_sd & x < plus_one_sd),
              aes(ymax=y, ymin=0),
              fill="green", 
              alpha=0.4)+
  geom_text(x=-1.6,y=0.03,label=round(prob_under_minus1,4),size=4) +
  geom_text(x=1.6,y=0.03,label=round(prob_under_minus1,4),size=4) +
  geom_text(x=0,y=0.1,label=round(1-(prob_under_minus1*2),4),size=5) 

norm_plot_w_calc


qnorm(p = .99, mean=0, sd=1) # 99% chance to randomly select an obs lower than this
qnorm(p = 0.75,mean=0, sd=1) # 75% chance to randomly select a obs lower than this
qnorm(p = 0.5,mean=0, sd=1) # 50% chance to randomly select a obs lower than this
qnorm(p = 0.25,mean=0, sd=1) # 25% chance to randomly select a obs lower than this
qnorm(p = .01,mean=0, sd=1) # 1% chance to randomly select a obs lower than this

# 2.5% left tail
left_cutoff <- qnorm(p = 0.025,mean=0, sd=1) # 2.5% chance to randomly select a obs lower than this
# 2.5% right tail
right_cutoff <- qnorm(p = 0.975,mean=0, sd=1) # 97.5% chance to randomly select a obs lower than this

# roughly 5% of data fall outside of these cutoffs (aka outside 2nd sd)
norm_plot_w_calc + 
  geom_vline(xintercept = left_cutoff, color="black")+
  geom_vline(xintercept = right_cutoff, color="black")

###################################   Binomial/Bernouli Distribution   ##########################################
