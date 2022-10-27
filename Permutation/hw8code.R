rm(list = ls())
set.seed(440)
library(data.table)
library(ggplot2)

setwd("C:/Users/Charlie Lu/Desktop/")
data <- fread('./R file/tate_art.csv')

data_filtered <- subset(data, medium == "Photograph" | medium == "Watercolor")
df <- subset(data_filtered, select = c(medium,height,width))

photo <- subset(df, medium == "Photograph")
watercolor <- subset(df, medium == "Watercolor")

photo$area <- photo$height*photo$width
watercolor$area <- watercolor$height*watercolor$width

same_dist_perm_test = function(n_perms, xs, ys, test_statistic) {
  #' 
  #' perform a generic permutation test 
  #' @param n_perms number of permutations to generate
  #' @param xs vector of samples from distribution X
  #' @param yx vector of samples from distribution y
  #' @param test_statistic function that calculates the test statistic
  
  # calculate the number of samples in X and Y
  n = length(xs) 
  m = length(ys) 
  # define labels (1 = X samples, 0 = y samples)
  labels = c(rep(1, n), rep(0, m))
  all_data = c(xs, ys)
  
  # for every permutation replication
  replicate(
    n_perms, {
      # permute label orders
      permuted_labels = sample(labels)
      
      # generate new test statistic under permutation
      test_statistic(all_data[permuted_labels == 1],
                     all_data[permuted_labels == 0])
    }
    
  )
}
x_obs <- photo$area
y_obs <- watercolor$area
obs_perms <- same_dist_perm_test(10000,photo$area,watercolor$area,
                                 function(a, b){(mean(a)-mean(b))^2})
ggplot(data=data.frame(x=obs_perms)) + 
  geom_histogram(aes(x=x, y=..density..), bins=30, color="black", fill="white") + 
  geom_vline(xintercept=mean(x_obs) - mean(y_obs), color="blue", size=1.5) + 
  xlab("Difference in sample means") + ylab("Density") + theme_classic() 

p_value = mean(obs_perms > (mean(x_obs) - mean(y_obs))^2)
p_value