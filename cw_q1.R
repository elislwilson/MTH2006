library(tidyverse)
library(ggplot2)
load("~/OneDrive - University of Exeter/R Directory /aut2021.RData")
# ------ Question 1(a) ------

# Examining and filtering data 
alfheim_dry <- filter(alfheim, y <= 1 )
alfheim_wet <- filter(alfheim, y > 1)
alfheim_wet <- filter(alfheim_wet, y < 100)
head(alfheim_wet)
head(alfheim_dry)

# Adding years column to table
year <- c(rep(1,365),rep(2,365),rep(3,365),rep(4,365),rep(5,365),
          rep(6,365),rep(7,365),rep(8,365),rep(9,365),rep(10,365))

alfheim <- mutate(alfheim, year = year)

# Plotting rainfall data 
ggplot(alfheim) + 
  geom_boxplot(aes(group = year, x = year, y = y, 
                   fill = year) 
  ) +
  ylim(0,2.5) +
  theme(legend.position="none",
        plot.title = element_text(size=11)) +
  ggtitle("Yearly variation in daily rainfall data") +
  scale_x_continuous('Year', seq(1,10,1)) + 
  ylab('Daily rainfall (mm)')

# --- Question 1(d) ---

# getting information from data
m = nrow(alfheim_wet)
n = nrow(alfheim)
sum_y_wet = sum(alfheim_wet[,2])

# estimating params using formulas
phi_hat =   m / n
theta_hat = m / ((sum_y_wet) - m)

print("The estimate for phi is:")
print(phi_hat)
print("The estimate for theta is: " )
print(theta_hat)

# --- Question 1(e) ---


# p[1] = phi, p[2] = theta
# Creating log-likelihood function 
loglik <- function(p){
  if(p[1] <= 0) {
    return(-1e20)
  }
  if(p[2] <= 0) {
    return(-1e20)
  }
  
  # Setting values from data 
  n = nrow(alfheim)
  m = nrow(alfheim_wet) # alfheim_wet - all vals s.t y > 1
  sum_y = sum(alfheim_wet[,2])
  
  # Calculating log-likelihood 
  (n-m) * log(1-p[1]) + m * ( log(p[2]) + log(p[1]) ) - p[2] * (sum_y - m)
}

# Optimising for parameters
phi_opt <- optim(c(0.5, 0.5), loglik, control = list(fnscale = -1))$par[1]
theta_opt <- optim(c(0.5, 0.5), loglik, control = list(fnscale = -1))$par[2]

# Creating data frame of results
df <- data.frame("Optimized MLE" = c(phi_opt, theta_opt),
                 'Diff MLE' = c(phi_hat, theta_hat))
df
