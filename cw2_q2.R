# ---- MTH2006 - Coursework 2 - Q2 ----
rm(list = ls())
set.seed(110322)
setwd("~/OneDrive - University of Exeter/RStudio/Coursework 2")

# ---- 2(a) - Models ----
# Modifying data for values greater than 0
rainfall <- read.csv('seavington2020.csv')
rainfall <- filter(rainfall, prcp > 0)
prcp <- rainfall$prcp

params = c(0.775, 0.272) # setting MLE parameters

# Calling functions of cdf and inverse cdf
cdf <- function(params, y) {
  if (y > 0) {1 - exp( - (params[2] * y)^params[1] ) }
  else { 0 }
}
inv_cdf <- function(params, y){
  if (y > 0) {
    ( ( - log( 1 - y ) ) ^(1/params[1]) / params[2]) } 
  else { 0 }
}

fy <- cdf(params, prcp)
inv.fy <- inv_cdf(params, fy)

# Plotting, formatting, saving Q-Q plot 
qqplot <- ggplot(rainfall, aes(x = prcp)) + 
                stat_ecdf(geom = 'point', alpha = 0.4) +
                stat_function(fun = cdf , args = list(params = params))
qqplot <- qqplot + labs(x = 'Theoretical Quantiles', y = 'Data Quantiles',
                        title = 'Q-Q Plot') +
                   theme(plot.title = 
                           element_text(hjust = 0.5 , face = "bold", size = 16))
pdf('qq_plot.pdf')
  qqplot
dev.off()

# ---- 2(b)  - Hypothesis test for chi-squared ----
# H_0 - Sample is in the distribution 
# H_1 - Sample is not in the distribution

bins <- seq(0,40,5) # setting desired bins
bins[length(bins)+1] <- Inf # modifying right hand boundary to be infinity 
obs <- table(cut(prcp, bins)) # finding number of values in each bin from data

F_Y = NULL # calculating cdf for each bin boundary
for (i in 1:length(bins)){
  F_Y[i] <- cdf(params, bins[i])}

# Calculating expected numbers for each bin 
expec <- nrow(rainfall) * diff(F_Y)

# Comparison of expected vs observed values 
comp <- tibble('Expected' = round(expec), 'Observed' = obs)

# Conducting hypothesis test 
test.stat <- sum((obs - expec)^2 / expec) # test statistic
d.f <- length(expec) - length(params) - 1 # degrees of freedom (J - p - 1),
pval <- 1 - pchisq(test.stat, d.f) # p-value calculation

# Not significant at the 5% level - no significant evidence to reject H0

# ---- 2(c) - Kernel Density Estimate ---- 
# Takes input of y-value, data points, the specified distribution 
# and the bandwidth 
kde_sqr <- function(y, y_i, distribution, h) {
  x = 0 # Initialising  x 
  n = length(y_i) # specifying n
  for (i in 0:n) { # looping and summing for all points of data
    
    x <- x + ( density( ( sqrt(y) - sqrt(y_i[i]) ) / h, kernel = distribution,
                        bw = h, give.Rkern = TRUE) ) / (2 * sqrt(y)) 
  }
  fy_sqr = x / ( n * h ) # dividing data by nh and returning 
  return(fy_sqr)
}
# KDE for points for a plot
points <- seq(0,40,0.5)
kde.points <- kde_sqr(points, prcp, 'gaussian', 0.3)
kde.tab <- tibble('Points' = points, 'Est' = kde.points)

# Plotting points 
kdeplot <- ggplot(kde.tab, aes(x = points, y = kde.points)) + 
                geom_point(alpha = 0.5) + 
                geom_line(lty = 2) 
                
kdeplot <- kdeplot + labs(x = 'Rainfall (mm)', 
                            y = 'Kernel Density Estimate',
                            title = 'Kernel Density Function') +
            theme(plot.title = element_text(hjust = 0.5 , 
                                            face = "bold", size = 16))

# Saving KDE plot
pdf('kde_plot.pdf')
  kdeplot
dev.off()            

# Evaluating set values
vals2 <- c(0.5, 1, 2, 5, 10)
kde.vals2 <- kde_sqr(vals2, prcp, 'gaussian', 0.3)
kde.tab2 <- tibble('y' = vals2, 'KDE' = kde.vals2)
