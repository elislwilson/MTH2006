# ---- Question 2 ----

library(tidyverse)
library(ggplot2)
load("~/OneDrive - University of Exeter/R Directory /aut2021.RData")

# ---- Question 2(a) ----

# creating a plot of data
tempplot <- ggplot(global, aes(year, temp)) + geom_point( aes(x = year, y = temp, 
            colour = temp), alpha = 0.7) +
            geom_smooth( aes(x = year, y = temp), colour = 'blue2') +
            labs(x = "Year", y = 'Change in global surface  
                 air temperature (°C)') +
            ggtitle('Change in global air surface temperature \n 1857 - 2021') +
            theme(legend.position = "none") 
tempplot
#ggsave(plot = tempplot, filename ='temp_plot.png')

# ---- Question 2(b) ----

# generating linear model
fit.lm <- lm(temp ~ year, data = global)
summary(fit.lm)

# finding rate from summary
rate <-  summary(fit.lm)$coefficients[2,1] 
std_err <- summary(fit.lm)$coefficients[2,2]

# find confidence intervala of rate
low_CI <- rate - 2 * std_err
hi_CI <- rate + 2* std_err 
print('The confidence interval is')
print((low_CI))
print((hi_CI))

# Conducting hypothesis test
p_value <- summary(fit.lm)$coefficients[2,4]
sig_level <- 0.01
test_stat <- summary(fit.lm)$coefficients[2,3]
print("The null hypothesis is that r = 0 (rate of change), 
       the alternate hypothesis is r != 0")
if (p_value < sig_level){
  print("The p-value is below the significance level hence 
        we reject the null hypothesis, the rate of change 
        is significantly different from 0")
}
 
# ---- Question 2(c) ----

# plotting linear model on graph
m <- fit.lm$coefficients[1] 
temp_plot_fit <-  + geom_abline(intercept = m, slope = rate,
                                        colour = 'green3', size = 1)
temp_plot_fit
#ggsave(plot = temp_plot_fit, filename ='temp_fit_plot.png')

#plot(fit.lm) 


# ---- Question 2(d) ----

# Generating models and find p-value and adj.r-squared values
lm.poly1 <- lm(temp ~ poly(year,1), data = global)
ra_1 <- summary(lm.poly1)$adj.r.squared
p_1 <- anova(lm.poly1)$'Pr(>F)'[1]

lm.poly2 <- lm(temp ~ poly(year, 2), data = global)
ra_2 <-summary(lm.poly2)$adj.r.squared
p_2 <- anova(lm.poly2)$'Pr(>F)'[1]

lm.poly3 <- lm(temp ~ poly(year, 3), data = global)
ra_3 <- summary(lm.poly3)$adj.r.squared
p_3 <- anova(lm.poly3)$'Pr(>F)'[1]

lm.poly4 <- lm(temp ~ poly(year, 4), data = global)
ra_4 <- summary(lm.poly4)$adj.r.squared
p_4 <-  anova(lm.poly4)$'Pr(>F)'[1]

modelcheck <- tibble('PolyNumber' = 1:4, 
                     "Adj.R-Values" = c(ra_1, ra_2, ra_3, ra_4), 
                     "P-Values" = c(p_1, p_2, p_3, p_4) )


# Conducting hypothesis test 
if (p_4 < 0.05) {
  print("Reject H_0")
}

# ---- Question 2(e) ----

# Predicting value for year 2040 
predict = data.frame(year = 2040)
predict_val <- predict(lm.poly4, newdata = predict, interval = 'confidence')