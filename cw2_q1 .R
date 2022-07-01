# ---- MTH2006 - Coursework 2 - Q1 ----
rm(list = ls())
set.seed(110322)

mosquito <- read.csv('mosquito2021.csv')
alpha = 0.05 # setting significance level

# ---- 1(a) - Relabeling ----
# Modifying data set to treat dose levels as factors and 
# reordering 
mosquito <- mutate(mosquito, dose = factor(dose, levels =  
                                          c('low', 'medium', 'high'))) %>%
            arrange(match(dose, c('low', 'medium', 'high' )))
head(mosquito)

# ---- 1(b) - Plotting data ---- 
mosq.plot <- ggplot(mosquito, aes(x = insecticide, y = hours10)) +
  geom_jitter(width = 0.2, height = 0,  aes(colour = dose))

# Formatting and saving plot
format <- theme(plot.title = element_text(hjust = 0.5 ,face = "bold", size = 16),
                plot.subtitle =  element_text(hjust = 0.5, size = 12),
                axis.title = element_text(size = 13),
                legend.position = 'bottom',
                legend.background = element_rect(fill = 'grey95'),
                legend.text = element_text(size = 10)) + 
          scale_colour_discrete(labels = c('Low', 'Medium', 'High'))
mosq.plot <- mosq.plot +
                  labs(x = 'Insecticide', y = 'Survival Time (tens of hours)', 
                       colour = 'Dose', title = 'Survival of mosquitos', 
                       subtitle = 'when exposed to varying 
                       doses of different insecticides') +
                  format 
pdf('mosq_plot.pdf') 
  mosq.plot  
dev.off()

# ---- 1(c) - Replicates and Sample sizes ----
n <- nrow(mosquito) # sample size
n.dose <- length(unique(mosquito$dose)) # number of doses
n.insect <- length(unique(mosquito$insecticide)) # number of insecticides 
repl <- n / (n.dose * n.insect) # number of replicates


# ---- 1(d) -  Fit ANOVA and linear models ----
# Fitting ANOVA models for all possible models
aov.both <- summary(aov(hours10 ~ dose + insecticide, mosquito))
aov.dose <- summary(aov(hours10 ~ dose, mosquito))
aov.insect <-summary(aov(hours10 ~ insecticide, mosquito))
aov.int <- summary(aov(hours10 ~ dose:insecticide, mosquito))
aov.all <- summary(aov(hours10 ~ dose + insecticide + dose:insecticide, mosquito))

# Fitting possible linear models 
lm.both <- summary(lm(hours10 ~ dose + insecticide, mosquito)) 
lm.dose <- summary(lm(hours10 ~ dose, mosquito))
lm.insect <- summary(lm(hours10 ~ insecticide, mosquito))
lm.int <- summary(lm(hours10 ~ dose:insecticide, mosquito))
lm.all <- summary(lm(hours10 ~ dose + insecticide + dose:insecticide, mosquito))

# Isolating p-values
p.vals <- c( aov.dose[[1]][1,5], aov.insect[[1]][1,5], aov.int[[1]][1,5],
             aov.both[[1]][1,5], aov.both[[1]][2,5], 
             aov.all[[1]][1,5], aov.all[[1]][2,5], aov.all[[1]][3,5])

# Doing significance test on all models
sig = NULL
for (i in 0:length(p.vals)){
  sig[i] = ( p.vals[i] < alpha )
}
# p-values table              
p.tab <- tibble('Model' = c('Dose', 'Insecticide', 'Interaction',
                            'Both-Dose', 'Both-Insect', 
                            'All - Dose', 'All - Insecticide',
                            'All - Interaction'),
                'P-Values' = p.vals, 'Significant?' = sig)
print(p.tab)

# Isolating adjusted R vals
adjR.vals <- c( lm.dose$adj.r.squared, lm.insect$adj.r.squared, 
                lm.both$adj.r.squared, 
                lm.int$adj.r.squared, lm.all$adj.r.squared)

# Adjusted R-squared table
adjR.tab <- tibble('Model' = c('Both', 'Dose', 'Insecticide', 
                               'Interaction', 'All'),
                   'Adj R Values' =  adjR.vals)
print(adjR.tab)


# Fitting best model 
model <- lm(hours10 ~ dose + insecticide + dose:insecticide, mosquito)

# ---- 1(e) - Residual Plot ----
model.resid <- tibble('Fitted' = fitted(model), 'Residual' = resid(model))

# Plotting, formatting and saving residual plot
resid.plot <- ggplot(model.resid) + 
                geom_point(aes(x =  Fitted, y = Residual), 
                          alpha = 0.5, fill = 'white', 
                          colour = 'red3', shape = 1) +
                geom_hline(aes(yintercept = 0), alpha = 0.8, lty = 'dotted') +
                labs(x = 'Fitted values',
                     title = 'Residuals vs Fitted') +
               theme(plot.title = element_text(hjust = 0.5 , 
                                               face = "bold", size = 16))
pdf('resid_plot.pdf')
  resid.plot 
dev.off()

# ----  1(f) - Best insecticide ----
coef <- coef(summary(model))