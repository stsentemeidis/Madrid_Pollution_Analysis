
### THIS SCRIPT RUNS A SERIE OF OPERATIONS TO GENERATE A LINEAR REGRESSION

# First we split the data in train and test (80% - 20%)
set.seed(2018)
train.size <- 0.8
train.index <- sample.int(length(pollution_daily_h$NO2), round(length(pollution_daily_h$NO2) * train.size))
train.sample <- pollution_daily_h[train.index,]
test.sample <- pollution_daily_h[-train.index,]

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Train and Test Samples defined'))

# We want to define a multilinear regression model in order to explain NO2 with the rest of the variables.
# By definition, temp_min and temp_max are correlated with temp_avg, so we remove them.
# We use the variable temp_gap to measure their influence on the model.
multi_model_NO2<-lm(NO2~.-month-week-date-temp_min-temp_max, data=train.sample) 

lm_stats <- summary(multi_model_NO2)
# print(lm_stats)
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Initial Linear Model generated'))


# As we can see our R^2 value is 0.911 which means that our model is able to explain NO2 well.
# Particularly, this value means that predictors explain 91% of the variability in NO2.
# This could possibly improve if one or more predictors are not very good and are hurting our model.
# One thing to note though is that comparing R^2 values is not a great way of deciding which model is better than the other.
invisible(lm_stats$r.squared)
invisible(lm_stats$adj.r.squared)

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'R Square: ',round(lm_stats$r.squared, 6),' | ',
             'Adjusted R Square: ',round(lm_stats$adj.r.squared, 6)))

# Mean squared error is exactly how it sounds: we take the mean of all of our errors squared. 
# This is a good measure for seeing how accurate a model is because we obviously want as little error as possible.
invisible(lm_stats$sigma)

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Mean Squared Error: ',round(lm_stats$sigma, 6)))

# The first step in interpreting the multiple regression analysis is to examine the F-statistic 
# and the associated p-value, at the bottom of model summary.
# In our example, it can be seen that p-value of the F-statistic is < 2.2e-16, 
# which is highly significant. This means that, at least, one of the predictor variables
# is significantly related to the outcome variable, rejecting the null hypothesis.
lm_stats


# Another thing to look at is the confidence intervals for our coefficients. 
# Our estimates for each coefficient are not exact so we want to find a range 
# where we are at a certain percent confident that the actual value is in this range
# We can interpret this like: for every change of one (1) unit in the SO2, we are 95% confident
# that the NO2 will change between 1.2311 - 1.5350.
invisible(confint(multi_model_NO2, level=.95))

# Plotting the intervals
# plot_summs(multi_model_NO2, scale = TRUE, plot.distributions = TRUE, inner_ci_level = 0.95)

# Save Intervals Plot
png('plots/5. intervals.png', width = 900, height = 600)
print(plot_summs(multi_model_NO2, scale = TRUE, plot.distributions = TRUE, inner_ci_level = 0.95))
dev.off()

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Intervals Plot of the Initial Model is generated and saved in /plots!'))


# Another important thing is to see is if our assumptions are true, regarding the residuals.
resids_multi_NO2 <- multi_model_NO2$residuals

# par(mfrow=c(2,2))
# plot(resids_multi_NO2, type='o', xlab='',ylab='',xaxt='n',lwd=2,pch=19, main='Multi Model NO2', col='cornflowerblue'); grid()
# hist(resids_multi_NO2, col='cornflowerblue',main='Histogram of Residuals',xlab=' ')
# boxplot(resids_multi_NO2,main='Boxplot', col='cornflowerblue'); grid()
# qqnorm(resids_multi_NO2, col='cornflowerblue', main='QQ plot',xlab=' '); grid()
# dev.off()

# The residuals seem to be normally distributed, so our assumptions are correct.

# Save Residuals Plot
png('plots/6. residuals.png', width = 1500, height = 1000)
par(mfrow=c(2,2))
plot(resids_multi_NO2, type='o', xlab='',ylab='',xaxt='n',lwd=2,pch=19, main='Multi Model NO2', col='cornflowerblue'); grid()
hist(resids_multi_NO2, col='cornflowerblue',main='Histogram of Residuals',xlab=' ')
boxplot(resids_multi_NO2,main='Boxplot', col='cornflowerblue'); grid()
qqnorm(resids_multi_NO2, col='cornflowerblue', main='QQ plot',xlab=' '); grid()
dev.off()

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Residuals Plot of the Initial Model is generated and saved in /plots!'))


# After this analysis, lets see if making some transformations can be beneficial to our model.
# First, we implement stepwise regression to find out the significance in variables.

# Stepwise regression
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Step-Wise Regression:'))
cat('\n')

step <- stepAIC(multi_model_NO2,direction = "both")



# Display results
# summary(step)
# step$anova

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Initial Model Anova:'))
cat('\n')
print(step$anova)
cat('\n')

# Now that we have to most important variables, we see that we have to remove PM10 and NMHC from our model.
# After that, we will treat multicollinearity with the VIF (variance inflation factors) method.
multi_model_NO2_0<-lm(NO2~.-month-week-date-PM10-NMHC-temp_min-temp_max, data=train.sample) 
# summary(multi_model_NO2_0)
# formula(multi_model_NO2_0)
# vif(multi_model_NO2_0)

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Second Linear Model generated with formula and Variance Inflation Factors:'))
cat('\n')
print(formula(multi_model_NO2_0))
cat('\n')
print(vif(multi_model_NO2_0))
cat('\n')


# As a general rule, if VIF is larger than 5, then multi collinearity is assumed to be high.
# As a result, each time we are going to calculate the VIF values, remove the biggest one,
# re-do the model until all the explanatory variables have a VIF below 5.
# Handle the above procedure with a WHILE loop.
selectedMod <- step

all_vifs <- car::vif(selectedMod)
# print(all_vifs)

signif_all <- names(all_vifs)

while(any(all_vifs > 5)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the variable with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove this variable
  myForm <- as.formula(paste("NO2~ ", paste (signif_all, collapse=" + "), sep=""))  # design the new formula
  selectedMod <- lm(myForm, data=train.sample)  # re-build model with new formula
  all_vifs <- car::vif(selectedMod)
}

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Variables with significantly multicollinearity:'))
cat('\n')
print(all_vifs)
cat('\n')


# # Now step by step
# # remove CO
# multi_model_NO2_1<-lm(NO2~.-month-week-date-PM10-NMHC-temp_min-temp_max-CO, data=train.sample)  
# summary(multi_model_NO2_1)
# vif(multi_model_NO2_1)
# 
# # remove BEN
# multi_model_NO2_2<-lm(NO2~.-month-week-date-PM10-NMHC-temp_min-temp_max-CO-BEN, data=train.sample)  
# summary(multi_model_NO2_2)
# vif(multi_model_NO2_2)
# 
# # remove NO
# multi_model_NO2_3<-lm(NO2~.-month-week-date-PM10-NMHC-temp_min-temp_max-CO-BEN-NO, data=train.sample)  
# summary(multi_model_NO2_3)
# vif(multi_model_NO2_3)
# 
# 
# # remove humidity
# multi_model_NO2_4<-lm(NO2~.-month-week-date-PM10-NMHC-temp_min-temp_max-CO-BEN-NO-humidity, data=train.sample)  
# summary(multi_model_NO2_4)
# vif(multi_model_NO2_4)
# 
# # remove TOL
# multi_model_NO2_5<-lm(NO2~.-month-week-date-PM10-NMHC-temp_min-temp_max-CO-BEN-NO-humidity-TOL, data=train.sample)  
# summary(multi_model_NO2_5)
# vif(multi_model_NO2_5)
# print(all_vifs)

# So our final model after removing the multicollinear variables is
multi_model_NO2_final<-lm(NO2~ SO2 + O3 + PM2.5 + EBE + TCH + temp_avg + precipitation + wind_avg_speed + temp_gap, data=train.sample) 
# formula(multi_model_NO2_final)
# summary(multi_model_NO2_final)
# vif(multi_model_NO2_final)

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Final Model recalculated by removing multicollinear variables'))
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Final Model Formula:'))
cat('\n')
print(formula(multi_model_NO2_final))
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Final Model VIFs:'))
cat('\n')
print(vif(multi_model_NO2_final))
cat('\n')


# Let's see what a 10-Fold Cross validation will tell us about our model
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Starting 10-Fold Cross Validation'))

set.seed(2018)
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10, verboseIter = FALSE)

model_lm_final <- train(NO2 ~ SO2  + O3 + PM2.5 + EBE + TCH + temp_avg + precipitation + wind_avg_speed + temp_gap, 
                  data=train.sample, 
                  trControl=train_control, 
                  method="lm",
                  preProcess = c('center','scale'))

model_lm_0 <- train(NO2~.-month-week-date-temp_min-temp_max, 
                    data=train.sample, 
                    trControl=train_control, 
                    method="lm",
                    preProcess = c('center','scale'))


# Summarise Results
cat('\n')
print('### INITIAL MODEL:')
cat('\n')
print(model_lm_0)
cat('\n')
print('### FINAL MODEL:')
cat('\n')
print(model_lm_final)
cat('\n')


# Let's have a look at the predictions
test.sample$NO2_predicted_model_final <- predict(multi_model_NO2_final,test.sample)
test.sample$NO2_predicted_model_0 <- predict(multi_model_NO2_0,test.sample)

# Let's show randomly rows 80-90. We can see that in some cases the original model is better, but also the other way around.
# test.sample[80:90,c('NO2','NO2_predicted_model_0','NO2_predicted_model_final')] 

# Let's visualize it
# ggplot(test.sample,aes(x=test.sample$NO2,y=test.sample$NO2_predicted_model_final))+
#   geom_point(size=1, colour='orange')+
#   geom_point(data = test.sample, aes(x=test.sample$NO2, y=test.sample$NO2_predicted_model_0), size=1, colour='cornflowerblue')+
#   geom_abline(intercept = 0, slope = 1)+
#   ggtitle("Predictions on Actual Values") +
#   xlab("Actual Values") + ylab("Predicted Values")


# Save Test Sample Results Plot
png('plots/7. test_sample_results.png', width = 1500, height = 1000)
print(ggplot(test.sample,aes(x=test.sample$NO2,y=test.sample$NO2_predicted_model_final))+
  geom_point(size=1, colour='orange')+
  geom_point(data = test.sample, aes(x=test.sample$NO2, y=test.sample$NO2_predicted_model_0), size=1, colour='cornflowerblue')+
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("Predictions on Actual Values") +
  xlab("Actual Values") + ylab("Predicted Values"))
dev.off()

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Plot Comparing Initial and Final Models Predictions is generated and saved in /plots!'))

# Now lets compare the 2 models (initial and final)
print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Anova Table of the Initial and Final Models:'))
cat('\n')
print(anova(multi_model_NO2_0, multi_model_NO2_final))
cat('\n')

# plot_summs(multi_model_NO2_0,multi_model_NO2_final,scale=TRUE)

# Save Intervals Comparison Plot
png('plots/8. intervals_comparison.png', width = 1500, height = 1000)
print(plot_summs(multi_model_NO2_0,multi_model_NO2_final,scale=TRUE))
dev.off()


# # # fittness of the models
# fit<-multi_model_NO2$fitted
# fit_2<-multi_model_NO2_final$fitted
# 
par(mfrow=c(4,1), mai=c(0.2,0.3,0.2,0.3))
plot(train.sample$NO2, xlab= element_blank(),ylab='',type='l',xaxt='n',lwd=2, main='Multi Model NO2',col='cornflowerblue'); grid()
lines(fit,col='red2',lwd=2)
var_plot_fit <- abs(train.sample$NO2 - fit)
plot(var_plot_fit, type = 'h',,col='cornflowerblue',xaxt='n')

plot(train.sample$NO2, type='l', xlab='',ylab='',xaxt='n',lwd=2, main='Multi Model NO2 final',col='cornflowerblue'); grid()
lines(fit_2,col='red2',lwd=2)
var_plot_fit_2 <- abs(train.sample$NO2 - fit_2)
plot(var_plot_fit_2, type = 'h',,col='cornflowerblue',xaxt='n')

