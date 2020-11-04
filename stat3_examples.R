####

library(car)

hetero_test <-  function(test_data){
  zav <- test_data[,1]
  nezav <- test_data[,-1]
  fit <- lm(zav ~ ., nezav)
  df$resid <- fit$residuals^2
  fit2 <- lm(df$resid ~ ., nezav)
  return (summary(fit2)$r.squared)
}

hetero_test(mtcars)

####

VIF <-  function(test_data){
  pred <- test_data[,-1]
  fit <- lm(test_data[,1] ~ ., pred)
  if (length(pred) > 2){ 
  v <- sapply(seq_along(pred), function(i){
    (summary(lm(pred[,i] ~ ., pred[,-i]))$r.squared)
  })
  } else {
    v1 <- summary(lm(pred[[1]] ~ pred[[2]], pred))$r.squared
    v2 <- summary(lm(pred[[2]] ~ pred[[1]], pred))$r.squared
    v <- c(v1, v2)
    }
  names(v) <- names(pred)
  return (1/(1-v))
}
v <- VIF(mtcars)

####

smart_model <-  function(test_data){
  v <- VIF(test_data)
  while (TRUE){ 
  if (length(v[v>10]) >= 1){
    test_data <- test_data[-which.max(v)]
  } else { break }}
  return(lm(test_data[,1] ~., test_data[,-1])$coefficients) 
}
smart_model(mtcars)
df <- mtcars

length(v[v > 10]) >= 1
df[-names(which.max(v))]
library(dplyr)
select(df, names(df) != 'hp')

####
library(ggplot2)
exp_data <- read.csv('https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/politeness_data.csv')
exp_data$scenario <-  factor(exp_data$scenario)

plot_1 <- ggplot(exp_data, aes(x = scenario, y = frequency, fill = attitude)) +
          geom_boxplot()
####

ggplot(exp_data, aes(x = frequency, fill = subject))+
  facet_grid(gender ~ .)+
  geom_density()

####

library(lme4)
fit1 <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), data = exp_data)
