test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
fill_na <- function(x){
  fit_model <- lm(x[[3]] ~ x[[1]] + x[[2]], data = x, na.action = 'na.exclude')
  x$y_full <- x[[3]]
  new_var <- data.frame(x[[3]])
  x$y_full <- ifelse(is.na(x$y_full), predict(fit_model, new_var), x$y_full)
  return(x)
}

fill_na <- function(my_df){    
  fit <- lm(y ~ x_1+x_2, my_df)    
  my_df$y_full = ifelse(is.na(my_df$y), predict(fit, my_df), my_df$y)    
  return(my_df)}

df <- mtcars[, c(1,3,4,5,6)]
model <- lm(wt ~ mpg + disp + hp, df)
summary(model)

df <- attitude
model <- lm(rating ~ complaints*critical, df)
summary(model)

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point()

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
model <- lm(mpg ~ wt*am, mtcars)
summary(model)


ggplot(mtcars, aes(x = wt, y = mpg, col = am)) + 
  geom_smooth(method = 'lm')

model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)

ideal_model <- step(model_full, direction = 'backward')

anova(model_full, ideal_model)

ideal_model <- step(model_full, scope = list(lower = model_null, upper = model_full), direction = c("both", "backward", "forward"))

model <- lm(as.formula('sr ~ . ^2'), LifeCycleSavings)
?formula
