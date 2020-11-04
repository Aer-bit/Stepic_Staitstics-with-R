df <- mtcars
fit  <- glm(am ~ disp + vs + mpg, df, family = "binomial")
log_coef <- fit$coefficients
library("ggplot2")
df <- ToothGrowth
df$dose <- as.factor(df$dose)
obj <- ggplot(data = df, aes(x = supp, y = len, fill = dose))+
  geom_boxplot()

library(ROCR)
my_df <- read.csv("https://stepik.org/media/attachments/lesson/11478/data.csv")
fit  <- glm(admit ~ rank*gpa, my_df, family = "binomial")
my_df$prob  <- ifelse(is.na(my_df$admit), predict(object = fit, newdata = my_df, type = 'response'), NA)
my_df$pred <- ifelse(my_df$prob >= 0.4, 1, 0)
my_f <- subset(my_df, my_df$pred == 1)  
