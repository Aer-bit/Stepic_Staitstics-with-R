library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
mydata_female <- subset(mydata, Sex == 'Female')
obj <- ggplot(data = mydata_female, aes(x = Hair, y = Freq, fill = Eye)) +
  geom_bar(stat="identity", position = 'dodge') + scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

chisq.test(HairEyeColor['Brown', , 'Female'])

df <- diamonds
t2 <- table(df$cut, df$color)
main_stat <- chisq.test(t2)[1]

mean_price <- mean(diamonds$price)
mean_carat <- mean(diamonds$carat)

diamonds$factor_price <- ifelse(diamonds$price >= mean_price,1,0)
diamonds$factor_carat <- ifelse(diamonds$carat >= mean_carat,1,0)

t1 <- table(diamonds$factor_price, diamonds$factor_carat)
chi_result <- chisq.test(t1)
main_stat <- chi_result$statistic

df <- mtcars
t2 <- table(mtcars$am, mtcars$vs)
fisher_result <- fisher.test(t2)
main_stat <- fisher_result$p.value