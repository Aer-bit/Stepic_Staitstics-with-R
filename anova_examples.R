df <- ToothGrowth

v1 <- subset(df, supp == 'OJ' & dose == '0.5')
v2 <- subset(df, supp == 'VC' & dose == '2')

t_result <- t.test(v1$len, v2$len)
t_stat <- t_result$statistic

df_lek <- read.csv('lekarstva.csv')
t_result <- t.test(df_lek$Pressure_before, df_lek$Pressure_after, paired = T)
t_stat <- t_result$statistic

df1 <- read.table('dataset_11504_16.txt')
bartlett.test(V1  ~ V2, df1)
t_result <- t.test(df1$V1, df1$V2, var.equal = F)

wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

df <- npk
fit4 <- aov(yield ~ N + P + K, data=df)
summary(fit4)

mydata <- iris
fit5 <- aov(Sepal.Width ~ Species, data=mydata)
summary(fit5)


TukeyHSD(fit5)

df <- read.csv('Pillulkin.csv')
df$patient <- as.factor(df$patient)
fit3 <- aov(temperature ~ pill + Error(patient/pill), data = df)
summary(fit3)

df <- read.csv('Pillulkin.csv')
df$patient <- as.factor(df$patient)
fit4 <- aov(temperature ~ pill * doctor + Error(patient/(pill*doctor)), data = df)
summary(fit4)

library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
  
