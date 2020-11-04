qplot(x = x, data = diamonds, color = I('black'), geom = 'density')

qplot(x = x, color = cut, data = diamonds, geom = 'density')

df <- diamonds

qplot(x = color, y = price, data = diamonds, geom = 'violin')

my_plot <- ggplot(mtcars, aes(factor(am), mpg), color = 'black', fill = 'white')+
  geom_violin()+
  geom_boxplot(width = 0.2) 

DF <- read.csv('sales.csv')
sales = read.csv("https://stepic.org/media/attachments/course/724/sales.csv")

ggplot(sales, aes(income, sale)) +
  geom_point(aes(color = shop))+
  geom_smooth(color = 'blue')

ggplot(sales, aes(shop, income, color = season)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange", position = position_dodge(0.2))
  
ggplot(sales, aes(date, sale, color = shop))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', position = position_dodge(0.2)) + # добавим стандартную ошибку
  stat_summary(fun.data = mean_cl_boot, geom = 'point', position = position_dodge(0.2)) + # добавим точки
  stat_summary(fun.data = mean_cl_boot, geom = 'line') # соединим линиями

ggplot(mtcars, aes(mpg))+
  geom_dotplot()+
  facet_grid(am ~ vs)

ggplot(iris, aes(Sepal.Length)) +
  geom_density()+
  facet_wrap(~ Species)

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  facet_wrap(~ Species) +
  geom_point()+
  geom_smooth()

df <- read.csv('myMovieData.csv')
ggplot(df, aes(Type, Budget)) +
  facet_grid(.~ Year) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_color_discrete(name = "Вид цветка",
                       labels = c("Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский"))+
  scale_x_continuous(name = "Длина чашелистика",
                     breaks = seq(4,8,1),
                     limits = c(4,8))+ 
  scale_y_continuous(name = "Длина лепестка",
                     breaks = seq(1,7,1),
                     limits = c(1,7))
  
  
