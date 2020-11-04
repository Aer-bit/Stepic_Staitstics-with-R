library(dplyr)
library(ggplot2)
d <- data_frame(diamonds)
?seq
slice(diamonds, seq(2, nrow(d), 2))

mtcars %>%
  select(mpg,hp,am,vs) %>%
  filter(mpg > 14, hp > 100) %>%
  arrange(desc(mpg)) %>%
  slice(1:10) %>%
  rename('Miles per gallon' = mpg, 'Gross horsepower' = hp)

all_to_factor <- function(x){
  mutate_each(x, funs(as.factor(.)))
}

### Log transorm(rescaling)

f_log <- function (x) {
  if (is.numeric(x)){
    return(log(((x-min(x))/(max(x)-min(x)))+1))
  } else {
    return(x)
  }
}

log_transform <- function(test_data){
  mutate_each(d, funs(f_log))
}
log_transform(d)

####

test_data <- read.csv("https://stepic.org/media/attachments/course/724/salary.csv")
test_data_gr <- group_by(test_data, gender, country)
summarise(test_data_gr,
          numbers = n(),
          mean = mean(salary, na.rm = T), 
          sd = sd(salary, na.rm = T), 
          median = median(salary, na.rm = T), 
          first_quantile = quantile(salary, na.rm = T)[2],
          third_quantile = quantile(salary, na.rm = T)[4],
          na_values = sum(is.na(salary)))

####

fact <- function(x) {
  ifelse(x > mean(x, na.rm = T), x <- 1, x <- 0)
}
to_factors <- function(test_data, factors){
  test_data %>%
  mutate_at(factors, fact) %>%
  mutate_at(factors, funs(as.factor))
}

####

 
df <- diamonds
df_gr <- group_by(diamonds, color)
high_price <-df_gr %>%
  arrange(-price) %>%
  slice(1:10) %>%
  select(color, price)
head(high_price)
