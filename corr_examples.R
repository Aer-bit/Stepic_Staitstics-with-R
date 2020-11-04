corr.calc <- function(df){
  df$var1 <- df[, 1]
  df$var2 <- df[, 2]
  res <- cor.test(x = df$var1, y = df$var2)
  return(c(res$estimate, res$p.value))
}

corr.calc <- function(test_data){    
  fit  <- cor.test(test_data[[1]], test_data[[2]])    
  r <- fit$estimate    
  p <- fit$p.value    
  return(c(r, p))}

filtered.cor <- function(test_data){    
  test_data_num <- test_data[, sapply(test_data, is.numeric)]
  corr_result <- corr.test(test_data_num)
  diag(corr_result$r) <- 0
  min_val <- which.min(corr_result$r)
  max_val <- which.max(corr_result$r)
  ifelse(abs(corr_result$r[min_val]) >= corr_result$r[max_val], val <- corr_result$r[min_val], val <- corr_result$r[max_val])
  return(val)
}

filtered.cor <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))    
  cor_mat <- cor(x[, num_var])    
  diag(cor_mat) <- 0    
  return(cor_mat[which.max(abs(cor_mat))])}

smart_cor <- function(df){
  df$var1 <- df[, 1]
  df$var2 <- df[, 2]
  res_wil1 <- shapiro.test(df$var1)
  res_wil2 <- shapiro.test(df$var2)
  if (res_wil1$p.value < 0.05 | res_wil2$p.value < 0.05){
    res <- cor.test(df$var1, df$var2, method = 'spearman')
  }
  else {
    res <- cor.test(df$var1, df$var2, method = 'pearson')
  }
  return(res$estimate)
}

test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")

smart_cor <- function(x){    
  if (shapiro.test(x[[1]])$p < 0.05 | shapiro.test(x[[2]])$p < 0.05) {    
    return(cor.test(x[[1]], x[[2]], method = 'spearman')$estimate)    
  } else {    
    return(cor.test(x[[1]], x[[2]], method = 'pearson')$estimate)}}

my_data <- read.table('dataset_11508_12.txt')
fit <- lm(V1 ~ V2, my_data)
summary(fit)

data_for_model <- subset(diamonds, cut == 'Ideal' & carat == 0.46)    
fit <- lm(price ~ depth, data_for_model)    
fit_coef <- fit$coefficients

regr.calc <- function(x){
  if ((cor.test(x[[1]], x[[2]], method = 'pearson'))$p.value < 0.05){    
    mod1 <- lm(x[[1]] ~ x[[2]], x)
    new_var <- data.frame(x[[1]])
    x$fit <- predict(mod1, new_var)
    return(x)
  } else { 
    return('There is no sense in prediction')}}

regr.calc <- function(sample_data){    
  cor_result = cor.test(~sample_data[[1]] + sample_data[[2]])    
  if (cor_result$p.value < 0.05){    
    fit_model  <- lm(sample_data[[1]] ~ sample_data[[2]])    
    sample_data$fit  <- fit_model$fitted.values    
    return(sample_data)    
  } else {    
    return('There is no sense in prediction')}}

ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species))+
  geom_point(size = 1)+
  geom_smooth(method = "lm")
