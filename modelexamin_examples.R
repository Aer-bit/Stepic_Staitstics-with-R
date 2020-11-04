my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)


beta.coef <- function(x){
  x[[1]] <- scale(x[[1]])
  x[[2]] <- scale(x[[2]])
  model <- lm(x[[1]] ~ x[[2]], x)
  return(model$coefficients)
}

beta.coef(mtcars[,c(1,3)])

?QuantPsyc

normality.test  <- function(x){
 v1 <- c()
 i <- 1
  for (i in 1:length(x)){
   v1 <- c(v1, shapiro.test(x[ ,i])$p.value)
  }
  names(v1) <- names(x)
  return(v1)
}

normality.test  <- function(x){    
  return(sapply(x, FUN =  shapiro.test)['p.value',])}

df <- read.csv('homosc.csv')
x <- lm(DV ~ IV, df)
shapiro.test(x$residuals)$p.value
summary(x)

resid.norm  <- function(fit){
  df <- data.frame(fit$residuals)
  if (shapiro.test(fit$residuals)$p.value < 0.05){
    p1 <- ggplot(df, aes(fit.residuals))+
      geom_histogram(bins = 30, fill = 'red')
  } else{
   p1 <- my_plot <- ggplot(df, aes(fit.residuals))+
      geom_histogram(bins = 30, fill = 'green')
  }
  return(p1)
}

resid.norm <- function(fit) {    
  resid.norm.pv <- shapiro.test(fit$residuals)$p.value    
  plt <- ggplot(data.frame(fit$model), aes(x = fit$residuals)) +    
    geom_histogram(fill = ifelse(resid.norm.pv < 0.05, 'red', 'green'))    
  return(plt)}

