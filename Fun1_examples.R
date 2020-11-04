NA.position <- function(x){
  i <- 1
  s <- c()
  for (i in 1:length(x)){
    if (is.na(x[i]) == T){
      s <- c(s,i)
    }
  } 
  return(s)
}
my_vector <- c(1, -2, 3, 5, 12, 4, 2, 4, 3, 29)
NA.counter(my_vector)

NA.position <- function(x){    
  which(is.na(x))}

NA.counter <- function(x){
  i <- 1
  s <- 0
  for (i in 1:length(x)){
    if (is.na(x[i]) == T){
      s  = s + 1
    }
  } 
  return(s)
}

NA.counter <- function(x){    
  return(sum(is.na(x)))}

filtered.sum <- function(x){
  i <- 1
  s <- 0
  for (i in 1:length(x)){
    if(x[i] < 0 | is.na(x[i] == T)){
      x[i] <- 0
    }
  } 
  return(sum(x))
}
filtered.sum(my_vector)

filtered.sum <- function(x){    
  return(sum(x[x > 0], na.rm = T))}

outliers.rm <- function(x){
  limits <- quantile(as.numeric(x), probs = c(0.25, 0.75), na.rm = T)
  i <- 1
  s <- c()
  for (i in 1:length(x)){
   if (x[i] < limits[1]-1.5*IQR(x) | x[i] > limits[2] + 1.5*IQR(x)){
    s <- c(s,i) 
   } 
  }
  return(x[-s])
}
outliers.rm(my_vector)

outliers.rm <- function(x){    
  q <- quantile(x, 0.25) + quantile(x, 0.75)    
  return(x[abs(x - q/2) <= 2*IQR(x)])}
