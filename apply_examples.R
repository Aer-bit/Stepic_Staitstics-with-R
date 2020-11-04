test_data <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), V2 = c(NA, -10.2, -10.1, -9.3, -12.2), V3 = c(NA, NA, -9.3, -10.9, -9.8)))
get_negative_values <- function(test_data){
    library(purrr)
    y <- apply(test_data, 2, function(x) x[x < 0 & is.na(x)== F])
    return(compact(y))
}

get_negative_values <- function(test_data){    
  negative_col <- apply(test_data, 2, function(x) any(x[!is.na(x)] < 0))    
  return(apply(test_data[negative_col], 2, function(x) x[!is.na(x) & x <0]))}

####

test_data <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))

na_rm  <- function(x){
  df <- apply(x, 2, function(x) replace(x, which(is.na(x)), mean(x, na.rm =T)))
  return(as.data.frame(df))
}

na_rm  <- function(x){    
  na_to_mean  <- function(v){    
    v[is.na(v)]  <- mean(v , na.rm = T)    
    return(v)}    
  result  <- as.data.frame(apply(x, 2, na_to_mean))}

####

d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))

positive_sum <-  function(test_data){
   y <- as.list(test_data)
   l <- lapply(y, function(x) sum(x[x>0], na.rm = T))
   return(l)  
}

####

test_data <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")

fun_ger <- function(x){
  any(sapply(names, function(y) grepl(y, x)))
}

my_names <- function (dataset, names){
  dataset[sapply(dataset[[1]], fun_ger),]
}

#### Normality check and multiple regression

smart_lm <- function(x){
  df_minus1 <- x[2:length(x)]
  pl <- apply(df_minus1, 2, shapiro.test)
  p <- sapply(pl, function (x) x$p.value)
  n <- which(p>0.05)
  if(length(n) > 0){
    return(lm(x[[1]] ~., data = df_minus1[n])$coefficients)
  } else {       
    return("There are no normal variables in the data")
  }
}

smart_lm <- function(x){    
  check_norm <- sapply(x[-1], function(var) shapiro.test(var)$p.value > 0.05)    
  if (any(check_norm)){    
    x = x[, c(1, (which(check_norm) + 1))]    
    coef <- lm(x[[1]] ~ ., x[-1])$coef    
    return(coef)    
  } else{    
    return('There are no normal variables in the data')}}

#### Apply t-test to all numeric variables in df

one_sample_t <- function(test_data, general_mean){
  df_num <- test_data[, sapply(test_data, is.numeric)]
  res <- lapply(df_num, function(x) c(t.test(x, mu = general_mean)$statistic, 
                        t.test(x, mu = general_mean)$parameter,
                        t.test(x, mu = general_mean)$p.value))
  return(res)
}

one_sample_t <- function(test_data, general_mean){    
  apply_t_test <- function(col){    
    result <- t.test(col, mu = general_mean)    
    return(c(result$statistic, result$parameter, result$p.value))}    
  num_var <- sapply(test_data, function(x) is.numeric(x))    
  lapply(test_data[num_var], apply_t_test)}

####Get p value from list

get_p_value <- function(test_list){
  lapply(test_list, function(x) x$p.value)
}

normality_tests <- lapply(iris[, 1:4], shapiro.test)
get_p_value(normality_tests)
