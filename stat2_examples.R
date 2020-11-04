df <- table(mtcars[,c("am", "vs")])
c <- chisq.test(df)
f <- fisher.test(df)
df <- table(mtcars[1:20,c("am", "vs")])

smart_test <- function(x){
  t <- table(x) 
  if (length(t[t<5]) >= 1){
    return(fisher.test(t)$p.value)
  } else {
    return(c(chisq.test(t)$statistic, chisq.test(t)$parameter, chisq.test(t)$p.value))
  }
}

smart_test(mtcars[,c("am", "vs")])

####

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
chisq.test(test_data$V1)
?chisq.test
chisq.test(table(test_data$V1))$p.value
most_significant <-  function(data){
  res <- apply(data, 2, function(x) chisq.test(table(x))$p.value)
  return(names(res[res == min(res)]))
}
most_significant(test_data)

####

df <- iris
df$important_cases <- factor(levels = c(0,1), labels = c("Yes", "No"))
vals <- sapply(iris[, 1:4], function(x) ifelse(x > mean(x), 1, 0)) 
iris$important_cases <- apply(vals, 1, sum)
iris$important_cases <- sapply(iris$important_cases, function(x) ifelse(x >= 3, 'Yes', 'No'))
iris$important_cases <- as.factor(iris$important_cases)

table(iris$important_cases)

####

test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

get_important_cases <- function(data){
  num <- sapply(data, function(x) ifelse(x > mean(x), 1, 0))
  num2 <- apply(num, 1, function(x) ifelse(sum(x) > length(x)/2, 1, 0))
  data$important_cases <- factor(num2, levels = c(0,1), labels = c('No', 'Yes'))
  return(data)
}

get_important_cases(test_data)

####

v <- c(1, 2, 3, 3, 3, 4, 5)
v <- c(1, 1, 1, 2, 3, 3, 3)
t <- table(v)
as.numeric(names(t[t == max(t)]))

####

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
resid <- chisq.test(table(test_data))$stdres

max_resid <- function(test_data){
  resid <- chisq.test(table(test_data))$stdres
  c <- colnames(resid)
  r <- rownames(resid)
  rows <- apply(resid,1, max)
  cols <- apply(resid,2, max)
  return(c(r[which.max(rows)],c[which.max(cols)]))
}

####
df <- group_by(diamonds, color)
ggplot(diamonds, aes(x =color, fill = cut))+
  geom_bar(position = 'dodge')

####

test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
test_data  <- transform(test_data, x = factor(x), y = factor(y)) 

get_coefficients <- function(dataset){
  m1 <- glm(dataset[[2]] ~ dataset[[1]], dataset, family = 'binomial')
  return(exp(m1$coefficients))
}

get_coefficients(test_data)

####

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
var_names = c("X4", "X2", "X1")

centered <- function(test_data, var_names){
  test_data[var_names] <- sapply(test_data[var_names], function(x) x - mean(x))
  return(test_data)
}

centered(test_data, var_names)

####

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")

m1 <- glm(is_prohibited ~ ., test_data, family = 'binomial')
result <- anova(m1, test = "Chisq")
which((result$`Pr(>Chi)`) < 0.05)
names(test_data[which((result$`Pr(>Chi)`) < 0.05)])
p <- result$`Pr(>Chi)`
any(p < 0.05, na.rm = T)

get_features <- function(dataset){
  m1 <- glm(is_prohibited ~ ., dataset, family = 'binomial')
  result <- anova(m1, test = "Chisq")
  pvals <- result$`Pr(>Chi)`
  if (any(pvals< 0.05, na.rm =T)) {
    return(names(dataset[which(pvals < 0.05)]))
  } else {
    return(print("Prediction makes no sense"))
  }
}

get_features(test_data)

####

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

most_suspicious <- function(test_data, data_for_predict){
  m1 <- glm(is_prohibited ~ ., test_data, family = 'binomial')
  prob <- predict(m1, newdata = data_for_predict, type = 'response')
  n <- which(prob == max(prob))
  return(as.character(data_for_predict[n, 5]))
}

most_suspicious(test_data, data_for_predict)

####

normality_test <- function(dataset){
  dataset <- dataset[, sapply(dataset, is.numeric)] 
  st <- apply(dataset, 2, shapiro.test)
  pvals <- sapply(st, function(x) x$p.value)
  return(pvals)
}

normality_test(iris)

####

test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")

smart_anova <- function(test_data){
  s1 <- shapiro.test(filter(test_data, y == "A")[,1])$p.value
  s2 <- shapiro.test(filter(test_data, y == "B")[,1])$p.value
  s3 <- shapiro.test(filter(test_data, y == "C")[,1])$p.value
  bt <- bartlett.test(test_data[,1], test_data[,2])$p.value
  if(s1 >= 0.05 | s2 >= 0.05 | s3 >= 0.05 | bt >= 0.05){
    fit <- aov(x ~ y, test_data)
    p_value <- summary(fit)[[1]]$'Pr(>F)'[1]
    names(p_value) <- 'ANOVA'
    return(p_value)
  } else {
    p_value <- kruskal.test(test_data[,1], test_data[,2])$p.value
    names(p_value) <- 'KW'
    return(p_value)
  }
}

smart_anova(test_data)
library(dplyr)
smart_anova <- function(test_data){
  s1 <- shapiro.test(subset(test_data, y == 'A')[,1])$p.value
  s2 <- shapiro.test(subset(test_data, y == 'B')[,1])$p.value
  s3 <- shapiro.test(subset(test_data, y == 'C')[,1])$p.value
  bt <- bartlett.test(x ~ y, test_data)$p.value
  if(s1 < 0.05 | s2 < 0.05 | s3 < 0.05 | bt < 0.05){
    p_kw <- kruskal.test(x ~ y, test_data)$p.value
    names(p_kw) <- 'KW'
    return(p_kw)   
  } else {
    fit <- aov(x ~ y, test_data)
    p_anova <- summary(fit)[[1]]$'Pr(>F)'[1]
    names(p_anova) <- 'ANOVA'
    return(p_anova)
  }
}

bartlett.test(x ~ y, test_data)$p.value
kruskal.test(x ~ y, test_data)$p.value

test_data <- as.data.frame(list(x = c(1.34, -0.6, -1.41, -2.52, 2.69, 2.37, 0.16, 2.28, -0.41, 18, 0.02, 0.36, 1.43, 1.36, 0, 0.01, 0.19, 0.09, 0, 10, 0.12, 0.01, 0.04, 1.97, 0.8, 0.39, 0.49, 1.09, 0.69, 15), y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)))
test_data$y <-  factor(test_data$y, labels = c('A', 'B', 'C'))

test_data <- as.data.frame(list(x = c(1.26, -2.33, -0.05, 2.13, -0.87, 0.6, 1.95, 0.36, -1.76, 15, 0.12, 0.06, 0.15, 0.4, 3.49, 0, 1.28, 0, 0.43, 10, 3.43, 0.08, 1.01, 4.55, 0.16, 2.77, 0.86, 0.43, 0.01, 18), y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)))
test_data$y <-  factor(test_data$y, labels = c('A', 'B', 'C'))

####


normality_by(test_data)
normality_by(mtcars[, c("mpg", "am", "vs")])
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")


normality_by <- function(test){
  names_initial <- names(test)
  names(test) <- c('a', 'b', 'c')
  test <- mutate(test, b = factor(b), c = factor(c))
  test_gr <- group_by(test, b, c)
  res <- summarise(test_gr, p_value = shapiro.test(a)$p.value)
  names(res) <- c(names_initial[2], names_initial[3], 'p_value')
  return(res)
}

library(dplyr)    
get_p_value <- function(x){      
  shapiro.test(x)$p.value    
}    
normality_by <- function(test){    
  grouped <- test %>%    
    group_by_(.dots = colnames(.)[2:3]) %>%         
    summarise_each(funs(get_p_value))         
  names(grouped)[3] <- 'p_value'         
  return(grouped)         
}

####

ggplot(iris, aes(Sepal.Length, fill = Species))+
  geom_density(alpha = 0.2)

####

d <- iris[, c("Sepal.Length", "Petal.Width")]

fit <- kmeans(d, 3)
d$clusters <- factor(fit$cluster)

ggplot(d, aes(Sepal.Length, Petal.Width, col = clusters))+
  geom_point(size = 2)+
  theme_bw() 

####

library(ape)
set.seed(222)
tr <- rtree(20, tip.label = c("B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")) 
#левое дерево
plot.phylo(tr) 
#правое дерево 
plot.phylo(tr, use.edge.length=FALSE)

####

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")

smart_hclust<-  function(test_data, cluster_number){
  dist_matrix <- dist(test_data)
  fit <- hclust(dist_matrix)
  cluster <- cutree(fit, cluster_number)
  test_data$cluster <- factor(cluster)
  return(test_data)
}
smart_hclust(test_data, 3)

####

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")

get_difference<-  function(test_data, n_cluster){
  dist_matrix <- dist(test_data)
  fit <- hclust(dist_matrix)
  cluster <- factor(cutree(fit, n_cluster))
  p_vec <- apply(test_data, 2, function(x) {fit <- aov(x ~ cluster, test_data)
  return(summary(fit)[[1]]$'Pr(>F)'[1]) })
  return(names(which(p_vec < 0.05)))
}

get_difference(test_data, 2)

####
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")

pc <- prcomp(test_data)
pc$x[,c(1,2)]

get_pc <- function(d){
  pc <- prcomp(d)
  d$PC1 <- pc$x[,1]
  d$PC2 <- pc$x[,2]
  return(d)
}

get_pc(test_data)

####

dist_matrix <- dist(swiss)
fit <- hclust(dist_matrix)
swiss$cluster <- factor(cutree(fit, 2))

my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster))+
  geom_smooth(method = 'lm')+
  geom_point()
