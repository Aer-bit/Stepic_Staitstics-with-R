sample.products <- data.table(price = c(10000, 600000, 700000, 1000000),
                              brand = c("a", "b", "c", "d"),
                              available = c(T, T, F, T))


filter.expensive.available <- function(products, brands) {
  products[price >= 500000 & brand %in% brands & available]
}

filter.expensive.available(sample.products, c("a", "c", "d"))

####

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

ordered.short.purchase.data <- function(purchases) {
  dt1 <- purchases[order(-price)]
  dt1[quantity > 0, list(ordernumber, product_id)]
}

ordered.short.purchase.data(sample.purchases)

ordered.short.purchase.data<- function(purchases) {    
  purchases[order(-price)][quantity >= 0][, .(ordernumber, product_id)]}

####

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)





sample.purchases[j = quantity > 0, by = ordernumber]$price
  sample.purchases[quantity > 0]$quantity]

dt <- sample.purchases[quantity > 0, .(tot = sum(price*quantity)), by = ordernumber]
median(dt$tot)


  
}