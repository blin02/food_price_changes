library(googleCharts)
library(dplyr)

prepare_dataframe_google = function () {
  
  food.price = read.csv("./data/food/Food_Price_Transpose.csv", na.strings = "NA")
  #food.price = food.price[, c(length(food.price), 2:(length(food.price)-1))]
  
  #************************ transform food price index data ******************
  food.price.google = data.frame(year=integer(), food.price=double(), category=factor())
  
  for(i in 2:(ncol(food.price)-1)) {
    col = colnames(food.price[i])
    dataToBind = mutate(select(food.price, year, food.price = one_of(col)), category=as.factor(col))
    
    food.price.google = rbind(food.price.google, dataToBind)
  }

  #************************ transform producer price index data ******************
  producer.price = read.csv("./data/producer/Producer_Price_Transpose.csv", na.strings = "NA")
  producer.price.google = data.frame(year=integer(), producer.price=double(), category=factor())
  
  for(i in 2:(ncol(producer.price)-1)) {
    col = colnames(producer.price[i])
    dataToBind = mutate(select(producer.price, year, producer.price = one_of(col)), category=as.factor(col))
    producer.price.google = rbind(producer.price.google, dataToBind)
  }
  
  #************************ combined food price and producer price based on category ******************
  combined_data = inner_join(food.price.google, producer.price.google, by = c("year", "category"))
  
  
  category_percent = read.csv("./data/category_percent.csv")

  #************************ combined with food category importance ******************
  combined_data_with_percent = inner_join(combined_data, category_percent, by = "category")
  combined_data_with_percent$category = as.factor(combined_data_with_percent$category)
  combined_data_with_percent$group = combined_data_with_percent$category

  return (combined_data_with_percent)
  
}

normalize = function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

