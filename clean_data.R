setwd("/Users/binlin/Documents/DataScience/projects/Shiny Project/food_price_changes/data/")


category_percent = read.csv("./category_percent.csv")

category_percent = filter(category_percent, !(category %in% c('All.food', 'Food.at.home', 'Meats')))

category_percent[order(-category_percent$Percent),] 

#******************* Transpose food price index data ******************
food.price = read.csv("./food/Food_Price_Clean.csv", na.strings = "N/A")


food.price.transpose = setNames(data.frame(t(food.price[,-1])), food.price[,1])

food.price.transpose$year  = as.integer(substr(rownames(food.price.transpose), 2, 5))

food.price.transpose
dim(food.price.transpose)

is.na(food.price.transpose)

food.category.cor = select(food.price.transpose, -year, -All.food)
food.category.cor
cor(food.category.cor)

#write.csv(food.price.transpose, "./food/Food_Price_Transpose.csv")

colSums(is.na(food.category.cor))

colnames(food.category.cor)[colSums(is.na(food.category.cor)) > 0]


#******************* Transpose producer price index data ******************
producer.price = read.csv("producer/producer_price.csv", na.strings = "N/A")

producer.price.transpose = setNames(data.frame(t(producer.price[,c(-1, -2)])), producer.price[,2])

producer.price.transpose$year  = substr(rownames(producer.price.transpose), 2, 5)

dim(producer.price.transpose)

colSums(is.na(producer.price.transpose))

write.csv(producer.price.transpose, "producer/Producer_Price_Transpose.csv")
