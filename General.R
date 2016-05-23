library(plyr)
library(DataComputing)
library(geosphere)

#Load Data || Change directory
bdata <- read.csv("/Users/EllyWang/Documents/School/Stats 133/Yelp/Businesses.csv")
cdata <- read.csv("/Users/EllyWang/Documents/School/Stats 133/Yelp/Complete.csv")

#Join Restaurants and Average Reviews
restaurants <- bdata %>% filter(Restaurant==TRUE)
id_stars <- cdata %>% group_by(business_id) %>% 
  summarize(avgStars = mean(stars, na.rm=TRUE), numReview = n())
restaurants <- left_join(restaurants, id_stars, by= c("business_id"= "business_id"))

#Add Zipcode
restaurants <- restaurants %>% 
  extractMatches("[:space:]([:digit:]+)$", full_address, ZipCode=1)
restaurants$ZipCode <- strtoi(restaurants$ZipCode)
restaurants <- restaurants %>% filter(ZipCode>10, !is.null(ZipCode))

restaurants <- restaurants %>% 
  mutate(categories = gsub("'|^\\[|\\]$|","",categories)) %>% 
  mutate(categories = gsub("^u","",categories)) %>% 
  mutate(categories = gsub(", u", "," ,categories))

#Getting all categories
lstcat = c()
cate <- restaurants$categories
for (i in 1:length(restaurants$categories)){
  lstcat = c(unlist(strsplit(cate[i], split=",")),lstcat)
}
unique_categories <- unique(lstcat)

checkcategory <- function(category){
  pattern = paste("(", category, ")", sep="")
  Categories <- grepl(pattern, restaurants$categories) 
  return(Categories)
}

#adding category variables
original_names <- names(restaurants)
new_names <- c(original_names, unique_categories)
for (cat in unique_categories){
  vec <- checkcategory(cat)
  restaurants<- cbind(restaurants, vec)
}
names(restaurants)<- new_names
  