library(plyr)
library(DataComputing)
library(geosphere)

#Load Data || Change directory
bdata <- read.csv("/Users/EllyWang/Documents/School/Stats 133/Yelp/Businesses.csv")
cdata <- read.csv("/Users/EllyWang/Documents/School/Stats 133/Yelp/Complete.csv")
MeanIncome <- read.csv("/Users/EllyWang/stat-133/ZipIncomeMean.csv")

asianf <- c("Asian Fusion","Chinese","Dim Sum", "Laotian", "Filipino",  "Indian", "Japanese", "Vietnamese", "Korean", "Sushi Bars", "Thai", "Himalayan/Nepalese", "Indonesian", "Mongolian")
mideastf <- c("Middle Eastern", "Afghan", "Falafel", "Kosher", "Pakistan", "Persian/Iranian", "Lebanese")
africanf <- c("African", "Ethiopian", "Soul Food", "Moroccan")
americanf <- c("American(New)", "American(Traditional)","Cajun", "Hawaiian", "Southern", "Burgers", "Fast Food", "Hot Dogs", "Sandwishes")
latinamericaf <- c("Latin American", "Caribbean", "Cuban", "Mexican", "Peruvian", "Argentine", "Colombian")
europef <- c("Belgain", "French", "German", "Greek", "Hungarian", "Irish", "Italian", "Mediterranean", "Polish", "Portuguese", "Russian", "Spanish", "Pizza")

#Join Restaurants and Average Reviews
id_stars <- cdata %>% group_by(business_id) %>% 
  summarize(avgStars = mean(stars, na.rm=TRUE), numReview = n())
bdata <- left_join(bdata, id_stars, by= c("business_id"= "business_id"))

#Add Zipcode
bdata <- bdata %>% 
  extractMatches("[:space:]([:digit:]+)$", full_address, ZipCode=1)
bdata$ZipCode <- strtoi(bdata$ZipCode)
bdata <- bdata %>% filter(ZipCode>10, !is.null(ZipCode))

bdata <- bdata %>% 
  mutate(categories = gsub("'|^\\[|\\]$|","",categories)) %>% 
  mutate(categories = gsub("^u","",categories)) %>% 
  mutate(categories = gsub(", u", "," ,categories))

restaurants <- bdata[grepl("Restaurants", bdata$categories),]
  
#Getting all categories
lstcat = c()
cate <- restaurants$categories
for (i in 1:length(restaurants$categories)){
  lstcat = c(unlist(strsplit(cate[i], split=",")),lstcat)
}
unique_categories <- unique(lstcat)

checkcategory <- function(category){
  #pattern = paste("(", category, ")", sep="")
  pattern = paste(category, collapse = "|")
  Categories <- grepl(pattern, restaurants$categories) 
  return(Categories)
}
Asian <- checkcategory(asianf)
MiddleEastern <- checkcategory(mideastf)
African <- checkcategory(africaf)
LatinAmerican <- checkcategory(latinamericaf)
European <- checkcategory(europef)
restaurants <- cbind(restaurants, Asian, MiddleEastern, African, LatinAmerican,
                     European)

#adding category variables
#original_names <- names(restaurants)
#new_names <- c(original_names, unique_categories)
#for (cat in unique_categories){
#  vec <- checkcategory(cat)
#  restaurants<- cbind(restaurants, vec)
#}
#names(restaurants)<- new_names

#have reviews
restaurants <- restaurants %>% filter(!is.na(numReview))

#Income
rest_Income <- left_join(restaurants, MeanIncome, by= c("ZipCode"= "Zip"))
restaurants <- rest_Income %>% 
  extractMatches("([:digit:])",attributes, price=1)
