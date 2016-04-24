# Stat 133 Final Project: Yelp
# Avery Kan: What kind of restaurant have large variations in the ratings?

# Calculate the standard deviation of the ratings for each type of restaurant
Businesses <- read.csv("C:/Users/user/Google Drive/Stat 133 Final Project/Businesses.csv")
Complete <- read.csv("C:/Users/user/Google Drive/Stat 133 Final Project/Complete.csv", comment.char="#")

Counts <- 
  Complete %>%
  group_by(business_id) %>%
  tally(sort=TRUE)
# Cleaning up
Businesses <- 
  Businesses%>%
  mutate(attributes= gsub("u'","",attributes),
         categories= gsub("u'","",categories),
         neighborhoods=gsub("u'","",neighborhoods))
# filter for open restaurants
Open <- 
  Businesses %>% 
  filter(grepl("Restaurant",categories), as.character(open) == "True")

# Inner-join to form Reviews table
Reviews <- Open %>%
  select(name,business_id,city,categories) %>%
  inner_join(Complete[,-(1:4)],by= c("business_id"="business_id"))

AvgReviews <- 
  Reviews %>%
  select(name, city, categories, stars) %>% 
  group_by(name) %>%
  mutate_each(funs(mean(., na.rm=TRUE)), stars) %>% 
  arrange(desc(stars)) 

Avg <- 
  AvgReviews[!duplicated(AvgReviews[,1]),]

test <- Avg %>% 
  mutate(categories= gsub("', Restaurants'","",categories) )
test <- test %>%
  mutate(categories= gsub("Restaurants'","",categories))

Types
testcount <- 
  test %>% 
  group_by(matches(),categories) %>% 
  tally(sort=TRUE)