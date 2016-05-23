#install.packages("maps")

library(devtools)
#install_github('arilamstein/choroplethrZip@v1.5.0')

#use
#install_github('arilamstein/choroplethrZip@v1.5.0')

library(choroplethr)
library(choroplethrZip)

test <- haveReview %>% 
  group_by(ZipCode) %>% 
  summarize(avgReview = mean(avgStars, na.rm = TRUE),
            JpCount = sum(Japanese), 
            ChCount = sum(Chinese),  
            KCount = sum(Korean), 
            CajunCount = sum(`Cajun/Creole`), 
            ItCount = sum(Italian), 
            GrCount = sum(Greek), 
            MedCount = sum(Mediterranean), 
            BritCount = sum(British), 
            SouthernCount = sum(Southern), 
            VietCount = sum(Vietnamese), 
            ThaiCount = sum(Thai), 
            FilipinoCount = sum(Filipino), 
            HawaiiCount = sum(Hawaiian), 
            IndianCount  =sum(Indian), 
            LebCount = sum(Lebanese), 
            TurkCount = sum(Turkish), 
            PakCount = sum(Pakistani), 
            SpanCount = sum(Spanish), 
            AmerCount = (sum(`American (New)`)+sum(`American (Traditional)`)), 
            MexCount = sum(Mexican))


zip_state <- restaurants %>% select(ZipCode, state)
zip_state <- unique(zip_state)

newtable <- left_join(test, zip_state, by = c("ZipCode" = "ZipCode"))


newtable <- newtable %>% mutate(region = as.character(ZipCode))
newtable <- newtable %>% mutate(value = avgReview)


not_included_zip <- c(28246, 85218, 85219, 85220, 85222, 85228, 85232, 85239, 85242, 85358, 85378, 89152)
shorttable <- newtable %>% filter(! region %in% not_included_zip)
#zip_choropleth(shorttable)


shorttable %>% filter(state == "NC") %>% zip_choropleth(state_zoom = "north carolina")

shorttable %>% zip_choropleth(state_zoom = c("north carolina", "pennsylvania", "nevada", "arizona", "wisconsin"))
