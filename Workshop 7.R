library(tidyverse)
library(skimr)


coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')


skim(coffee_ratings)

#Option 1 to convert to factors (Column gets replaced by the factor since the same variable name is being used)

coffee_ratings <- coffee_ratings%>%
  mutate(
    species=as_factor(species),
    country_of_origin=as_factor(country_of_origin)
  )

#Option 2 (Direct) to convert to factors (Only one variable at a time)

coffee_ratings$owner<-as_factor(coffee_ratings$owner)



coffee_ratings%>%count(country_of_origin, sort = TRUE)

#lump some of the countries together to reduce 
coffee_ratings<-coffee_ratings%>%
  mutate(
    country_of_origin_lumped=fct_lump(country_of_origin,n=14))

coffee_ratings%>%count(country_of_origin_lumped, sort = TRUE)  


#Use ggplot with geom_col where one of the variables is country_of_origin_lumped


coffee_ratings%>% ggplot(
  aes(country_of_origin_lumped,total_cup_points))+
  geom_col()

#*part of assessment2*
coffee_ratings%>% 
  mutate(
    country_of_origin_lumped=fct_reorder(country_of_origin_lumped,total_cup_points,sum)
    )%>%ggplot(
  aes(country_of_origin_lumped,total_cup_points))+
  geom_col()+
  coord_flip()



coffee_ratings%>% 
  mutate(
    country_of_origin_lumped=fct_reorder(country_of_origin_lumped,aroma,sum)
  )%>%ggplot(
    aes(country_of_origin_lumped,aroma))+
  geom_col()+
  coord_flip()


coffee_ratings%>% 
  mutate(
    country_of_origin_lumped=fct_reorder(country_of_origin_lumped,acidity,sum)
  )%>%ggplot(
    aes(country_of_origin_lumped,acidity))+
  geom_col()+
  coord_flip()

#to have a look at the distribution of total_cup_points

coffee_ratings%>%ggplot(aes(total_cup_points))+
  geom_histogram()

#boxplot - explore your data

coffee_ratings%>%boxplot(total_cup_points)
