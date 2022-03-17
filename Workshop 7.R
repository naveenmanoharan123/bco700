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


#Session 8
#variation exploration 

ggplot(data=coffee_ratings)+geom_bar(mapping=aes(x=aroma), fill="navy")


coffee_ratings%>%
  filter(!is.na(aroma))%>%
  ggplot() + geom_bar(mapping=aes(x=aroma))+ coord_flip()


coffee_ratings%>%
  filter(!is.na(color))%>%
  ggplot() + geom_bar(mapping=aes(x=color))+ coord_flip()


#continuous variables
coffee_ratings%>%ggplot() + 
  geom_histogram(mapping=aes(x=cupper_points, binwidth = 0.5))


coffee_ratings%>%ggplot() + 
  geom_histogram(mapping=aes(x=number_of_bags))




#Use Geom_smooth

coffee_ratings%>%
  ggplot(aes(x=acidity, y=aftertaste, color = species))+
  geom_point(size = 1.5) +
  geom_smooth(method = lm, se=FALSE, colour = "blue")

coffee_ratings%>%
  ggplot(aes(x=acidity, y=aftertaste, color = species))+
  geom_point(size = 1.5) +
  geom_smooth(method = lm, se=FALSE, colour = "blue")



coffee_ratings%>%
  filter(flavor > 5)%>%
  ggplot(aes(x=moisture, y=flavor))+
  geom_point(size = 1.5) +
  geom_smooth(method = lm, se=FALSE, colour = "blue")


#Simple Linear regression

lm(total_cup_points~altitude_mean_meters, data = coffee_ratings)

lm(total_cup_points~altitude_mean_meters + acidity, data = coffee_ratings)

results<-lm(total_cup_points~altitude_mean_meters + acidity, data = coffee_ratings)
summary(results)
































