#Setup
library(tidyverse)
install.packages("visdat")
library(visdat)
install.packages("broom")
library(broom)
library(ggthemes)


airquality <- airquality
vis_dat(airquality)
#The Ozone data probably doesn't require cleaning, though it has some missing values. In this case, I think it would be best to simply drop the NA values

#Removal
airquality <- airquality %>% 
  drop_na()

#Linear regression model of Ozone predicted by the maximum daily temperature
model <- airquality %>% 
  lm(formula = Ozone ~ Temp)
summary(model) #Summary statistics; R^2 = 0.48, meaning that approximately 48% of the variation in the mean amound of ozone is explained by the maximum daily temperature. 

augment <- augment(model, airquality)
#Plot of Actual vs Predicted with a correlation of 0.7
ggplot(augment)+
  geom_point(aes(x=Ozone, y = .fitted)) +
  geom_abline(intercept = 0, slope = 1, color = "red")+ 
  labs(subtitle = paste("Correlation:", round(cor(augment$Ozone, augment$.fitted),2)),
       x = "Actual",
       y = "Predicted")+
  ggthemes::theme_clean()


