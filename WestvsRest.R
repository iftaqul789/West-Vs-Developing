library(dslabs)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
data('gapminder')
str(gapminder)

# gdp null values

print(is.na((gapminder$gdp)))

gapminder <- mutate(gapminder,per_day_gdp = gapminder$gdp / 365)

str(gapminder)

#To compare the same countries both in 1970 and 2010

country_list1 <- gapminder %>% filter(year == 1970 & !is.na(per_day_gdp)) %>%.$country
country_list2 <- gapminder %>% filter(year == 2010 & !is.na(per_day_gdp)) %>%.$country
countries <- intersect(country_list1,country_list2)

#Per day Gdp of countries aroound the world in 1960
#countries are color plotted according to their contininents as the goal is to compare west vs rest.
# clearly there is a huge difference between and west and the rest of countries in terms of wealth i the year 1960.
#It can be seen from the graph the western countries are doing good comparatively than other developing countries.
# BUt is this scenario still exits ???

gapminder %>% filter(year == 1960 & !is.na(gdp)) %>% mutate(per_day_gdp = gdp/population/365, region = reorder(region,per_day_gdp, FUN = median)) %>%
  ggplot(aes(region,per_day_gdp, fill = continent)) + geom_boxplot()+theme_bw() + theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  scale_y_continuous(trans = "log2") + ggtitle("Per Day GDP 1960") + geom_point() + scale_color_discrete("Continent") +
  theme(legend.position = "top")

# BUt is this scenario still exits ???
#per day GDP plot of countries in 2010

gapminder %>% filter(year == 2010 & !is.na(gdp)) %>% mutate(per_day_gdp = gdp/population/365, region = reorder(region,per_day_gdp, FUN = median)) %>%
  ggplot(aes(region,per_day_gdp, fill = continent)) + geom_boxplot()+theme_bw() + theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  scale_y_continuous(trans = "log2") + ggtitle("Per Day GDP 1960") + geom_point() + scale_color_discrete("Continent") +
  theme(legend.position = "top")

# from the plot it is clear that most of the developing countries has done very good and imporoved so much in terms of per day GDP.
# Souther Asia is moved right and imporved .
#Eastern asia is now doing much better than of western countries .
# So the saying of west vs rest is no longer the same as compared to 1960.



# Compararison plot of GDP 1960 & 2010
gapminder %>%filter(country %in% countries & !is.na(gdp)& year == c(1960,2011)) %>%
  mutate(per_day_income = gdp/population/365 , region = reorder(region,per_day_income,FUN = median))%>% 
  ggplot(aes(region,per_day_income, fill = factor(year))) + geom_boxplot()+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(trans = "log2") +
  ggtitle("Per day income (1970 vs 2011)")+ theme(legend.position = "top")

# From the plots  Among all the countries Eastern Asia is the one whose gdp chnaged significantly.

#west vs developing plot 

west <- c("Northern Europe","Northern America", "Australia and New Zealand", "Southern Europe","Western Europe")
gapminder %>% filter(year %in%c(1970, 2011) & !is.na(gdp)) %>% mutate(per_day_gdp = gdp/population/365, group = ifelse(region%in% west ,"West","Developing") ) %>%
  ggplot(aes(log2(per_day_gdp))) + geom_histogram(binwidth = 1, col = "black")+theme_bw() + facet_grid(year~group)


#from the plot it can be be seen the the difference between the GDPs and per day income of west vs developing countries is no longer exist.
# the grwoth of developing countries is more as compare to west.

#density plot
gapminder %>%filter(year %in% c(1970,2010) & !is.na(gdp & country %in% countries)) %>%
  mutate(dollars_per_day = gdp/population/365, group = ifelse(region %in% west, "West","Developing"))%>%
  ggplot(aes(x = dollars_per_day, y = ..count.., fill = group )) + geom_density(alpha = .45) +
  scale_x_continuous(trans = "log2") + theme_light() + ggtitle("west vs developing 1970 vs 2010")+ facet_grid(year~group)





