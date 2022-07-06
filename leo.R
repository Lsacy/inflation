library(altair)
library(reticulate)
#reticulate::py_config()
#altair::check_altair()
#altair::install_altair()
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(countrycode)
library(plotly)

library(broom)
library(purrr)

library(ggthemes)
library(tmap)
library(countrycode)

data("World")
load("worldbankindicators.RData")



World$iso2c=countrycode(sourcevar = World$iso_a3,
                        origin = "iso3c",
                        destination = "iso2c")

averaged_data <- worldbank_data%>%
  group_by(iso2c, country, Indicator)%>%
  summarise(Values=mean(Values,na.rm=T))



current_data <- worldbank_data %>% 
  filter(year==2000)

joined_data <- World%>%left_join(current_data, by='iso2c')

inflation <- filter(joined_data, Indicator=='Inflation')
unemployment <- filter(joined_data, Indicator =='Unemployment')

# life expectancy vs GDP per capita for every Country
joined_data %>% 
  filter(continent %in% c('Asia', 'Africa', 'Europe', 'South America', 'Oceania', 'North America')) %>% 
  ggplot(aes(x = (gdp_cap_est), 
             y = (life_exp),
             size= pop_est,
             )) +
  geom_point(alpha=0.1)+
  xlim(0,90000) +
  geom_smooth(se=F)+
  #facet_wrap(~continent)+
  labs(title='Life expectancy explained by GDP',
       x = 'GDP per capita',
       y = 'Life expectancy',
       colour = 'continent')

# life expectancy vs GDP per capita for each continent
joined_data %>% 
  filter(continent %in% c('Asia', 'Africa', 'Europe', 'South America', 'Oceania', 'North America')) %>% 
  ggplot(aes(x = (gdp_cap_est), 
             y = (life_exp),
             size= pop_est,
             color=continent
  )) +
  geom_point(alpha = 0.1)+
  xlim(0,65000) +
  geom_smooth(method=lm, se=F)+
  facet_wrap(~continent)+
  labs(title='Life expectancy and GDP per capita',
       x = 'GDP per capita',
       y = 'Life expectancy',
       colour = 'continent')


# Happiness vs GDP per capita for each continent
joined_data %>% 
  filter(continent %in% c('Asia', 'Africa', 'Europe', 'South America', 'Oceania', 'North America')) %>% 
  ggplot(aes(x = log(gdp_cap_est), 
             y = HPI,
             size= pop_est,
             color=continent
  )) +
  geom_point(alpha = 0.1)+
  facet_wrap(~continent)+
  labs(title='Happiness and GDP per capita',
       x = 'log (GDP per capita)',
       y = 'Happiness Index',
       colour = 'continent')


# Happiness vs Inequality for all countries
joined_data %>% 
  filter(continent %in% c('Asia', 'Africa', 'Europe', 'South America', 'Oceania', 'North America')) %>% 
  ggplot(aes(x = inequality, 
             y = HPI,
             size= pop_est,
             color=pop_est_dens
  )) +
  geom_point(alpha = 0.1)+
  geom_smooth(method=lm, se=F)+
  labs(title='Happiness and Inequality',
       x = 'Inequality',
       y = 'Happiness Index')

# Life Expectancy And Happiness
joined_data %>% 
  filter(continent %in% c('Asia', 'Africa', 'Europe', 'South America', 'Oceania', 'North America')) %>% 
  ggplot(aes(x = life_exp, 
             y = HPI,
             size= pop_est,
             color=pop_est_dens
  )) +
  geom_point(alpha = 0.1)+
  geom_smooth(method=lm, se=F)+
  labs(title='Happiness and Life Expectancy',
       x = 'Life Expectancy',
       y = 'Happiness Index')

# plotting inflation of year 2020
ggplot(data = inflation) +
  geom_sf(aes(fill=Values)) +
  colorspace::scale_fill_continuous_sequential(palette='viridis') +
  labs(caption= 'Inflation Map 2020')

# reshape long to wide 
columns <- c('iso_a3', 'country', 'Indicator', 'Values', 'continent')
filtered <- joined_data[columns] %>% 
  st_drop_geometry()

filtered %>% 
  mutate(Indicator = as.factor(Indicator))

wide_data <- filtered %>% 
  pivot_wider(names_from = Indicator, values_from = Values) 

# inflation vs interest rate
wide_data %>% 
  ggplot(aes(y = log(Inflation), 
             x = `Interest Rate`,
             size= GDP,
  )) +
  geom_point(alpha=0.3)+
  geom_smooth(method=lm, se=F)+
  #facet_wrap(~continent)+
  labs(title='Inflation and Interest rate',
       x = 'log(Inflation)',
       y = 'Interest Rate')

# inflation vs unemployment?
ger_usa <- worldbank_data %>% 
  filter(country == 'Germany' | country == 'United States' )

ger_usa_wide <- ger_usa %>% 
  pivot_wider(names_from = Indicator, values_from = Values)

ger_usa_wide %>% 
  ggplot(aes(x = (Unemployment), 
             y = (Inflation),
             color= country,
  )) +
  geom_point(alpha=0.3)+
  geom_smooth(method=lm, se=F)+
  #facet_wrap(~continent)+
  labs(title='Inflation and unemployment for US and Germany 2000-2020',
       x = 'Unemployment rate',
       y = 'Inflation')
