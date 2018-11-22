# Introduction
# A air-air heat pump is an amazing device.  It efficiently transfers the heat from the (cold) air outside to the warm air inside my house.  
# The efficiency of the heat pump is measured by it s coefficient of performance: how many kWh of heat will be added to my house when 1 kWh is used by the machine?
#   
# Devices such as the York YHE24B21S

#  load libraries ----

library(plotly)
library(tidyverse)
library(lubridate)
#devtools::install_github("ropensci/weathercan") 
library(weathercan)
library(modelr)

# Define generic functions ----
wrapper <- function(x, ...)
{
  paste(strwrap(x, ...), collapse = "\n")
}

# Import electricity consumption data ----

# hydro <- read_csv2("0311972681_heure_2018-10-17_au_2018-11-20.csv", locale = locale(decimal_mark = ","))
# colnames(hydro) <- c("contrat", "datetime", "kWh", "code_kWh", "temperature", "code_temperature")
# hydro <- hydro %>% mutate(date = lubridate::date(datetime),
#                  hour = lubridate::hour(datetime)) %>% select(-contrat)
# write_csv(hydro, "hydro.csv")

hydro <- read_csv("hydro.csv")

# Model kWh consumption as a function of external temperature ----

# I have held my  indoor temperature constant at 21 degree Celcius  from November 3rd to November 21st 2018  
# We can argue that 100% of my electricity consumption goes to heating.  For example, all the electricity used by my fridge, computer and light is 
# eventually 100% converted into heat that my electric furnace won't need to generate.
# The only electricity that is "wasted" is hot water, since that hot water  is just flushed down the drain and doesnt warm up my house.  
# The biggest use of hot water is the morning (adult shower) and evening (children bath).

start <- "2018-11-03"


# it appears that we can model the kWh as a linear function of temperature ( awesome!)
ggplot(hydro %>% filter(date>=  lubridate::ymd(start) ))+
  geom_point(aes(x= temperature, y= kWh))+
  geom_smooth(aes(x=temperature, y= kWh))+ 
  labs(title = wrapper("Electricty consumption is linearly related to exterior temperature",50))


mod_lm <- lm(formula = kWh ~ temperature, data = hydro %>% filter(date>=  lubridate::ymd(start) ) )
mod_lm

#predicted_kWh <- tibble(temperature = -30:30) %>% add_predictions(mod_lm) %>% rename(kWh = pred) %>% mutate(MBH = kWh * kW_to_MBH)

## ok, now we have to figure out how much time I spend at each temperature during the heating season.





# Import weather data ----
# my_station_name <- stations_search(coords = c(46.73, -71.27), dist = 20, interval = "hour") %>%
#   filter(end ==2018)%>%  #still active
#   head(1) %>% pull (station_name)
# 
# my_station_id <- stations_search(coords = c(46.73, -71.27), dist = 20, interval = "hour") %>%
#   filter(end ==2018)%>%  #still active
#   head(1) %>%  # first station is the closest as they are sorted by distance
#   mutate(station_id = as.numeric(as.character(station_id))) %>%  # factor to integer
#   pull(station_id) # pull my station id
# 
# my_weather_data <- weather_dl(
#   station_ids = my_station_id,
#   start= "2010-01-01",
#   end = "2017-12-31")
# 
# write_csv(my_weather_data, "my_weather_data.csv")

my_weather_data <- read_csv( "my_weather_data.csv")

avg_weather  <- my_weather_data %>%
  mutate(yday = yday(date)) %>%  # day of the year (for averaging purposes)
  group_by(yday, hour) %>% 
  summarise( temperature = mean(temp,na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(yday) %>% 
  mutate(day_temp  = mean(temperature, na.rm = TRUE)) %>%  
  ungroup()  

ggplot(data= avg_weather, aes(x= temperature))+ geom_histogram() +
  labs(title = wrapper("Distribution of average hourly temperature in Quebec City, 2010-2017",50))


plot <- avg_weather %>% ggplot(  aes(x=yday, y = day_temp)) +
  geom_line() 

ggplotly(plot)

# Import Heat Pump technical sheet ---- 
# http://www.upgnet.com/PdfFileRedirect/5005747-YTG-A-0216.PDF#page=41

## let's move to the heat pump specifications.

# 1 kw is equal to 3.4121416331 MBH
#https://www.unitconverters.net/power/mbh-to-kilowatt.htm
kW_to_MBH = 3.4121416331 

heat_pump_specs <- tibble(
  outdoor_temp_F = c(60,47,40,30,17,10),
  MBH_800CFM = c(35.8, 30.7, 28.2, 24.8, 20.1, 18.4),
  COP_800CFM = c(4.13, 3.67, 3.43, 3.09, 2.58, 2.39))  %>%
  mutate(temperature = (outdoor_temp_F -32)*5/9) %>%
  arrange(temperature) %>%
  mutate(last_temperature = lag(temperature), 
         last_MBH = lag(MBH_800CFM),
         last_COP = lag(COP_800CFM),
         slope_MBH = (MBH_800CFM - last_MBH) / (temperature - last_temperature),
         slope_COP = (COP_800CFM - last_COP) / (temperature - last_temperature))

get_COP <- function(temperature){ #linear interpolation of technical sheets.  # COP is 1 below minimum temperature as the heat pump is replaced by the electric furnace
  case_when(
    temperature < -12.2 ~ 1,
    temperature < -8.33 ~ 2.39 + 0.0489 * (temperature -  -12.2),
    temperature < -1.11 ~ 2.58 + 0.0706 * (temperature -  -8.33),
    temperature <  4.44 ~ 3.09 + 0.0612 * (temperature -  -1.11),
    temperature <  8.33 ~ 3.43 + 0.0617 * (temperature -   4.44),
    temperature < 15.60 ~ 3.67 + 0.0637 * (temperature -   8.33),
    TRUE ~ 4.13
  )
}

get_MBH <- function(temperature){ #linear interpolation of technical sheets.  # COP is 1 below minimum temperature as the heat pump is replaced by the electric furnace
  case_when(
    temperature < -12.2 ~ 0,
    temperature < -8.33 ~ 18.4 + 0.437 * (temperature -  -12.2),
    temperature < -1.11 ~ 20.1 + 0.651 * (temperature -  -8.33),
    temperature <  4.44 ~ 24.8 + 0.612 * (temperature -  -1.11),
    temperature <  8.33 ~ 28.2 + 0.643 * (temperature -   4.44),
    temperature < 15.60 ~ 30.7 + 0.706 * (temperature -   8.33),
    TRUE ~ 35.8
  )
}



# Define heating season ----

# the normal measure yearly heating needs is "heating degree days",
# which is the difference between the daily average temperature and 18C.
# however, I know that I dont heat after May 15th or before September 15th,
# (day 135 and 258 of the year), so I will assume that this means that I dont heat 
# when the outside temperature is above 13C.  

kWh_price <- 0.07
heating_season <- avg_weather %>% filter(day_temp < 13)  %>% 
  add_predictions(mod_lm) %>% 
  rename(kWh_required= pred)  

annuel_heating_cost <- heating_season %>% summarise(dollar_cost = sum(kWh_required)* kWh_price) 

ggplot(data= heating_season, aes(x= temperature))+ geom_histogram() +
  labs(title = wrapper("Distribution of average hourly temperature in Quebec City *during heating season*, 2010-2017",50))


# Define system performance for a given temperature ----
predicted_kWh <- 
  tibble(temperature = -30:30) %>% 
  add_predictions(mod_lm) %>% 
  rename(kWh_required = pred) %>%
  mutate(
         MBH_required = kWh_required * kW_to_MBH,
         heat_pump_MBH = get_MBH(temperature),
         heat_pump_COP = get_COP(temperature),
         real_COP =   ifelse(heat_pump_MBH > MBH_required, heat_pump_COP, 1),
         real_kWh = kWh_required / real_COP)


ggplot_data <- predicted_kWh %>% gather(key=key, value=value, kWh_required, MBH_required ,heat_pump_MBH, heat_pump_COP, real_COP,real_kWh )  

ggplot_data %>% 
  filter(temperature <= 13) %>% 
  filter( key %in% c("MBH_required", "heat_pump_MBH")) %>%
  ggplot(aes(x=temperature, y = value, color= key), alpha = 0.5) + geom_line()+
  labs(title=wrapper( "Heat pump can only generate enough heat until to heat the house until -9C, even though it can generate some heat until -12C",50),
       subtitle= wrapper("Increasing energy efficiency by 13% would allow us to use the heatpump until -12C",50))

ggplot_data %>% 
  filter(temperature <= 13) %>% 
  filter( key %in% c("kWh_required", "real_kWh")) %>%
  ggplot(aes(x=temperature, y = value, color= key), alpha = 0.5) + geom_line() +
  labs(title= wrapper("Electricity consumption explodes when the system switches from heat pump to electric furnace",50))

ggplot_data %>% 
  filter(temperature <= 13) %>% 
  filter( key %in% c("heat_pump_COP", "real_COP")) %>%
  ggplot(aes(x=temperature, y = value, color= key), alpha = 0.2) + geom_line() +
  labs(title = wrapper("The real COP is lower than the heat pump's between -12C and -9C because the heat pump can't generate enough heat"))


# next step:  revisit when the heat pump is installed with a piecewise regression


# Predict electricity needs and savings based on 2017 season. ----
z <-heating_season %>% 
  mutate(MBH_required = kWh_required * kW_to_MBH,
         heat_pump_MBH = get_MBH(temperature),
         heat_pump_COP = get_COP(temperature),
         real_COP =   ifelse(heat_pump_MBH > MBH_required, heat_pump_COP, 1),
         real_kWh = kWh_required / real_COP)
# saving percentage
z %>% summarise(real_COP = weighted.mean(x=real_COP, w= real_kWh), kWh_required = sum(kWh_required), real_kWh = sum(real_kWh), savings_pct = (kWh_required - real_kWh)/ kWh_required, )
