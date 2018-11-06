library(plotly)
library(tidyverse)
library(lubridate)
#devtools::install_github("ropensci/weathercan") 
library(weathercan)

hydro <- read_tsv("0311972681_heure_2018-10-17_au_2018-11-04.csv", locale = locale(decimal_mark = ","))
colnames(hydro) <- c("contrat", "datetime", "kWh", "code_kWh", "temperature", "code_temperature")
hydro <- hydro %>% mutate(date = lubridate::date(datetime),
                 hour = lubridate::hour(datetime),
                 heating_degree_hour = 18- temperature)


### COP
specs <- data.frame( outdoor_temp_F = c(60,47,40,30,17,10), COP1000 = c(4.22, 3.81, 3.53, 3.14, 2.61, 2.42), MBH1000 = c(36.5, 31.2, 28.7, 25.1, 20.3, 18.7)) %>%
  mutate(outdoor_temp_C = (outdoor_temp_F -32)*5 / 9)

début <- "2018-11-03"
ggplot(hydro %>% filter(date>=  lubridate::ymd(pouet) ))+
  geom_point(aes(x= hour, y= kWh))+
  geom_smooth(aes(x= hour, y= kWh))

ggplot(hydro %>% filter(date==  lubridate::ymd(pouet) ))+
  geom_point(aes(x= temperature, y= kWh))+
  geom_smooth(aes(x=temperature, y= kWh))


hydro %>% filter(date==  lubridate::ymd(pouet) ) %>%
  summarise_at(vars(heating_degree_hour, kWh),
               sum) %>%
  mutate(kWh_per_hdh = kWh / heating_degree_hour)
  
my_station <- stations_search(coords = c(46.73, -71.27), dist = 20, interval = "hour") %>%
  filter(end ==2018)%>%  # toujours en activité
  head(1) %>%  # la plus proche car les stations sont triées par distance
  mutate(station_id = as.numeric(as.character(station_id))) %>%  # factor to integer
  pull(station_id) # pull my station id

my_weather <- weather_dl(
  station_ids = my_station,
  start= "2010-01-01",
  end = "2017-12-31")

avg_weather  <- my_weather %>%
  mutate(yday = yday(date)) %>%  # day of the year (for averaging purposes)
  group_by(yday, hour) %>% 
  summarise( temp = mean(temp,na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(yday) %>% 
  mutate(hdd = pmax( 18 - mean(temp, na.rm = TRUE),0)) %>%  # heating degree days
  ungroup() %>%
  mutate (
    hdh = case_when(
      hdd==0 ~ 0,
      TRUE ~ pmax( 18- temp,0))) # heating degree hours, only if heating degree days >0

plot <- avg_weather %>% ggplot(  aes(x=yday, y = hdh)) +
  geom_line() 
  
ggplotly(plot)
  


