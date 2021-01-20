#setup
library(tidyverse)
library(ggplot2)
library(readxl)
library(flexdashboard)
library(tmap)
library(countrycode)
data("World")

#read data
s<-read.table(file = "suiciderate.csv", sep = ",")
gdp<-read.table(file = "gdppercap.csv", sep = ",")
workers<- read_csv("MH_6,MH_7,MH_8,MH_9.csv")
policy<- read_csv("MH_25,MH_26,MH_27,MH_3,MH_5.csv")
beds<- read_csv("MH_13,MH_15,MH_16.csv")
healthcare <- read_csv("MH_10,MH_11,MH_14,MH_17,MH_18.csv")

#tidy data
s1<- s[-c(1,2),]
s2<-filter(select(s1, V1, V2, V3), V2==" Both sexes")
s3<-transform(select(s2, V1, V3), V3=as.numeric(V3))
s2016<-arrange(s3, desc(V3))
s4<-rename(s2016, sovereignt=V1, suicide_rate_per_100000_population=V3)
s5<-cbind(
  iso_a3=apply(s4[1],1,countrycode, origin = 'country.name', destination = 'iso3c'),
  s4[2]
)

p2_1<-inner_join(World, s5, by="iso_a3")

gdp2016<-cbind(
  V1=apply(gdp[1],1,countrycode, origin = 'country.name', destination = 'iso3c'),
  gdp[61]
)

s6<-cbind(
  V1=apply(s3[1],1,countrycode, origin = 'country.name', destination = 'iso3c'),
  s3[2]
)

s_gdp<-inner_join(s6, gdp2016, by="V1")
s_gdp2016<-s_gdp %>% 
  rename(
    country=V1,
    suicide_rate_per_100000_population=V3,
    gdp_per_capita=V61
  )

gdp2016_1<-rename(gdp2016,
  iso_a3=V1,
  gdp_per_capita=V61
)


mental_health<-inner_join(healthcare, workers, by="Country")

mh<-transmute(mental_health, Country=Country, 
                         mental_health_units = mental_health[[3]]+mental_health[[4]],
                         number_of_psychiatrists = mental_health[[9]],
              )
mh1<-cbind(
    iso_a3=apply(mh[1],1,countrycode, origin = 'country.name', destination = 'iso3c'),
    mh[2],
    mh[3]
)

mh2<-inner_join(mh1, s5, by="iso_a3")

mh3<-inner_join(mh1, gdp2016_1, by="iso_a3")

mh4<-left_join(World, mh3, by="iso_a3")

mh5<-mutate(mh4, missing_data=is.na(mental_health_units))

#graphs
p1<-ggplot(data = s_gdp2016) + 
    geom_point(mapping = aes(x = suicide_rate_per_100000_population, y = gdp_per_capita))
p1+scale_x_continuous(
  breaks = c(5, 10, 15, 20, 25, 30),
  label = c("5", "10", "15", "20","25","30")
  )

p2<-ggplot(data = mh2) + 
  geom_point(mapping = aes(x = suicide_rate_per_100000_population, y = mental_health_units))
p2+scale_x_continuous(
  breaks = c(5, 10, 15, 20, 25, 30),
  label = c("5", "10", "15", "20","25","30")
)

p3<-ggplot(data = mh2) + 
  geom_point(mapping = aes(x = suicide_rate_per_100000_population, y = number_of_psychiatrists))
p3+scale_x_continuous(
  breaks = c(5, 10, 15, 20, 25, 30),
  label = c("5", "10", "15", "20","25","30")
)

p4<-ggplot(data = mh3) + 
  geom_point(mapping = aes(x = gdp_per_capita, y = number_of_psychiatrists))
p4+scale_x_continuous(
  breaks = c(5, 10, 15, 20, 25, 30),
  label = c("5", "10", "15", "20","25","30")
)

p5<-ggplot(data = mh3) + 
  geom_point(mapping = aes(x = gdp_per_capita, y = mental_health_units))
p5+scale_x_continuous(
  breaks = c(5, 10, 15, 20, 25, 30),
  label = c("5", "10", "15", "20","25","30")
)

tm_shape(p2_1) +
  tm_polygons("suicide_rate_per_100000_population")

tm_shape(mh5) +
  tm_polygons("missing_data")





