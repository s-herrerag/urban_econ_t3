# Taller 3 punto 1

#packages
require("pacman")
p_load("tidyverse", #data wrangling
       "modeldata", # package with the housing data from Ames, Iowa
       "stargazer", # gazing at the stars 
       "broom", #tidy data set
       "fixest" #fix effect estimation
) 




## Directorio de trabajo
setwd("C:/Users/ADMIN/Documents/RAFAEL/3. Uniandes/12. Urban Economics/Taller 3")
## Cargue base de datos
dataT3P1 <- readRDS("db_ejercicio1.Rds") 
head(dataT3P1)

## Missing data
porcentajeMiss <- function(x) {sum(is.na(x)) / length(x)*100}
apply(dataT3P1, 2, porcentajeMiss)
dataT3P1 <- subset(dataT3P1, select = -c(land_sqft,township_code,class))
dataT3P1 <- na.omit(dataT3P1)

## Transformacion variable año
table(dataT3P1$year)
class(dataT3P1$year)

dataT3P1<- dataT3P1  %>% mutate(years=factor(year,levels=c(2000,2001,2002,2003,2004,2005,2006,2007,
                                                          2008,2009,2010,2011,2012,2013,2014,2015,
                                                          2016,2017,2018,2019,2020),
                                    labels=c("d2000","d2001","d2002","d2003","d2004",
                                             "d2005","d2006","d2007","d2008","d2009",
                                             "d2010","d2011","d2012","d2013","d2014",
                                             "d2015","d2016","d2017","d2018","d2019",
                                             "d2020")))
class(dataT3P1$years)

## Logaritmo precio de venta
dataT3P1<- dataT3P1  %>% mutate(log_Sale_Price=log(sale_price))
dataT3P1 <- subset(dataT3P1, select = -c(sale_price))

## Dummies para cada año
dataT3P1<- dataT3P1  %>% mutate(d2000=ifelse(year==2000,1,0),
                        d2001=ifelse(year==2001,1,0),
                        d2002=ifelse(year==2002,1,0),
                        d2003=ifelse(year==2003,1,0),
                        d2004=ifelse(year==2004,1,0),
                        d2005=ifelse(year==2005,1,0),
                        d2006=ifelse(year==2006,1,0),
                        d2007=ifelse(year==2007,1,0),
                        d2008=ifelse(year==2008,1,0),
                        d2009=ifelse(year==2009,1,0),
                        d2010=ifelse(year==2010,1,0),
                        d2011=ifelse(year==2011,1,0),
                        d2012=ifelse(year==2012,1,0),
                        d2013=ifelse(year==2013,1,0),
                        d2014=ifelse(year==2014,1,0),
                        d2015=ifelse(year==2015,1,0),
                        d2016=ifelse(year==2016,1,0),
                        d2017=ifelse(year==2017,1,0),
                        d2018=ifelse(year==2018,1,0),
                        d2019=ifelse(year==2019,1,0),
                        d2020=ifelse(year==2020,1,0)
)
dataT3P1 <- subset(dataT3P1, select = -c(year))

reg1<-lm(log_Sale_Price ~d2001+d2002+d2003+d2004+d2005+d2006+d2007+
           d2008+d2009+d2010+d2011+d2012+d2013+d2014+d2015+
           d2016+d2017+d2018+d2019+d2020+year_built+ building_sqft + num_bedrooms+
           num_rooms+num_full_baths+num_half_baths+num_fireplaces+
           type_of_residence+construction_quality+attic_finish+garage_attached+
           garage_area_included+garage_size+garage_ext_wall_material+attic_type+
           basement_type+ext_wall_material+central_heating+basement_finish+
           roof_material+site_desirability+renovation+recent_renovation+porch+
           central_air,data=dataT3P1)

stargazer(reg1, type="text")

resreg1<-broom::tidy(reg1, conf.int = TRUE)
resreg1 <- resreg1  %>% filter(grepl("d2",term))
resreg1

ggplot(resreg1, aes(y = estimate, x = term, group = 1)) +
  labs(x = "Años", y = "Log precio vivienda", title = "Indice de precios anual") +
  geom_point() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),fill = "lightpink"
  ) + geom_line(aes(y = estimate))

## Punto 2

dataT3P2 <- dataT3P1 %>% group_by(pin,years)
Repetida <- dataT3P2 %>% group_by(pin) %>% summarise(n = n())
Repetida <- filter(Repetida,n>1)

dataT3P2 <- dataT3P2 %>%
  group_by(pin) %>%
  mutate(n = n()) %>%
  select(pin, n)

Repetida <- filter(dataT3P2,n>1)

df1 <- filter(dataT3P2, dataT3P2 %in% Repetida)
