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
dataT3P1 <- subset(dataT3P1, select = -c(land_sqft,class))
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


reg1<-lm(log_Sale_Price ~d2001+d2002+d2003+d2004+d2005+d2006+d2007+
           d2008+d2009+d2010+d2011+d2012+d2013+d2014+d2015+
           d2016+d2017+d2018+d2019+d2020+year_built+ building_sqft + num_bedrooms+
           num_rooms+num_full_baths+num_half_baths+num_fireplaces+
           type_of_residence+construction_quality+ garage_area_included+garage_size+attic_type+
           basement_type+central_heating+roof_material+site_desirability+renovation+recent_renovation+porch+
           central_air,data=dataT3P1)

stargazer(reg1, type="text")

stargazer(
  reg1,
  dep.var.labels = c("Ln(income)"),
  out = "ındice de precios anual hedonico.tex"
)


resreg1<-broom::tidy(reg1, conf.int = TRUE)
resreg1 <- resreg1  %>% filter(grepl("d2",term))

resreg1$anos <- c(2001,2002,2003,2004,2005,2006,2007,
  2008,2009,2010,2011,2012,2013,2014,2015,
  2016,2017,2018,2019,2020)

hedoni <- ggplot(resreg1, aes(y = estimate, x = anos, group = 1)) +
  labs(x = "Años", y = "Log precio vivienda", title = "Índice de precios anual hedonico") +
  geom_point(aes(color = factor(anos)), size = 2) +
  geom_text(aes(label = round(estimate, digits = 2) ), vjust = -3)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high),fill = "skyblue")+
  geom_line(aes(y = estimate), color = "gray", linetype = "solid") +
  theme_minimal()
ggsave("Índice_hedonico.png", hedoni)

# modelo de MCO con errores robustos
install.packages("lmtest")
library(lmtest)

modelo_2 <- coeftest(reg1)
modelo_2


## Punto 2
dataT3P2 <- subset(dataT3P1, select = -c(d2000,d2001,d2002,d2003,d2004,
                                         d2005,d2006,d2007,d2008,d2009,
                                         d2010,d2011,d2012,d2013,d2014,
                                         d2015,d2016,d2017,d2018,d2019,
                                         d2020))
dataT3P2 <- dataT3P2 %>% group_by(pin,years)
Repetida <- dataT3P2 %>% group_by(pin) %>% summarise(n = n())
Repetida <- filter(Repetida,n>1)

df1 <- dataT3P2 %>%
  inner_join(Repetida, by="pin")

df1 <- df1 %>%
  group_by(pin) %>%
  mutate(CumulativeCount = order(years)) %>%
  ungroup()

df1$new_column <- ifelse(df1$CumulativeCount == 1, -1,
                            ifelse(df1$CumulativeCount == 2,1,0))

#Creo la variable dicotoma para d2000, donde 
datos <- df1 %>%
  filter(year == 2000) %>%  # Filtrar solo las observaciones para el año 2000
  group_by(pin) %>%
  mutate(d2000 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2000")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2000 = ifelse(is.na(d2000), 0, d2000))

  
#Creo la variable dicotoma para d2001, donde 
datos <- df1 %>%
  filter(year == 2001) %>%  # Filtrar solo las observaciones para el año 2000
  group_by(pin) %>%
  mutate(d2001 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2001")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2001 = ifelse(is.na(d2001), 0, d2001))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2002) %>%  # Filtrar solo las observaciones para el año 2002
  group_by(pin) %>%
  mutate(d2002 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2002")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2002 = ifelse(is.na(d2002), 0, d2002))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2003) %>%  # Filtrar solo las observaciones para el año 2003
  group_by(pin) %>%
  mutate(d2003 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2003")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2003 = ifelse(is.na(d2003), 0, d2003))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2004) %>%  # Filtrar solo las observaciones para el año 2004
  group_by(pin) %>%
  mutate(d2004 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2004" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2004")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2004 = ifelse(is.na(d2004), 0, d2004))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2005) %>%  # Filtrar solo las observaciones para el año 2005
  group_by(pin) %>%
  mutate(d2005 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2005")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2005 = ifelse(is.na(d2005), 0, d2005))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2006) %>%  # Filtrar solo las observaciones para el año 2006
  group_by(pin) %>%
  mutate(d2006 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2006")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2006 = ifelse(is.na(d2006), 0, d2006))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2007) %>%  # Filtrar solo las observaciones para el año 2007
  group_by(pin) %>%
  mutate(d2007 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2007")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2007 = ifelse(is.na(d2007), 0, d2007))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2008) %>%  # Filtrar solo las observaciones para el año 2008
  group_by(pin) %>%
  mutate(d2008 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2008")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2008 = ifelse(is.na(d2008), 0, d2008))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2009) %>%  # Filtrar solo las observaciones para el año 2009
  group_by(pin) %>%
  mutate(d2009 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount== 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2009")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2009 = ifelse(is.na(d2009), 0, d2009))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2010) %>%  # Filtrar solo las observaciones para el año 2010
  group_by(pin) %>%
  mutate(d2010 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2010")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2010 = ifelse(is.na(d2010), 0, d2010))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2011) %>%  # Filtrar solo las observaciones para el año 2011
  group_by(pin) %>%
  mutate(d2011 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2011")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2011 = ifelse(is.na(d2011), 0, d2011))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2012) %>%  # Filtrar solo las observaciones para el año 2012
  group_by(pin) %>%
  mutate(d2012 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2012")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2012 = ifelse(is.na(d2012), 0, d2012))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2013) %>%  # Filtrar solo las observaciones para el año 2013
  group_by(pin) %>%
  mutate(d2013 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2013")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2013 = ifelse(is.na(d2013), 0, d2013))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2014) %>%  # Filtrar solo las observaciones para el año 2014
  group_by(pin) %>%
  mutate(d2014 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2014")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2014 = ifelse(is.na(d2014), 0, d2014))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2015) %>%  # Filtrar solo las observaciones para el año 2015
  group_by(pin) %>%
  mutate(d2015 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2015")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2015 = ifelse(is.na(d2015), 0, d2015))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2016) %>%  # Filtrar solo las observaciones para el año 2016
  group_by(pin) %>%
  mutate(d2016 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2016")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2016 = ifelse(is.na(d2016), 0, d2016))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2017) %>%  # Filtrar solo las observaciones para el año 2017
  group_by(pin) %>%
  mutate(d2017 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2017")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2017 = ifelse(is.na(d2017), 0, d2017))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2018) %>%  # Filtrar solo las observaciones para el año 2018
  group_by(pin) %>%
  mutate(d2018 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2018")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2018 = ifelse(is.na(d2018), 0, d2018))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2019) %>%  # Filtrar solo las observaciones para el año 2019
  group_by(pin) %>%
  mutate(d2019 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2019")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2019 = ifelse(is.na(d2019), 0, d2019))

#Creo la variable dicotoma para d2002, donde 
datos <- df1 %>%
  filter(year == 2020) %>%  # Filtrar solo las observaciones para el año 2020
  group_by(pin) %>%
  mutate(d2020 = ifelse(CumulativeCount == 1, -1, ifelse(CumulativeCount == 2, 1, 0))) %>%
  ungroup()

# Agregar la columna "d2000" al DataFrame original "data"
df1 <- merge(df1, datos[, c("pin", "year", "d2020")], by = c("pin", "year"), all.x = TRUE)

#En la base nueva los valores que son NA los reeplazamos por cero
df1 <- df1 %>%
  mutate(d2020 = ifelse(is.na(d2020), 0, d2020))


regB<-lm(log_Sale_Price~d2001+d2002+d2003+d2004+d2005+d2006+d2007+
           d2008+d2009+d2010+d2011+d2012+d2013+d2014+d2015+
           d2016+d2017+d2018+d2019+d2020 ,data=df1)

#df1$Estimated <- predict(regB)

df1$residuos <- resid(regB)

#df1 <- df1 %>%
#  group_by(pin) %>%
#  mutate(diff_estimated = Estimated - lag(Estimated, n=1))

stargazer(regB, type="text")

#  df1 <- df1 %>%
#  group_by(pin) %>%
#  mutate(diff_value = log_Sale_Price - lag(log_Sale_Price, n=1))

  
  df1 <- df1 %>%
    group_by(pin) %>%
    mutate(diff_year = year - lag(year, n=1))
  
  df1 <- df1 %>%
    group_by(pin) %>%
    mutate(diff_year2 = (diff_year)^2)
  
#  df1 <- df1 %>%
#    group_by(pin) %>%
#    mutate(error1=(diff_value-diff_estimated)^2)

  
  regError<-lm(residuos~ diff_year+diff_year2,data=df1)  

  stargazer(regError, type="text")

  predicciones <- predict(regError, newdata = df1)
  df1$Predicciones <- (predicciones)^(1/2)
  

## Etapa 3
  
  wls_model <- lm(log_Sale_Price~d2001+d2002+d2003+d2004+d2005+d2006+d2007+
                    d2008+d2009+d2010+d2011+d2012+d2013+d2014+d2015+
                    d2016+d2017+d2018+d2019+d2020,data=df1, weights = df1$Predicciones)

  stargazer(wls_model, type="text")
  
  stargazer(
    wls_model,
    dep.var.labels = c("Ln(income)"),
    out = "IPVU.tex"
  )
  
  
  Beta <- predict(wls_model, newdata = df1)
  df1$Beta <- ((Beta))

  
  resregfinal<-broom::tidy(wls_model, conf.int = TRUE)
  resregfinal <- resregfinal  %>% filter(grepl("d2",term))
  resregfinal$anos <- c(2001,2002,2003,2004,2005,2006,2007,
                    2008,2009,2010,2011,2012,2013,2014,2015,
                    2016,2017,2018,2019,2020)

  IPVU <- ggplot(resregfinal, aes(y = estimate, x = anos, group = 1)) +
    labs(x = "Años", y = "Log precio vivienda", title = "IPVU") +
    geom_point(aes(color = factor(anos)), size = 2) +
    geom_text(aes(label = round(estimate, digits = 2) ), vjust = -3)+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    #geom_ribbon(aes(ymin = conf.low, ymax = conf.high),fill = "skyblue")+
    geom_line(aes(y = estimate), color = "gray", linetype = "solid") +
    theme_minimal()
  ggsave("IPV.png", IPVU)
 
  
## ¨Punto 3

reg4<-feols(log_Sale_Price ~d2001+d2002+d2003+d2004+d2005+d2006+d2007+
              d2008+d2009+d2010+d2011+d2012+d2013+d2014+d2015+
              d2016+d2017+d2018+d2019+d2020+year_built+ building_sqft + num_bedrooms+
              num_rooms+num_full_baths+num_half_baths+num_fireplaces+
              type_of_residence+construction_quality+ garage_area_included+garage_size+attic_type+
              basement_type+central_heating+roof_material+site_desirability+renovation+recent_renovation+porch+
              central_air + factor(township_code),vcov = ~pin, data=dataT3P1)

etable(list(reg4), tex=FALSE, file ='fijos.latex')

resreg4<-broom::tidy(reg4, conf.int = TRUE)
resreg4 <- resreg4  %>% filter(grepl("d2",term))
resreg4$anos <- c(2001,2002,2003,2004,2005,2006,2007,
                      2008,2009,2010,2011,2012,2013,2014,2015,
                      2016,2017,2018,2019,2020)

fijos <- ggplot(resreg4, aes(y = estimate, x = anos, group = 1)) +
  labs(x = "Años", y = "Log precio vivienda", title = "Indice precio vivienda con efectos fijos") +
  geom_point(aes(color = factor(anos)), size = 2) +
  geom_text(aes(label = round(estimate, digits = 2) ), vjust = -3)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high),fill = "skyblue")+
  geom_line(aes(y = estimate), color = "gray", linetype = "solid") +
  theme_minimal()
ggsave("fijos.png", fijos)


