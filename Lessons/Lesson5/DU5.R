dt_Policy <- read.csv("lesson5_PolicyHistory.csv")
dt_Claims <- read.csv("lesson5_Claims.csv")
dt_pol_w_claims <- left_join(dt_Policy, 
                             dt_Claims, 
                             by = c("NrPolicy", "NrObject"))
head(dt_pol_w_claims)
summary(dt_pol_w_claims)

library(lubridate)
dt_pol_w_claims <- 
dt_pol_w_claims %>% mutate(Time_Exposure = lubridate::dmy(Dt_Exp_End) - lubridate::dmy(Dt_Exp_Start),
                           Ult_Loss = Paid + Reserves,
                           Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure)))

#budeme sa zaoberat premennou Construct_year(rokom vyroby auta)
library(ggplot2)
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_jitter()
summary(dt_pol_w_claims$Construct_year)
#vidime ze priem. rok vyroby auta je 2010 a najdu sa aj nejake outliery vo forme veteranov z roku 1977
#vacsina aut, ktora je poistenych, je z roku okolo 2011 a celkovo mame menej dat pre rok 1990 a starsie

dt_pol_w_claims %>% 
  filter(Burning_Cost != 0) %>% 
  group_by(Construct_year) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
#najvyssi priemerny BC je pre auta z roku 1997, ale to nie je take signifikantne, lebo mame len 2 data z toho toku
#signifikantnejsie navyssi BC je preto pre data 136 aut z roku 2014 
#predikcny model na zaklade roku vyroby sa moze "pokazit" na zaklade outlierov, napr co sa tyka aut
#s BC nad 250 alebo aj to ze pre starsie auta mame malo dat

#spravime model pre BC < 100 (podla vzoru na hodine)
model1 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ Construct_year,
              family = Gamma())
summary(model1)
#vidime ze parameter Construct_year pri predikcii je signifikantny, teda rok vyroby auta je dolezity parameter pri odhadovani cien poistneho


#teraz sa zameriame na premennu Capacity, ktora pravdepodobne vyjadruje objem motora
dt_pol_w_claims %>%
  ggplot(aes(y = Burning_Cost, x = Capacity)) + 
  geom_jitter()
summary(dt_pol_w_claims$Capacity)
#vidime, ze priemerny objem je 2183 a najdu sa aj outliery s objemom nad 18000(asi ich predstavuju nejake vykonnejsie kamiony a pod.)

dt_pol_w_claims %>% 
  filter(Burning_Cost != 0) %>% 
  group_by(Capacity) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))

model2 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ Capacity,
              family = Gamma())
summary(model2)
#na zaklade tohto modelu parameter Capacity nevysiel taky signifikantny ako pri predoslom modeli param. Construct_year,
#ale moze to byt sposobene strukturou dat, lebo rozne objemy mame zastupene malym poctom dat alebo mozno keby zanedbame velke kamiony... 


