library(dplyr)
dt_pol_w_claims <- readRDS("lesson6_dt_pol_w_claims.rds")
set.seed(58742) # to fix randomizer
ind <- sample(2, nrow(dt_pol_w_claims), replace=TRUE, prob=c(0.80, 0.20))
dt_pol_w_claims <- mutate(dt_pol_w_claims,
                          data_status = ifelse(ind == 1, 
                                               "Training",
                                               ifelse(ind == 2, 
                                                      "Validation", 
                                                      "Unseen")
                          )
)

#rozdelenie dat na trenovacie a validacne
train <- dt_pol_w_claims %>% filter(data_status == "Training")
val <- dt_pol_w_claims %>% filter(data_status == "Validation")

mse <- function(prediction, actual){
  return(sum((prediction-actual)^2, na.rm = TRUE)/length(prediction))
}

#budeme sa zaoberat premennou Construct_year(rokom vyroby auta)

#spravime model (podla predoslej du)
model1 <- glm(data = train,
              formula = Burning_Cost ~ Construct_year,
              family = Gamma())
summary(model1)
#vidime ze parameter Construct_year pri predikcii nie je signifikantny, teda rok vyroby auta nie je dolezity parameter pri odhadovani cien poistneho
mse(predict(model1, train, type = "response"), train$Burning_Cost) #201.4312
mse(predict(model1, val, type = "response"), val$Burning_Cost) #285.3343


#teraz sa zameriame na premennu Capacity, ktora vyjadruje objem motora
model2 <- glm(data = train,
              formula = Burning_Cost ~ Capacity,
              family = Gamma())
summary(model2)
#na zaklade tohto modelu parameter Capacity vysiel signifikantnejsi ako pri predoslom modeli param. Construct_year,
#teda objem motora ma vyznamny vplyv na nehodovost, a teda vyplacanie poistneho, 
#napr. su nieco vacsie objemy ako bezne auta -> predvadzanie, riskantnejsia jazda mozno, vacsia pp nehody
mse(predict(model2, train, type = "response"), train$Burning_Cost) #202.4066
mse(predict(model2, val, type = "response"), val$Burning_Cost) # 284.3031


#skusime skombinovat obe premenne
model23 <- glm(data = train,
              formula = Burning_Cost ~ Capacity + Construct_year,
              family = Gamma())
summary(model23)
mse(predict(model23, train, type = "response"), train$Burning_Cost) #199.8263
mse(predict(model23, val, type = "response"), val$Burning_Cost) #285.6043
#vidime, ze cislo mse sa znizilo na 199.8263, cim sa model o nieco zlepsil


#pridame novy parameter D_age, lebo vek vodica moze vyrazne ovplyvnit nehodovost
model3 <- glm(data = train,
               formula = Burning_Cost ~ Capacity + Construct_year + D_age ,
               family = Gamma())
summary(model3)
mse(predict(model3, train, type = "response"), train$Burning_Cost) #197.4693
mse(predict(model3, val, type = "response"), val$Burning_Cost) #284.0754
#vidime, ze sa nam na zaklade mse model o trochu zlepsil pridanim parametra o veku vodica, teda potvrdilo sa nase tusenie o zlepseni modelu


#vyhodime parameter Construct_year, lebo pri modeli modelujucom len na tomto parametri vysiel tento ako nesignifikantny a pridame novy parameter Veh_type2
model4 <- glm(data = train,
              formula = Burning_Cost ~ Capacity + D_age + Veh_type2,
              family = Gamma())
summary(model4)
mse(predict(model4, train, type = "response"), train$Burning_Cost) #194.654
mse(predict(model4, val, type = "response"), val$Burning_Cost) #284.5059

#takto sa nam model zlepsil, ked sme vyhodili ten jeden parameter, ktory pri modelovani nie je tak dolezity, 
#co vidime aj pri modelovani pomocou tohto samostatneho parametra, ze mse sa az tak nezlepsilo a nie je taky signifikantny
#vidime, ze Veh_type2 prispieva k lepsiemu modelu znizenim chybovosti

