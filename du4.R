library(dplyr)
library(ggplot2)
library(ChainLadder)

dt_PaidCase <- read.csv("Data/lesson4_PaidCase.csv")
summary(dt_PaidCase)

#z hodiny
Paid_HH_sml <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Small" & dataset_type == "PAID" ) 
head(Paid_HH_sml)
Paid_HH_sml_triangle <- Paid_HH_sml %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")
Paid_HH_sml_triangle
plot(Paid_HH_sml_triangle)
plot(predict(chainladder(Paid_HH_sml_triangle)))
ata(Paid_HH_sml_triangle)

################
#house & large data
Paid_HH_lrg <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Large" & dataset_type == "PAID" ) 
head(Paid_HH_lrg)
Paid_HH_lrg_triangle <- Paid_HH_sml %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")
Paid_HH_sml_triangle
plot(Paid_HH_lrg_triangle)
plot(predict(chainladder(Paid_HH_lrg_triangle)))
ata(Paid_HH_lrg_triangle)
#z grafu triangle vidime, ze casom naklady rastu cca este 4 roky a potom su priblizne konstantne,
#co dobre vidiet aj na grafe predikcii a taktiez aj na ata faktoroch blizkych 1
#ide o short tail, lebo po 4 rokoch su naklady malo rastuce, priblizne skonvergovane


#3rd party & small data
Paid_3_sml <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Small" & dataset_type == "PAID" ) 
head(Paid4_HH_sml)
Paid_3_sml_triangle <- Paid_HH_sml %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")
Paid_3_sml_triangle
plot(Paid_3_sml_triangle)
plot(predict(chainladder(Paid_3_sml_triangle)))
ata(Paid_3_sml_triangle)
#podobna uvaha ako pri house & large datach, len v tomto pripade su naklady konstantnejsie uz skor(po 3.roku)


#3rd party & large data
Paid_3_lrg <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Large" & dataset_type == "PAID" ) 
head(Paid_3_lrg)
Paid_3_lrg_triangle <- Paid_3_lrg %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")
Paid_3_lrg_triangle
plot(Paid_3_lrg_triangle)
plot(predict(chainladder(Paid_3_lrg_triangle)))
ata(Paid_3_lrg_triangle)
#na grafe vidime, ze naklady a ich vyvoj pre rozne roky su rozlicne, napr. 2,3 rastu pomerne stabilne,
#zatial co pri 4 vidime vyrazny narast nakladov v 3. roku
#podla predikovaneho modelu by malo ist o short tail


Paid_lrg <- dt_PaidCase %>% filter(ClaimSize == "Large" & dataset_type == "PAID" ) 
head(Paid_lrg)
Paid_lrg_triangle <- Paid_lrg %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")
Paid_lrg_triangle
plot(Paid_lrg_triangle)
plot(predict(chainladder(Paid_lrg_triangle)))
ata(Paid_lrg_triangle)

Paid_sml <- dt_PaidCase %>% filter(ClaimSize == "Small" & dataset_type == "PAID" ) 
head(Paid_sml)
Paid_sml_triangle <- Paid_3_lrg %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")
Paid_sml_triangle
plot(Paid_sml_triangle)
plot(predict(chainladder(Paid_sml_triangle)))
ata(Paid_sml_triangle)

#rozdiel medzi large a small je v konvergencii, pricom large skonverguje rychlejsie ako small

Paid_3 <- dt_PaidCase %>% filter(Business == "3rd Party" & dataset_type == "PAID" ) 
head(Paid_3)
Paid_3_triangle <- Paid_3 %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")
Paid_3_triangle
plot(Paid_3_triangle)
plot(predict(chainladder(Paid_3_triangle)))
ata(Paid_3_triangle)

Paid_h <- dt_PaidCase %>% filter(Business == "House" & dataset_type == "PAID" ) 
head(Paid_h)
Paid_h_triangle <- Paid_h %>% as.triangle(origin="ay", dev="dy", value="SumOfamount")
Paid_h_triangle
plot(Paid_h_triangle)
plot(predict(chainladder(Paid_h_triangle)))
ata(Paid_h_triangle)

#rozdiel medzi house a 3rd party, pricom naklady na house konverguju rychlejsie ako na 3rd party
#moze to byt vysvetlene tym, ze po oprave domov nevznikaju ziadne dalsie vyznamne naklady, pokial sa nieco dalsie nestane, co by ale bola poistna udalost, ktorej naklady sa rataju a kumuluju samostatne
#a pri autach, ak napr. bolo havarovane, opravi sa teraz, ale v dalsich rokoch sa mozu prejavit v dosledku nehody ine skrytejsie naklady
