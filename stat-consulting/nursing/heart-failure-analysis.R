rm(list=ls())
setwd("C:/Users/jd033/OneDrive/Documents/R/Nursing")
heart <- read.csv("Heart-failure-project-data.csv",sep=",",header=T)
str(heart)
library(tidyverse)
# heart.wide.1 <- spread(heart,Question,Score)

library(reshape2)
colnames(heart)[1] <- "subject"
heart.new <- heart[,c(1:3,5)]
heart.wide <-  dcast(heart.new, subject + Group ~ Question, sum, value.var="Score")
head(heart.wide)

recode(heart.wide$`Forget Meds`, `1` = 4, `2` = 3, `3`= 2, `4` = 1, `0` = 0)

heart.wide[is.na(heart.wide)] <- 0 

# (heart.wide$sec.A.score = Weigh+Swelling+Sick+Activity+Appointment+`Low Salt Diet`+
  # Exercise+`Forget Meds`+`Ask for  low Salt`+`Pill box`)

heart.wide$sec.A.score = with(heart.wide, Weigh+Swelling+Sick+Activity+Appointment+`Low Salt Diet`+
       Exercise+`Forget Meds`+`Ask for  low Salt`+`Pill box`)
# Max.sec.A.score = 4*10
# Min.sec.A.score = 0
# (heart.wide$normalized.sec.A.score = (heart.wide$sec.A.score - Min.sec.A.score)/(Max.sec.A.score - Min.sec.A.score)*100)

(heart.wide$normalized.sec.A.score = (heart.wide$sec.A.score - 10)*3.33)

attach(heart.wide)

fit <- lm(normalized.sec.A.score ~ Group, data = heart.wide)
summary(fit)

fit <- lm(`Ask for  low Salt` ~ Group, data = heart.wide)
summary(fit)


# library(lme4)
# gm2 <- lmer(`Low Salt Diet` ~ Group + (1 | subject), data = heart.wide)

heart.wide$sec.B.score = with(heart.wide, Symptoms + Recognize + `Reduce Na`+`Reduce H2O`
                              +`Inc Med`+`Contact NP`+ `Remedy Eval`)
summary(heart.wide$sec.B.score)

# Max.sec.B.score = 4*7 #max(heart.wide$sec.B.score)
# Min.sec.B.score = 0 #min(heart.wide$sec.B.score)
(heart.wide$normalized.sec.B.score = (heart.wide$sec.B.score - Min.sec.B.score)/(Max.sec.B.score - Min.sec.B.score)*100)

(heart.wide$management = (heart.wide$sec.B.score-4)*5)

fit <- lm(management ~ Group, data = heart.wide)
summary(fit)

heart.wide$sec.C.score = with(heart.wide, `Free fromSx` + `Follow Tx`+ `Eval Sx` + `Rec change`+
                                `Act on Sx`+`Eval Tx`)
summary(heart.wide$sec.C.score)

Max.sec.C.score = max(heart.wide$sec.C.score)
Min.sec.C.score = min(heart.wide$sec.C.score)
(heart.wide$normalized.sec.C.score = (heart.wide$sec.C.score - Min.sec.C.score)/(Max.sec.C.score - Min.sec.C.score))

(heart.wide$confidence = (heart.wide$sec.C.score - 6)*5.56)

fit <- lm(confidence ~ Group, data = heart.wide)
summary(fit)
