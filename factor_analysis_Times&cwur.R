library(plm)
library(dplyr)
library(ggplot2)
library(plotly)
setwd("/Users/ArvSharma/Desktop/Social-Media-Analytics-in-Universties/")
rank <- read.csv("input/timesData.csv")
rank %>%
  select(research,university_name,total_score,year,teaching,citations,international,income) %>%
  filter(university_name=="Australian National University" | university_name=="University of Melbourne" | university_name=="University of Sydney" | university_name=="University of New South Wales" | university_name=="University of Western Australia" |
           university_name=="University of Adelaide" | university_name=="The University of Queensland" | university_name=="Monash University")-> G08
G08$total_score<-as.numeric(G08$total_score)
G08$international<-as.numeric(G08$international)
G08$income<-as.numeric(G08$income)
G08 <- pdata.frame(G08,c("university_name","year"))
randomfit1 <- plm(total_score~research+teaching+citations+international+income,
                  data=G08, index= c("university_name","year"), model="random")
fixedfit1 <- plm(total_score~research+teaching+citations+international+income,
                 data=G08, index= c("university_name","year"), model="within")
summary(randomfit1)
summary(fixedfit1)
phtest(randomfit1,fixedfit1)
df <- data.frame(Component = c("Research", "Teaching", "Citations", "International", "Income"), Impact = summary(randomfit1)$coef[2:6])
g <-ggplot(df, aes(Component, Impact)) + geom_bar(stat = "identity")+labs(x="SCORE COMPONENT", y="IMPACT ON TOTAL SCORE")
ggplotly(g)