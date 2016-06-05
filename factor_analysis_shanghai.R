library(plm)
library(dplyr)
library(ggplot2)
library(plotly)
setwd("/Users/ArvSharma/Desktop/Social-Media-Analytics-in-Universties/")
rank <- read.csv("input/shanghaiData.csv")
rank %>%
  select(alumni,university_name,total_score,year,award,hici,ns,pub) %>%
  filter(university_name=="Australian National University" | university_name=="University of Melbourne" | university_name=="University of Sydney" | university_name=="University of New South Wales" | university_name=="University of Western Australia" |
           university_name=="University of Adelaide" | university_name=="The University of Queensland" | university_name=="Monash University")-> G08
G08$total_score<-as.numeric(G08$total_score)
G08$Alumni<-as.numeric(G08$alumni)
G08$award<-as.numeric(G08$award)

##I fit a random effect and a fixed effects panel data model. Using the results of
##the Hausman test I select the random effects model.

G08 <- pdata.frame(G08,c("university_name","year"))
randomfit1 <- plm(total_score~alumni+award+hici+ns+pub,
                  data=G08, index= c("university_name","year"), model="random")
fixedfit1 <- plm(total_score~alumni+award+hici+ns+pub,
                 data=G08, index= c("university_name","year"), model="within")
summary(fixedfit1)
summary(randomfit1)
phtest(randomfit1,fixedfit1)

df <- data.frame(Component = c("Alumni", "Award", "HICI", "NS", "Pub"), Impact = summary(randomfit1)$coef[2:6])
g <-ggplot(df, aes(Component, Impact)) + geom_bar(stat = "identity")+labs(x="SCORE COMPONENT", y="IMPACT ON TOTAL SCORE")
ggplotly(g)