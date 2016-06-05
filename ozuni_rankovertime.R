# Listing out all the Universities in Australia

uni1 <- c("University of Melbourne", "The University of Melbourne")
uni2 <- c("Australian National University", "The Australian National University")
uni3 <- c("University of Sydney")
uni4 <- c("University of Adelaide", "The University of Adelaide")
uni5 <- c("The University of Queensland", "University of Queensland")
uni6 <- c("University of New South Wales", "The University of New South Wales")
uni7 <- c("Monash University")
uni8 <- c("University of Western Australia", "The University of Western Australia")
uni9 <- c("Macquarie University")
uni10 <- c("University of Wollongong")
uni11 <- c("Queensland University of Technology")
uni12 <- c("University of Newcastle", "The University of Newcastle, Australia")
uni13 <- c("Charles Darwin University")
uni14 <- c("University of Tasmania")
uni15 <- c("Curtin University", "Curtin University of Technology")
uni16 <- c("Deakin University")
uni17 <- c("Flinders University")
uni18 <- c("Griffith University")
uni19 <- c("La Trobe University")
uni20 <- c("Swinburne University of Technology")
uni21 <- c("University of South Australia")
uni22 <- c("Murdoch University")
uni23 <- c("University of Technology Sydney", "University of Technology, Sydney")
uni24 <- c("Western Sydney University", "Western Sydney University")
uni25 <- c("James Cook University")

setwd("/Users/ArvSharma/Desktop/Social-Media-Analytics-in-Universties")

library('readr')   
library('ggplot2')  # visualization
library('ggthemes') # visualization
library('dplyr')    # data manipulation
library('stringr')  # text manipulation
library('tidyr')
library(readr)

data_times <- read_csv('input/timesData.csv')
data_shanghai <- read_csv('input/shanghaiData.csv')
data_cwur <- read_csv('input/cwurData.csv')
data_all <- c(data_times$university_name, data_shanghai$university_name) %>% unique

getwd()


df_unis_shanghai_uni1 <- data_shanghai %>% filter(university_name %in% c(uni1)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni1[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni1 <- data_times %>% filter(university_name %in% c(uni1)) %>% 
  transmute(dataset = 'times', year, university_name = uni1[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni1 <- rbind(df_unis_shanghai_uni1, df_unis_times_uni1)

df_unis_shanghai_uni2 <- data_shanghai %>% filter(university_name %in% c(uni2)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni2[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni2 <- data_times %>% filter(university_name %in% c(uni2)) %>% 
  transmute(dataset = 'times', year, university_name = uni2[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni2 <- rbind(df_unis_shanghai_uni2, df_unis_times_uni2)

df_unis_shanghai_uni3 <- data_shanghai %>% filter(university_name %in% c(uni3)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni3[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni3 <- data_times %>% filter(university_name %in% c(uni3)) %>% 
  transmute(dataset = 'times', year, university_name = uni3[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni3 <- rbind(df_unis_shanghai_uni3, df_unis_times_uni3)

df_unis_shanghai_uni4 <- data_shanghai %>% filter(university_name %in% c(uni4)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni4[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni4 <- data_times %>% filter(university_name %in% c(uni4)) %>% 
  transmute(dataset = 'times', year, university_name = uni4[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni4 <- rbind(df_unis_shanghai_uni4, df_unis_times_uni4)

df_unis_shanghai_uni5 <- data_shanghai %>% filter(university_name %in% c(uni5)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni5[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni5 <- data_times %>% filter(university_name %in% c(uni5)) %>% 
  transmute(dataset = 'times', year, university_name = uni5[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni5 <- rbind(df_unis_shanghai_uni5, df_unis_times_uni5)

df_unis_shanghai_uni6 <- data_shanghai %>% filter(university_name %in% c(uni6)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni6[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni6 <- data_times %>% filter(university_name %in% c(uni6)) %>% 
  transmute(dataset = 'times', year, university_name = uni6[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni6 <- rbind(df_unis_shanghai_uni6, df_unis_times_uni6)

df_unis_shanghai_uni7 <- data_shanghai %>% filter(university_name %in% c(uni7)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni7[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni7 <- data_times %>% filter(university_name %in% c(uni7)) %>% 
  transmute(dataset = 'times', year, university_name = uni7[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni7 <- rbind(df_unis_shanghai_uni7, df_unis_times_uni7)

df_unis_shanghai_uni8 <- data_shanghai %>% filter(university_name %in% c(uni8)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni8[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni8 <- data_times %>% filter(university_name %in% c(uni8)) %>% 
  transmute(dataset = 'times', year, university_name = uni8[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni8 <- rbind(df_unis_shanghai_uni8, df_unis_times_uni8)

df_unis_shanghai_uni9 <- data_shanghai %>% filter(university_name %in% c(uni9)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni9[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni9 <- data_times %>% filter(university_name %in% c(uni9)) %>% 
  transmute(dataset = 'times', year, university_name = uni9[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni9 <- rbind(df_unis_shanghai_uni9, df_unis_times_uni9)

df_unis_shanghai_uni10 <- data_shanghai %>% filter(university_name %in% c(uni10)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni10[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni10 <- data_times %>% filter(university_name %in% c(uni10)) %>% 
  transmute(dataset = 'times', year, university_name = uni10[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni10 <- rbind(df_unis_shanghai_uni10, df_unis_times_uni10)

df_unis_shanghai_uni11 <- data_shanghai %>% filter(university_name %in% c(uni11)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni11[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni11 <- data_times %>% filter(university_name %in% c(uni11)) %>% 
  transmute(dataset = 'times', year, university_name = uni11[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni11 <- rbind(df_unis_shanghai_uni11, df_unis_times_uni11)

df_unis_shanghai_uni12 <- data_shanghai %>% filter(university_name %in% c(uni12)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni12[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni12 <- data_times %>% filter(university_name %in% c(uni12)) %>% 
  transmute(dataset = 'times', year, university_name = uni12[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni12 <- rbind(df_unis_shanghai_uni12, df_unis_times_uni12)

df_unis_shanghai_uni13 <- data_shanghai %>% filter(university_name %in% c(uni13)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni13[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni13 <- data_times %>% filter(university_name %in% c(uni13)) %>% 
  transmute(dataset = 'times', year, university_name = uni13[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni13 <- rbind(df_unis_shanghai_uni13, df_unis_times_uni13)

df_unis_shanghai_uni14 <- data_shanghai %>% filter(university_name %in% c(uni14)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni14[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni14 <- data_times %>% filter(university_name %in% c(uni14)) %>% 
  transmute(dataset = 'times', year, university_name = uni14[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni14 <- rbind(df_unis_shanghai_uni14, df_unis_times_uni14)

df_unis_shanghai_uni15 <- data_shanghai %>% filter(university_name %in% c(uni15)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni15[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni15 <- data_times %>% filter(university_name %in% c(uni15)) %>% 
  transmute(dataset = 'times', year, university_name = uni15[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni15 <- rbind(df_unis_shanghai_uni15, df_unis_times_uni15)

df_unis_shanghai_uni16 <- data_shanghai %>% filter(university_name %in% c(uni16)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni16[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni16 <- data_times %>% filter(university_name %in% c(uni16)) %>% 
  transmute(dataset = 'times', year, university_name = uni16[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni16 <- rbind(df_unis_shanghai_uni16, df_unis_times_uni16)

df_unis_shanghai_uni17 <- data_shanghai %>% filter(university_name %in% c(uni17)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni17[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni17 <- data_times %>% filter(university_name %in% c(uni17)) %>% 
  transmute(dataset = 'times', year, university_name = uni17[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni17 <- rbind(df_unis_shanghai_uni17, df_unis_times_uni17)

df_unis_shanghai_uni18 <- data_shanghai %>% filter(university_name %in% c(uni18)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni18[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni18 <- data_times %>% filter(university_name %in% c(uni18)) %>% 
  transmute(dataset = 'times', year, university_name = uni18[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni18 <- rbind(df_unis_shanghai_uni18, df_unis_times_uni18)

df_unis_shanghai_uni19 <- data_shanghai %>% filter(university_name %in% c(uni19)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni19[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni19 <- data_times %>% filter(university_name %in% c(uni19)) %>% 
  transmute(dataset = 'times', year, university_name = uni19[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni19 <- rbind(df_unis_shanghai_uni19, df_unis_times_uni19)

df_unis_shanghai_uni20 <- data_shanghai %>% filter(university_name %in% c(uni20)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni20[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni20 <- data_times %>% filter(university_name %in% c(uni20)) %>% 
  transmute(dataset = 'times', year, university_name = uni20[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni20 <- rbind(df_unis_shanghai_uni20, df_unis_times_uni20)

df_unis_shanghai_uni21 <- data_shanghai %>% filter(university_name %in% c(uni21)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni21[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni21 <- data_times %>% filter(university_name %in% c(uni21)) %>% 
  transmute(dataset = 'times', year, university_name = uni21[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni21 <- rbind(df_unis_shanghai_uni21, df_unis_times_uni21)

df_unis_shanghai_uni22 <- data_shanghai %>% filter(university_name %in% c(uni22)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni22[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni22 <- data_times %>% filter(university_name %in% c(uni22)) %>% 
  transmute(dataset = 'times', year, university_name = uni22[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni22 <- rbind(df_unis_shanghai_uni22, df_unis_times_uni22)

df_unis_shanghai_uni23 <- data_shanghai %>% filter(university_name %in% c(uni23)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni23[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni23 <- data_times %>% filter(university_name %in% c(uni23)) %>% 
  transmute(dataset = 'times', year, university_name = uni23[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni23 <- rbind(df_unis_shanghai_uni23, df_unis_times_uni23)

df_unis_shanghai_uni24 <- data_shanghai %>% filter(university_name %in% c(uni24)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni24[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni24 <- data_times %>% filter(university_name %in% c(uni24)) %>% 
  transmute(dataset = 'times', year, university_name = uni24[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni24 <- rbind(df_unis_shanghai_uni24, df_unis_times_uni24)

df_unis_shanghai_uni25 <- data_shanghai %>% filter(university_name %in% c(uni25)) %>% 
  transmute(dataset = 'shanghai', year, university_name = uni25[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))
df_unis_times_uni25 <- data_times %>% filter(university_name %in% c(uni25)) %>% 
  transmute(dataset = 'times', year, university_name = uni25[1], world_rank = as.integer(sapply(strsplit(world_rank,"-"),"[[",1)))

df_combine_uni25 <- rbind(df_unis_shanghai_uni25, df_unis_times_uni25)


df_combine <- rbind(
  df_combine_uni1, 
  df_combine_uni2,
  df_combine_uni3,
  df_combine_uni4,
  df_combine_uni5,
  df_combine_uni6,
  df_combine_uni7,
  df_combine_uni8)

df_combine <- rbind(
  df_unis_shanghai_uni1,
  df_unis_shanghai_uni2, 
  df_unis_shanghai_uni3, 
  df_unis_shanghai_uni4, 
  df_unis_shanghai_uni5, 
  df_unis_shanghai_uni6, 
  df_unis_shanghai_uni7, 
  df_unis_shanghai_uni8,
  df_unis_times_uni1, 
  df_unis_times_uni2,
  df_unis_times_uni3,
  df_unis_times_uni4,
  df_unis_times_uni5,
  df_unis_times_uni6,
  df_unis_times_uni7,
  df_unis_times_uni8) %>% 
  mutate(yeardate = as.Date(paste0(year, '-01', '-01')))

df_averages <- df_combine %>% 
  group_by(university_name, yeardate) %>% 
  summarize(avg_rank = mean(world_rank))

number_ticks <- function(n) {function(limits) pretty(limits, n)}


average_university_ranking <- df_averages %>% 
  ggplot(aes(yeardate, avg_rank, color=university_name))+
  geom_line() +
  scale_y_reverse(breaks=number_ticks(10)) +
  labs (x="Year", y="World Rank") +
  theme_few() +
  theme(legend.position="bottom") +
  guides(col=guide_legend(ncol=2,byrow=TRUE)) +
  ggtitle('Australian Universities Rank Over Time')


average_university_ranking