library(tidyverse)
d <- read_csv("data/archive/survey.csv")
summary(d)
d <- d%>%select(treatment,Age,Gender,family_history,work_interfere,benefits,care_options)
names(d)
colSums(is.na(d))
glimpse(d)
d%>%count(treatment)
d_clean <- d_clean%>%mutate(treatment=factor(treatment,levels=c("Yes","No")))
d%>%count(Age)
d_clean <- d%>%filter(Age>=18&Age<=75)
d_clean%>%count(Age)
d_clean%>%count(Gender)

d_clean%>%count(Gender) 
d_clean%>%count(family_history)
d_clean%>%count(work_interfere)
d_clean <- d_clean%>%mutate(
  work_interfere=if_else(is.na(work_interfere),"Unknown",work_interfere),
)
d_clean%>%count(work_interfere)
names(d_clean)
d_clean%>%count(benefits)
d_clean <- d_clean%>%mutate(benefits=factor(benefits,levels = c("Yes","No","Don't know")))
d_clean%>%count(benefits)

d_clean%>%count(care_options)
d_clean <- d_clean%>%mutate(care_options=factor(care_options,levels=c("Yes","No","Not sure")))
d_clean%>%count(care_options)  
d_clean%>%count(treatment)
d_clean <- d_clean%>%mutate(treatment=factor(treatment,levels = c("Yes","No")))

d_clean%>%count(Gender)
d_clean <- d_clean%>%mutate(Gender=case_when(
  str_detect(str_to_lower(Gender),"female|^f$")~"Female",
  str_detect(str_to_lower(Gender),"male|^m$")~"Male",
  TRUE~"Other"
  ),
  Gender=factor(Gender,levels=c("Female","Male","Other")))


