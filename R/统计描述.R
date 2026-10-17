library(tidyverse)
d <- read_csv("data/archive/survey.csv")
p1 <- ggplot(d_clean,aes(x=Age))+
  geom_histogram(aes(y=after_stat(density)),binwidth=2,
                 fill="steelblue",color="white")+
  geom_density(color="red",linewidth=1)+
  labs(title="年龄分布直方图",x="年龄",y="样本分位数")+
  theme_minimal()

p2 <- ggplot(d_clean,aes(sample=Age))+
  stat_qq(color="darkred",size=1.5)+
  stat_qq_line(color="blue",linewidth=1)+
  labs(title="年龄QQ图",x="理论分数",y="样本分位数")
  theme_minimal()
names(d_clean)
#通过两张图判断是偏态 
library(gtsummary)
table1 <- d_clean%>%
  select(treatment,Age,Gender,family_history,work_interfere,benefits,care_options)%>%
  tbl_summary(
    by=treatment,
    statistic=list(
      all_continuous()~"{median}({p25},{p75})",
      all_categorical()~"{n}({p}%)"
    ),
    digits=all_continuous()~1
  )%>%add_p(test=list(all_continuous()~"wilcox.test",
                      all_categorical()~"chisq.test"))%>%
  add_overall()
table1
















