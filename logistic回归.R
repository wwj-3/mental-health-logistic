d_clean <- d_clean%>%mutate(treatment=if_else(treatment=="Yes",1,0))
ggplot(d_clean,aes(x=Age,y=treatment))+
  geom_point(color="steelblue",size=2,alpha=0.6)+
  geom_smooth(method="glm",method.args=list(family="binomial"),
              color="red",se=TRUE)+
  theme_minimal()+
  labs(title="是否进行过心理治疗与年龄的关系",x="年龄",y="是否进行过心理治疗")
names(d_clean)
library(car)
fit_mulit_logiit <- glm(treatment~Age+Gender+family_history+work_interfere+benefits+care_options,
                        data=d_clean,family=binomial)
vif(fit_mulit_logiit)
par(mfrow=c(2,2))
plot(fit_mulit_logiit)
par(mfrow=c(1,1))

OR <- exp(coef(fit_mulit_logiit))
CI <- exp(confint(fit_mulit_logiit))
print(cbind(OR=OR,CI))









