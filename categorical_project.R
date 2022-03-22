#범주형 플젝
data=read.csv("data.csv",header=T)
names(data)<-c("country","bachelor","internet","marriage","GDP","GNI")
attach(data);head(data)

# Y: internet, X: bachelor, marriage, GDP
# by World Bank Atlas method. GNI per capita (current US$, Atlas).2019년 기준.

low-income(1): < $1026*10^10
low middle income(2): $1026*10^10 ~ $3995*10^10
upper middle income(3): $3996*10^10 ~ $12375*10^10
high-income(4): $12375*10^10 <

GNIclass=rep(NA,87)

for (i in 1:87){
if (GNI[i]<1026*10^10){
	GNIclass[i]=1
} else if (GNI[i]<3995*10^10){
	GNIclass[i]=2
} else if (GNI[i]<12375*10^10){
	GNIclass[i]=3
} else {
	GNIclass[i]=4
}
}

print(GNIclass)

data<-cbind(data, GNIclass)
head(data); attach(data)

# internet을 70을 기준으로, 70보다 크면 1, 아니면 0

int.class=rep(NA,87)

for (i in 1:87){
if (internet[i]<70){
	int.class[i]=1
} else {
	int.class[i]=0
}
}

print(int.class)

data<-cbind(data, int.class)
head(data); attach(data)

# 1) internet~bachelor: 그냥 linear fitting
fit1<-lm(internet~bachelor)
summary(fit1)

# 2) internet~bachelor: logistic fitting
fit2<-glm(int.class~bachelor, family=binomial)
summary(fit2)

# 3) internet~GNI: logistic fitting
fit3<-glm(int.class~factor(GNIclass), family=binomial)
summary(fit3)

# 4) internet~marriage: linear fitting
fit4<-lm(internet~marriage)
summary(fit4)

# 5) internet~marriage: logistic fitting
fit5<-glm(int.class~marriage, family=binomial)
summary(fit5)

# 6) internet~bachelor+GNI: logistic fitting
fit6<-glm(int.class~bachelor+factor(GNIclass), family=binomial)
summary(fit6)

# 7) internet~bachelor+marriage: logistic fitting
fit7<-glm(int.class~bachelor+marriage, family=binomial)
summary(fit7)

# 8) internet~bachelor+GNI+marriage: logistic fitting
fit8<-glm(int.class~bachelor+factor(GNIclass)+marriage, family=binomial)
summary(fit8)

# 9) internet~bachelor+GNI+marriage+bachelor*GNI: logistic fitting
fit9<-glm(int.class~bachelor+factor(GNIclass)+marriage+I(bachelor*marriage), family=binomial)
summary(fit9)


# 모형선택
# backward stepwise from fit9

library(MASS)
stepAIC(fit9)


# 모형진단
# deviance 비교->null model과 선택된 모델의 deviance 계산.

# 대체연결함수1: 프로빗 사용
fit10<-glm(int.class~bachelor+factor(GNIclass)+marriage+I(bachelor*marriage), family=binomial(link=probit))
summary(fit10)

# 대체연결함수2: 선형모형 사용
fit11<-lm(int.class~bachelor+factor(GNIclass)+marriage+I(bachelor*marriage))
summary(fit11)



