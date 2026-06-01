rm(list = ls())

#setwd("C:/Users/riosn/OneDrive/Documents/GMU/Papers in Progress/ConstrainedOOFA")

clean_survey_data = read.csv("clean_survey_data_4162024.csv")

# filter out times that took more than 2 hours (this doesn't seem to change the score_model much)
outlier_ids = which(clean_survey_data$Duration..in.seconds. > 7200)
if(length(outlier_ids) == 0){
  clean_data = clean_survey_data
} else{
  clean_data = clean_survey_data[-outlier_ids,]  
}

clean_data$duration = clean_data$Duration..in.seconds.
 # 63 observations
score_model = lm(Score ~ z12 + z34 + z35 + z36 + z46 + z56, data = clean_data)
summary(score_model)



library(dplyr)
library(car)
load("Figure 1.RData")
# set.seed(123) # random sample 1
set.seed(1234) # random sample 2
result.para<-NULL
for (i in 1:10^3){
uniform_data = clean_data %>% group_by(Order) %>% sample_n(size = 1)             
score_model2 = lm(Score ~ z12 + z34 + z35 + z36 + z46 + z56, data = uniform_data)
shapiro.test(score_model2$residuals)
ncvTest(score_model2, ~ z12 + z34 + z35 + z36 + z46 + z56)
result.para<-cbind(result.para,summary(score_model2)$coefficients[,c(1,4)])
}
\round(apply(sapply(1:10^3,function(x){result.para[,2*x-1]}),1, quantile, probs = c(0.025, 0.925), na.rm = TRUE),3)

set.seed(1234) 
nn=14
mm=1
result.para<-NULL
for (i in 1:10^3){
class.label<- sapply(1:nn,function(ii){as.vector(which(apply(clean_data[,20:25], 1, function(x) all(x == final.out.put_24n_14[[mm]]$design[ii,]))))})
uniform_data = clean_data[sapply(1:nn,function(x){class.label[[x]][sample(length(class.label[[x]]),1)]}),]
score_model2 = lm(Score ~ z12 + z34 + z35 + z36 + z46 + z56, data = uniform_data)
result.para<-cbind(result.para,summary(score_model2)$coefficients[,c(1,4)])
}

round(apply(sapply(1:10^3,function(x){result.para[,2*x-1]}),1, mean),3)
result1<-t(sapply(1:10^3,function(x){result.para[,2*x]}))

#sum(sapply(1:10^3,function(x){result.para[1,2*x]<=0.01}))
#sum(sapply(1:10^3,function(x){result.para[2,2*x]<=0.01}))
#sum(sapply(1:10^3,function(x){result.para[6,2*x]<=0.01}))
#shapiro.test(score_model2$residuals)
#ncvTest(score_model2, ~ z12 + z34 + z35 + z36 + z46 + z56)


#Figure 2(a)                                                       
par(mfrow= c(1,2),mar=c(2,2,2,2)+4,oma=c(1,1,1,1))
boxplot(result1,ylab="p-value",ylim=c(0,1),xaxt="n",cex.main=1.3,cex.lab=1.3)
lines(c(0,8),c(0.1,0.1),lty = 2,col="red")
axis(1,1:7,labels=c(expression(beta[0]),expression(beta[12]), expression(beta[34]),  expression(beta[35]),expression(beta[36]),expression(beta[46]),expression(beta[56])),cex.main=1.2,cex.lab=1.2)
title("(a) n=14",cex.main = 1.3)
                                                                                     

set.seed(1234) 
nn<-21
mm<-4
result.para<-NULL
for (i in 1:10^3){
  class.label<- sapply(1:nn,function(ii){as.vector(which(apply(clean_data[,20:25], 1, function(x) all(x == final.out.put_24n_21[[mm]]$design[ii,]))))})
  uniform_data = clean_data[sapply(1:nn,function(x){class.label[[x]][sample(length(class.label[[x]]),1)]}),]
  score_model2 = lm(Score ~ z12 + z34 + z35 + z36 + z46 + z56, data = uniform_data)
  result.para<-cbind(result.para,summary(score_model2)$coefficients[,c(1,4)])
}

round(apply(sapply(1:10^3,function(x){result.para[,2*x-1]}),1, mean),3)
#sum(sapply(1:10^3,function(x){result.para[1,2*x]<=0.01}))
#sum(sapply(1:10^3,function(x){result.para[2,2*x]<=0.01}))
#sum(sapply(1:10^3,function(x){result.para[6,2*x]<=0.01}))
#shapiro.test(score_model2$residuals)
#ncvTest(score_model2, ~ z12 + z34 + z35 + z36 + z46 + z56)

#Figure 2(b)                                                                                          
result2<-t(sapply(1:10^3,function(x){result.para[,2*x]}))
boxplot(result2,ylab="p-value",ylim=c(0,1),xaxt="n",cex.main=1.3,cex.lab=1.3)
lines(c(0,8),c(0.1,0.1),lty = 2,col="red")
axis(1,1:7,labels=c(expression(beta[0]),expression(beta[12]), expression(beta[34]),  expression(beta[35]),expression(beta[36]),expression(beta[46]),expression(beta[56])))
title("(b) n=21",cex.main = 1.3)

#a_data_frame <- data.frame(
#n = c(rep('n=14',7000),rep('n=21',7000)),
#Coff = c(rep(expression(beta[0]),1000),rep(expression(beta[12]),1000),rep(expression(beta[34]),1000),rep(expression(beta[35]),1000),rep(expression(beta[36]),1000),rep(expression(beta[46]),1000),rep(expression(beta[56]),1000)),
#Coff.value= c(result1,result2)
#)
#p1 <- ggplot(a_data_frame)+geom_boxplot(aes(x=Coff,y=Coff.value,fill=n))+scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1,10))



