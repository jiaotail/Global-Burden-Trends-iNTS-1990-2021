setwd('G:/11') ##设置工作路径
library(dplyr)                            ## 读取需要的R包
library(ggplot2)
EC <- read.csv('nation.csv',header = T)  ## 读取我们的数据
order <- read_xlsx('nations_name.xlsx')
matched_rows<-match(EC$location_id,order$location_id)
EC$location<-order$location_name_T[matched_rows[!is.na(matched_rows)]]

EC<-read.csv('region.csv',header=T)
order<-read.csv('order.csv',header = F)
EC$location <- factor(EC$location, 
                      levels=order$V1, 
                      ordered=TRUE)

## 1990发病人数
EC_1990 <- subset(EC,EC$year==1990 & 
                    EC$age=='All ages' & 
                    EC$metric== 'Number' &
                    EC$measure=='Deaths')
EC_1990 <- EC_1990[,c(2,8,9,10)]
#EC_1990 <- EC_1990[,c(3,9,10,11)]  ### 只取需要的变量：地区以及对应的数值
EC_1990$val <- round(EC_1990$val,1)  ###取整
EC_1990$lower <- round(EC_1990$lower,1)###取整
EC_1990$upper <- round(EC_1990$upper,1) ###取整
EC_1990$Num_1990 <- paste(EC_1990$lower,EC_1990$upper,sep = '-') ## 用-连接95%UI上下数值
EC_1990$Num_1990 <- paste(EC_1990$Num_1990,')',sep = '')  ##95%UI前后加括号             
EC_1990$Num_1990 <- paste('(',EC_1990$Num_1990,sep = '')  ##95%UI前后加括号
EC_1990$Num_1990 <- paste(EC_1990$val,EC_1990$Num_1990,sep = ' ') ##数据和95%UI用空格键连接

## 2021发病人数
EC_2021 <- subset(EC,EC$year==2021 & 
                    EC$age=='All ages' & 
                    EC$metric== 'Number' &
                    EC$measure=='Deaths')
EC_2021 <- EC_2021[,c(2,8,9,10)]
#EC_2021 <- EC_2021[,c(3,9,10,11)]  ### 只取需要的变量：地区以及对应的数值
EC_2021$val <- round(EC_2021$val,1)  ###取整

EC_2021$lower <- round(EC_2021$lower,1)###取整
EC_2021$upper <- round(EC_2021$upper,1) ###取整
EC_2021$Num_2021 <- paste(EC_2021$lower,EC_2021$upper,sep = '-') ## 用-连接95%UI上下数值
EC_2021$Num_2021 <- paste(EC_2021$Num_2021,')',sep = '')  ##95%UI前后加括号             
EC_2021$Num_2021 <- paste('(',EC_2021$Num_2021,sep = '')  ##95%UI前后加括号
EC_2021$Num_2021 <- paste(EC_2021$val,EC_2021$Num_2021,sep = ' ') ##数据和95%UI用空格键连接


## 1990 ASR
ASR_1990 <- subset(EC,EC$year==1990 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='Deaths')
ASR_1990 <- ASR_1990[,c(2,8,9,10)]
#ASR_1990 <- ASR_1990[,c(3,9,10,11)]  ### 只取需要的变量：地区以及对应的数值
ASR_1990$val <- round(ASR_1990$val,2)  ###取整
ASR_1990$lower <- round(ASR_1990$lower,2)###取整
ASR_1990$upper <- round(ASR_1990$upper,2) ###取整
ASR_1990$ASR_1990 <- paste(ASR_1990$lower,ASR_1990$upper,sep = '-') ## 用-连接95%UI上下数值
ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990,')',sep = '')  ##95%UI前后加括号             
ASR_1990$ASR_1990 <- paste('(',ASR_1990$ASR_1990,sep = '')  ##95%UI前后加括号
ASR_1990$ASR_1990 <- paste(ASR_1990$val,ASR_1990$ASR_1990,sep = ' ') ##数据和95%UI用空格键连接


## 2021 ASR
ASR_2021 <- subset(EC,EC$year==2021 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='Deaths')

ASR_2021 <- ASR_2021[,c(2,8,9,10)]
#ASR_2021 <- ASR_2021[,c(3,9,10,11)]  ### 只取需要的变量：地区以及对应的数值
ASR_2021$val <- round(ASR_2021$val,2)  ###取整
ASR_2021$lower <- round(ASR_2021$lower,2)###取整
ASR_2021$upper <- round(ASR_2021$upper,2) ###取整
ASR_2021$ASR_2021 <- paste(ASR_2021$lower,ASR_2021$upper,sep = '-') ## 用-连接95%UI上下数值
ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021,')',sep = '')  ##95%UI前后加括号             
ASR_2021$ASR_2021 <- paste('(',ASR_2021$ASR_2021,sep = '')  ##95%UI前后加括号
ASR_2021$ASR_2021 <- paste(ASR_2021$val,ASR_2021$ASR_2021,sep = ' ') ##数据和95%UI用空格键连接

EC_Deaths <- subset(EC, EC$age=='Age-standardized' & 
                         EC$metric== 'Rate' &
                         EC$measure=='Deaths')

##### EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Deaths')
EAPC <- EAPC[,c(2,7,8)]
#EAPC <- EAPC[,c(3,8,9)] ##获取地区、年份以及对应的数值

country <- EC_1990$location
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=27),UCI=rep(0,times=27),LCI=rep(0,times=27)) 
for (i in 1:27){  ###总共27个地区，所以循环27次
  country_cal <- as.character(EAPC_cal[i,1]) ### 依次取对应的地区
  a <- subset(EAPC, EAPC$location==country_cal)  ##取对应地区的数据子集
  a$y <- log(a$val)  ##根据EAPC计算方法计算y值
  mod_simp_reg<-lm(y~year,data=a) ##根据EAPC计算方法做线性回归方程
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100 ##根据EAPC计算方法取方程beta值来计算EAPC
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  ### 计算EAPC的95%可信区间的上限值
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  ### 计算EAPC的95%可信区间的下限值
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
plot(a$year, log(a$val), type="b", main=country_cal)
summary(mod_simp_reg)
summary(a$val)
hist(a$val, breaks=10)


country <- EC_1990$location
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204)) 
for (i in 1:204){  ###总共204个地区，所以循环204次
  country_cal <- as.character(EAPC_cal[i,1]) ### 依次取对应的地区
  a <- subset(EAPC, EAPC$location==country_cal)  ##取对应地区的数据子集
  a$y <- log(a$val)  ##根据EAPC计算方法计算y值
  mod_simp_reg<-lm(y~year,data=a) ##根据EAPC计算方法做线性回归方程
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100 ##根据EAPC计算方法取方程beta值来计算EAPC
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  ### 计算EAPC的95%可信区间的上限值
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  ### 计算EAPC的95%可信区间的下限值
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}

EAPC_cal$EAPC <- round(EAPC_cal$EAPC,2)  ##保留2位小数点
EAPC_cal$UCI <- round(EAPC_cal$UCI,2)
EAPC_cal$LCI <- round(EAPC_cal$LCI,2)
EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI,EAPC_cal$UCI,sep = '-') 
EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI,')',sep = '') 
EAPC_cal$EAPC_CI <- paste('(',EAPC_cal$EAPC_CI,sep = '') 
EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC,EAPC_cal$EAPC_CI,sep = ' ')  

### 数据整合
EC_1990 <- EC_1990[,c(1,5)]  ###取地区和整合好的变量
ASR_1990 <- ASR_1990[,c(1,5)]
EC_2021 <- EC_2021[,c(1,5)]
ASR_2021 <- ASR_2021[,c(1,5)]
EAPC_cal <- EAPC_cal[,c(1,5)]
Deaths <- merge(EC_1990,ASR_1990,by='location')
Deaths <- merge(Deaths,EC_2021,by='location')
Deaths <- merge(Deaths,ASR_2021,by='location')
Deaths <- merge(Deaths,EAPC_cal,by='location')
write.csv(Deaths,'Results for region_Deaths.csv')
write.csv(Deaths,'Results for nation_Deaths.csv')

