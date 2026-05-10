#### for joinpoint
setwd('G:/11') ##设置工作路径
library(dplyr)                            ## 读取需要的R包
library(ggplot2)
#EC <- read.csv('region.csv',header = T)  ## 读取我们的数据
EC <- read.csv('nation.csv',header = T)  ## 读取我们的数据
order <- read_xlsx('nations_name.xlsx')
matched_rows<-match(EC$location_id,order$location_id)
EC$location<-order$location_name_T[matched_rows[!is.na(matched_rows)]]
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Deaths')

EAPC$SE <- (EAPC$upper-EAPC$lower)/(1.96*2)   ##根据95%UI 计算标准误
EAPC <- EAPC[,c(3,8,9,12)] ##获取地区、年份以及对应的数值
EAPC <- EAPC[order(EAPC$location,EAPC$year),]   ### joinpoint要求年份按照升序排列，且一个组放在一起

write.csv(EAPC,'joinpoint_nation_Deaths.csv')


#### 数据处理好后,继续用R处理
AAPC <- read.table('nation_Deaths.Export.AAPC.txt',header = T)

AAPC <- AAPC[,c(1,6,7,8)]
names(AAPC)[2:4] <- c('val','lower','upper')

AAPC$AAPC <- paste(AAPC$lower,AAPC$upper,sep = ' - ') ## 用"-"连接95%UI上下数值
AAPC$AAPC <- paste(AAPC$AAPC,')',sep = '')  ##95%UI前后加括号             
AAPC$AAPC <- paste('(',AAPC$AAPC,sep = '')  ##95%UI前后加括号
AAPC$AAPC <- paste(AAPC$val,AAPC$AAPC,sep = ' ') ##数据和95%UI用空格键连接

## 我们获取推文1里的数据
## 1990发病人数
EC_1990 <- subset(EC,EC$year==1990 & 
                    EC$age=='All ages' & 
                    EC$metric== 'Number' &
                    EC$measure=='Deaths')

EC_1990 <- EC_1990[,c(3,9,10,11)]  ### 只取需要的变量：地区以及对应的数值
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

EC_2021 <- EC_2021[,c(3,9,10,11)]  ### 只取需要的变量：地区以及对应的数值
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

ASR_1990 <- ASR_1990[,c(3,9,10,11)]  ### 只取需要的变量：地区以及对应的数值
ASR_1990$val <- round(ASR_1990$val,1)  ###取整
ASR_1990$lower <- round(ASR_1990$lower,1)###取整
ASR_1990$upper <- round(ASR_1990$upper,1) ###取整
ASR_1990$ASR_1990 <- paste(ASR_1990$lower,ASR_1990$upper,sep = '-') ## 用-连接95%UI上下数值
ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990,')',sep = '')  ##95%UI前后加括号             
ASR_1990$ASR_1990 <- paste('(',ASR_1990$ASR_1990,sep = '')  ##95%UI前后加括号
ASR_1990$ASR_1990 <- paste(ASR_1990$val,ASR_1990$ASR_1990,sep = ' ') ##数据和95%UI用空格键连接


## 2021 ASR
ASR_2021 <- subset(EC,EC$year==2021 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='Deaths')

ASR_2021 <- ASR_2021[,c(3,9,10,11)]  ### 只取需要的变量：地区以及对应的数值
ASR_2021$val <- round(ASR_2021$val,1)  ###取整
ASR_2021$lower <- round(ASR_2021$lower,1)###取整
ASR_2021$upper <- round(ASR_2021$upper,1) ###取整
ASR_2021$ASR_2021 <- paste(ASR_2021$lower,ASR_2021$upper,sep = '-') ## 用-连接95%UI上下数值
ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021,')',sep = '')  ##95%UI前后加括号             
ASR_2021$ASR_2021 <- paste('(',ASR_2021$ASR_2021,sep = '')  ##95%UI前后加括号
ASR_2021$ASR_2021 <- paste(ASR_2021$val,ASR_2021$ASR_2021,sep = ' ') ##数据和95%UI用空格键连接

### 数据整合
EC_1990 <- EC_1990[,c(1,5)]  ###取地区和整合好的变量
ASR_1990 <- ASR_1990[,c(1,5)]
EC_2021 <- EC_2021[,c(1,5)]
ASR_2021 <- ASR_2021[,c(1,5)]
AAPC <- AAPC[,c(1,5)]

Deaths <- merge(EC_1990,ASR_1990,by='location')
Deaths <- merge(Deaths,EC_2021,by='location')
Deaths <- merge(Deaths,ASR_2021,by='location')
Deaths <- merge(Deaths,AAPC,by='location')
write.csv(Deaths,'AAPC_Results for nation_Deaths.csv')

