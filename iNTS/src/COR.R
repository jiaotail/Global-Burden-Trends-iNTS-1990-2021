setwd('G:/11') ##设置工作路径
library(dplyr)
EC <- read.csv('nation.csv',header = T)  ## 读取我们的数据

####  1990 ASIR_EAPC
##获取ASIR
ASIR_1990 <- subset(EC,EC$year==2021 & 
                      EC$age=='Age-standardized' & 
                      EC$metric== 'Rate' &
                      EC$measure=='Incidence') ## 获取1990年EC年龄校正后发病率
ASIR_1990 <- ASIR_1990[,c(3,9)]  ###只取需要的变量
names(ASIR_1990)[2] <- 'ASR'
###获取绝对发病数
Incidence_case_1990 <- subset(EC,EC$year==2021 & 
                                EC$age=='All ages' & 
                                EC$metric== 'Number' &
                                EC$measure=='Incidence')
Incidence_case_1990 <- Incidence_case_1990[,c(3,9)]  ###只取需要的变量
names(Incidence_case_1990)[2] <- 'case'
#### 计算EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')

EAPC <- EAPC[,c(3,8,9)]

country <- ASIR_1990$location  ###获取国家名称
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

###合并三者数据成一个数据集
Total <- merge(Incidence_case_1990,EAPC_cal, by='location')
Total <- merge(Total,ASIR_1990, by='location')
Total_incidence <- Total
Total_incidence$group <- 'ASIR'  ###指示变量提示该数据为发病率数据






####  1990 ASDR_EAPC
##获取ASDR
ASDR_1990 <- subset(EC,EC$year==2021 & 
                      EC$age=='Age-standardized' & 
                      EC$metric== 'Rate' &
                      EC$measure=='Deaths') ## 获取1990年EC年龄校正后发病率
ASDR_1990 <- ASDR_1990[,c(3,9)]  ###只取需要的变量
names(ASDR_1990)[2] <- 'ASR'
###获取绝对发病数
Deaths_case_1990 <- subset(EC,EC$year==2021 & 
                             EC$age=='All ages' & 
                             EC$metric== 'Number' &
                             EC$measure=='Deaths')
Deaths_case_1990 <- Deaths_case_1990[,c(3,9)]  ###只取需要的变量
names(Deaths_case_1990)[2] <- 'case'
#### 计算EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Deaths')

EAPC <- EAPC[,c(3,8,9)]

country <- ASDR_1990$location  ###获取国家名称
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

###合并三者数据成一个数据集
Total <- merge(Deaths_case_1990,EAPC_cal, by='location')
Total <- merge(Total,ASDR_1990, by='location')
Total_Deaths <- Total
Total_Deaths$group <- 'ASDR'  ###指示变量提示该数据为死亡率数据

## 合并发病率及死亡率数据集
Total <- rbind(Total_incidence,Total_Deaths)
###绘制图像
library(ggplot2)
Total$group <- factor(Total$group, 
                      levels=c('ASIR','ASDR'), 
                      ordered=TRUE)
p1 <- ggplot(Total,aes(ASR, EAPC, size = case))+  
  geom_point(color='#0099CC')+
  geom_smooth(data = Total,aes(ASR, EAPC),se = .8,colour='black',span=1) +
  scale_size(name = 'Cases in 2021', breaks = c(100,1000,10000,50000),
             labels = c("<500","500-1,000","10,000-50,000",
                        '>50,000')) + facet_grid(.~group,scales="free") +
  theme_light()
p1
library(eoffice)
topptx(p1,"EAPC~ASR_2021_Deaths&Incidence.pptx")
### 计算pearson相关系数及对应P值
cor.test(Total_incidence$EAPC,Total_incidence$ASR,method="pearson")
cor.test(Total_Deaths$EAPC,Total_Deaths$ASR,method="pearson")



####  2021 HDI_EAPC
####  2021 ASIR_HDI
### 读取 HDI数据
HDI <- read_xlsx('HDI.xlsx')
names(HDI) <- c('location','HDI')

###获取绝对发病数
Incidence_case_2021 <- subset(EC,EC$year==2021 & 
                                EC$age=='All ages' & 
                                EC$metric== 'Number' &
                                EC$measure=='Incidence')
Incidence_case_2021 <- Incidence_case_2021[,c(3,9)]  ###只取需要的变量
names(Incidence_case_2021)[2] <- 'case'

#### 计算EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')

EAPC <- EAPC[,c(3,8,9)]

country <- Incidence_case_2021$location  ###获取国家名称
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

### 合并三者数据
Total <- merge(Incidence_case_2021,EAPC_cal, by='location')
Total <- merge(Total,HDI, by='location')
Total_incidence <- Total
Total_incidence$group <- 'ASIR'

###获取绝对发病数
Deaths_case_2021 <- subset(EC,EC$year==2021 & 
                             EC$age=='All ages' & 
                             EC$metric== 'Number' &
                             EC$measure=='Deaths')
Deaths_case_2021 <- Deaths_case_2021[,c(3,9)]  ###只取需要的变量
names(Deaths_case_2021)[2] <- 'case'

#### 计算EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Deaths')

EAPC <- EAPC[,c(3,8,9)]

country <- Deaths_case_2021$location  ###获取国家名称
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

### 合并三者数据
Total <- merge(Deaths_case_2021,EAPC_cal, by='location')
Total <- merge(Total,HDI, by='location')
Total_Deaths <- Total
Total_Deaths$group <- 'ASDR'

###合并发病率以及死亡率数据
Total <- rbind(Total_incidence,Total_Deaths)
### 作图
library(ggplot2)
Total$group <- factor(Total$group, 
                      levels=c('ASIR','ASDR'), 
                      ordered=TRUE)
p1 <- ggplot(Total,aes(HDI, EAPC, size = case))+  
  geom_point(color='#0099CC')+
  geom_smooth(data = Total,aes(HDI, EAPC),se = .8,colour='black',span=1) +
  scale_size(name = 'Cases in 2021', breaks = c(100,1000,10000,50000),
             labels = c("<500","500-1,000","10,000-50,000",
                        '>50,000')) + facet_grid(.~group,scales="free") +
  theme_light()
p1
library(eoffice)
topptx(p1,"EAPC~HDI_2021_Deaths&Incidence.pptx")
### 计算pearson相关性系数
cor.test(Total_incidence$EAPC,Total_incidence$HDI,method="pearson")
cor.test(Total_Deaths$EAPC,Total_Deaths$HDI,method="pearson")
