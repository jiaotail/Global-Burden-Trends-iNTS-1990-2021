setwd('G:/11') ##设置工作路径
#  install.packages('ggmap')
#  install.packages('rgdal')
#  install.packages('maps')
#  install.packages('dplyr')
library(ggmap)
library(rgdal)
library(maps)
library(dplyr)

EC <- read.csv('nation.csv',header = T)  ## 读取我们的数据
ASR_2021 <- subset(EC,EC$year==2021 & 
                     EC$age=='Age-standardized' & 
                     EC$metric== 'Rate' &
                     EC$measure=='Incidence') ## 获取2021年EC年龄校正后发病率
ASR_2021 <- ASR_2021[,c(3,9,10,11)]
ASR_2021$val <- round(ASR_2021$val,1) ###保留一位小数点
ASR_2021$lower <- round(ASR_2021$lower,1) ###保留一位小数点
ASR_2021$upper <- round(ASR_2021$upper,1) ###保留一位小数点

####  map for ASR
worldData <- map_data('world')
country_asr <- ASR_2021
country_asr$location <- as.character(country_asr$location) 
###以下代码的目的是让country_asr$location的国家名称与worldData的国家名称一致
### 这样才能让数据映射到地图上
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == 'Democratic Socialist Sri Lanka'] = 'Sri Lanka'
country_asr$location[country_asr$location == 'Independent State of Papua New Guinea'] = 'Papua New Guinea'
country_asr$location[country_asr$location == 'Democratic Timor-Leste'] = 'Timor-Leste'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == 'Kyrgyz Republic'] = 'Kyrgyzstan'
country_asr$location[country_asr$location == "Democratic People's Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Korea"] = 'South Korea'
country_asr$location[country_asr$location == 'Federated States of Micronesia'] = 'Micronesia'
country_asr$location[country_asr$location == 'Independent State of Samoa'] = 'Samoa'
country_asr$location[country_asr$location == 'Socialist Viet Nam'] = 'Vietnam'
country_asr$location[country_asr$location == "People's China"] = 'China'
country_asr$location[country_asr$location == 'Union of Myanmar'] = 'Myanmar'
country_asr$location[country_asr$location == 'Principality of Andorra'] = 'Andorra'
country_asr$location[country_asr$location == 'Federal Germany'] = 'Germany'
country_asr$location[country_asr$location == 'Argentine Republic'] = 'Argentina'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'Brunei Darussalam'] = 'Brunei'
country_asr$location[country_asr$location == 'French Republic'] = 'France'
country_asr$location[country_asr$location == 'State of Israel'] = 'Israel'
country_asr$location[country_asr$location == 'Grand Duchy of Luxembourg'] = 'Luxembourg'
country_asr$location[country_asr$location == 'Slovak Republic'] = 'Slovakia'
country_asr$location[country_asr$location == 'Swiss Confederation'] = 'Switzerland'
country_asr$location[country_asr$location == 'Hellenic Republic'] = 'Greece'
country_asr$location[country_asr$location == 'Commonwealth of Dominica'] = 'Dominica'
country_asr$location[country_asr$location == 'Eastern Uruguay'] = 'Uruguay'
country_asr$location[country_asr$location == 'Portuguese Republic'] = 'Portugal'
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'United Great Britain and Northern Ireland'] = 'UK'
country_asr$location[country_asr$location == 'Bolivarian Venezuela'] = 'Venezuela'
country_asr$location[country_asr$location == 'Commonwealth of Bahamas'] = 'Bahamas'
country_asr$location[country_asr$location == 'State of Kuwait'] = 'Kuwait'
country_asr$location[country_asr$location == 'United States Virgin Islands'] = 'Virgin Islands'
country_asr$location[country_asr$location == 'Togolese Republic'] = 'Togo'
country_asr$location[country_asr$location == 'Democratic Sao Tome and Principe'] = 'Sao Tome and Principe'
country_asr$location[country_asr$location == 'Federal Nigeria'] = 'Nigeria'
country_asr$location[country_asr$location == 'Islamic Mauritania'] = 'Mauritania'
country_asr$location[country_asr$location == 'Federal Somalia'] = 'Somalia'
country_asr$location[country_asr$location == 'United Tanzania'] = 'Tanzania'
country_asr$location[country_asr$location == 'Federal Democratic Ethiopia'] = 'Ethiopia'
country_asr$location[country_asr$location == 'State of Eritrea'] = 'Eritrea'
country_asr$location[country_asr$location == 'Democratic Congo'] = 'Democratic Republic of the Congo'
country_asr$location[country_asr$location == 'Union of Comoros'] = 'Comoros'
country_asr$location[country_asr$location == 'Gabonese Republic'] = 'Gabon'
country_asr$location[country_asr$location == 'Islamic Pakistan'] = 'Pakistan'
country_asr$location[country_asr$location == 'Islamic Afghanistan'] = 'Afghanistan'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == 'Federal Democratic Nepal'] = 'Nepal'
country_asr$location[country_asr$location == 'Arab Egypt'] = 'Egypt'
country_asr$location[country_asr$location == 'Sultanate of Oman'] = 'Oman'
country_asr$location[country_asr$location == 'State of Qatar'] = 'Qatar'
country_asr$location[country_asr$location == 'Lebanese Republic'] = 'Lebanon'
country_asr$location[country_asr$location == 'United Mexican States'] = 'Mexico'
country_asr$location[country_asr$location == 'State of Libya'] = 'Libya'
country_asr$location[country_asr$location == 'Islamic Iran'] = 'Iran'
country_asr$location[country_asr$location == 'Federative Brazil'] = 'Brazil'
country_asr$location[country_asr$location == 'Plurinational State of Bolivia'] = 'Bolivia'
country_asr$location[country_asr$location == 'Hashemite Jordan'] = 'Jordan'
country_asr$location[country_asr$location == 'Principality of Monaco'] = 'Monaco'
country_asr$location[country_asr$location == "People's Bangladesh"] = 'Bangladesh'
country_asr$location[country_asr$location == "People's Democratic Algeria"] = 'Algeria'

country_asr$location[country_asr$location == 'Syrian Arab Republic'] = 'Syria'
country_asr$location[country_asr$location == 'Eswatini'] = 'Swaziland'
country_asr$location[country_asr$location == 'Cabo Verde'] = 'Cape Verde'
country_asr$location[country_asr$location == "Côte d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == 'Tokelau'] = 'New Zealand'
country_asr$location[country_asr$location == ''] = ''

country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"][2] = 'Tobago'

country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigua'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigua",])
country_asr$location[country_asr$location == "Antigua"][2] = 'Barbuda'

country_asr$location[country_asr$location == "Saint Vincent and Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"][2] = 'Grenadines'

country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"][2] = 'Nevis'

worldData <- map_data('world')
total <- full_join(worldData,country_asr,by = c('region'='location'))

p <- ggplot()
total <- total %>% mutate(val2 = cut(val, breaks = c(0,0.1,0.2,0.3,1,5,10,100),
                                     labels = c("0~0.1","0.1~0.2","0.2~0.3","0.3~1.0","1.0~5.0", "5.0~10.0","10.0+"),  ### breaks需要根据自己的实际结果来调整
                                     include.lowest = T,right = T))#Deaths
total <- total %>% mutate(val2 = cut(val, breaks = c(0,0.5,1,2,5,10,20,100),
                                     labels = c("0~0.5","0.5~1.0","1.0~2.0","2.0~5.0","5.0~10.0", "10.0~20.0","20.0+"),  ### breaks需要根据自己的实际结果来调整
                                     include.lowest = T,right = T))#Incidence
quantile(country_asr$val)
p2 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group = group,fill=val2),
                       colour="black",size = .2) + 
  scale_fill_brewer(palette = "Reds")+
  theme_void()+labs(x="", y="")+
  guides(fill = guide_legend(title='ASR(/10^5)'))+
  theme(legend.position = 'right')
p2
library(eoffice)
topptx(p2,"ASR_Deaths.pptx")

install.packages("officer")
install.packages("rvg")
install.packages("openxlsx")
install.packages("ggplot2")
install.packages("flextable")
install.packages("xtable")
install.packages("rgl")
install.packages("stargazer")
install.packages("tikzDevice")
install.packages("xml2")
install.packages("broom")
install.packages("devtools")
library(devtools)
devtools::install_github("tomwenseleers/export")
library(export)

graph2ppt(p2, file = "ASR_Incidence.pptx", width = 4, height = 4)


### case change
####   case change MAP
case_2021 <- subset(EC,EC$year==2021 & 
                      EC$age=='All ages' & 
                      EC$metric== 'Number' &
                      EC$measure=='Deaths') ## 获取2021年EC发病数
case_1990 <- subset(EC,EC$year==1990 & 
                      EC$age=='All ages' & 
                      EC$metric== 'Number' &
                      EC$measure=='Deaths') ## 获取1990年EC发病数
case_1990 <- case_1990[,c(3,9)]
case_2021 <- case_2021[,c(3,9)]
names(case_1990) <- c('location','case_1990')
names(case_2021) <- c('location','case_2021')
country_asr <- merge(case_1990, case_2021, by='location')
country_asr$val <- (country_asr$case_2021-country_asr$case_1990)/country_asr$case_1990*100  ### 获取我们的结果

worldData <- map_data('world')
country_asr$location <- as.character(country_asr$location) 

country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == 'Democratic Socialist Sri Lanka'] = 'Sri Lanka'
country_asr$location[country_asr$location == 'Independent State of Papua New Guinea'] = 'Papua New Guinea'
country_asr$location[country_asr$location == 'Democratic Timor-Leste'] = 'Timor-Leste'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == 'Kyrgyz Republic'] = 'Kyrgyzstan'
country_asr$location[country_asr$location == "Democratic People's Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Korea"] = 'South Korea'
country_asr$location[country_asr$location == 'Federated States of Micronesia'] = 'Micronesia'
country_asr$location[country_asr$location == 'Independent State of Samoa'] = 'Samoa'
country_asr$location[country_asr$location == 'Socialist Viet Nam'] = 'Vietnam'
country_asr$location[country_asr$location == "People's China"] = 'China'
country_asr$location[country_asr$location == 'Union of Myanmar'] = 'Myanmar'
country_asr$location[country_asr$location == 'Principality of Andorra'] = 'Andorra'
country_asr$location[country_asr$location == 'Federal Germany'] = 'Germany'
country_asr$location[country_asr$location == 'Argentine Republic'] = 'Argentina'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'Brunei Darussalam'] = 'Brunei'
country_asr$location[country_asr$location == 'French Republic'] = 'France'
country_asr$location[country_asr$location == 'State of Israel'] = 'Israel'
country_asr$location[country_asr$location == 'Grand Duchy of Luxembourg'] = 'Luxembourg'
country_asr$location[country_asr$location == 'Slovak Republic'] = 'Slovakia'
country_asr$location[country_asr$location == 'Swiss Confederation'] = 'Switzerland'
country_asr$location[country_asr$location == 'Hellenic Republic'] = 'Greece'
country_asr$location[country_asr$location == 'Commonwealth of Dominica'] = 'Dominica'
country_asr$location[country_asr$location == 'Eastern Uruguay'] = 'Uruguay'
country_asr$location[country_asr$location == 'Portuguese Republic'] = 'Portugal'
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'United Great Britain and Northern Ireland'] = 'UK'
country_asr$location[country_asr$location == 'Bolivarian Venezuela'] = 'Venezuela'
country_asr$location[country_asr$location == 'Commonwealth of Bahamas'] = 'Bahamas'
country_asr$location[country_asr$location == 'State of Kuwait'] = 'Kuwait'
country_asr$location[country_asr$location == 'United States Virgin Islands'] = 'Virgin Islands'
country_asr$location[country_asr$location == 'Togolese Republic'] = 'Togo'
country_asr$location[country_asr$location == 'Democratic Sao Tome and Principe'] = 'Sao Tome and Principe'
country_asr$location[country_asr$location == 'Federal Nigeria'] = 'Nigeria'
country_asr$location[country_asr$location == 'Islamic Mauritania'] = 'Mauritania'
country_asr$location[country_asr$location == 'Federal Somalia'] = 'Somalia'
country_asr$location[country_asr$location == 'United Tanzania'] = 'Tanzania'
country_asr$location[country_asr$location == 'Federal Democratic Ethiopia'] = 'Ethiopia'
country_asr$location[country_asr$location == 'State of Eritrea'] = 'Eritrea'
country_asr$location[country_asr$location == 'Democratic Congo'] = 'Democratic Republic of the Congo'
country_asr$location[country_asr$location == 'Union of Comoros'] = 'Comoros'
country_asr$location[country_asr$location == 'Gabonese Republic'] = 'Gabon'
country_asr$location[country_asr$location == 'Islamic Pakistan'] = 'Pakistan'
country_asr$location[country_asr$location == 'Islamic Afghanistan'] = 'Afghanistan'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == 'Federal Democratic Nepal'] = 'Nepal'
country_asr$location[country_asr$location == 'Arab Egypt'] = 'Egypt'
country_asr$location[country_asr$location == 'Sultanate of Oman'] = 'Oman'
country_asr$location[country_asr$location == 'State of Qatar'] = 'Qatar'
country_asr$location[country_asr$location == 'Lebanese Republic'] = 'Lebanon'
country_asr$location[country_asr$location == 'United Mexican States'] = 'Mexico'
country_asr$location[country_asr$location == 'State of Libya'] = 'Libya'
country_asr$location[country_asr$location == 'Islamic Iran'] = 'Iran'
country_asr$location[country_asr$location == 'Federative Brazil'] = 'Brazil'
country_asr$location[country_asr$location == 'Plurinational State of Bolivia'] = 'Bolivia'
country_asr$location[country_asr$location == 'Hashemite Jordan'] = 'Jordan'
country_asr$location[country_asr$location == 'Principality of Monaco'] = 'Monaco'
country_asr$location[country_asr$location == "People's Bangladesh"] = 'Bangladesh'
country_asr$location[country_asr$location == "People's Democratic Algeria"] = 'Algeria'

country_asr$location[country_asr$location == 'Syrian Arab Republic'] = 'Syria'
country_asr$location[country_asr$location == 'Eswatini'] = 'Swaziland'
country_asr$location[country_asr$location == 'Cabo Verde'] = 'Cape Verde'
country_asr$location[country_asr$location == "Côte d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == 'Tokelau'] = 'New Zealand'
country_asr$location[country_asr$location == ''] = ''

country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"][2] = 'Tobago'

country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigua'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigua",])
country_asr$location[country_asr$location == "Antigua"][2] = 'Barbuda'

country_asr$location[country_asr$location == "Saint Vincent and Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"][2] = 'Grenadines'

country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"][2] = 'Nevis'


total <- full_join(worldData,country_asr,by = c('region'='location'))
quantile(country_asr$val)
p <- ggplot()

total <- total %>% mutate(val2 = cut(val, breaks = c(-100,-50,0,50,100,300,600,3100),
                                     labels = c("100% to 50% decrease","<50% decrease","<50% increase",
                                                "50% to 100% increase","100% to 300% increase", 
                                                "300% to 600% increase", ">600% increase"),
                                     include.lowest = T,right = T))

p2 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group = group,fill=val2),
                       colour="black",size = .2) + 
  scale_fill_manual(values = c("#006400", "#66CD00", "#FFE4C4", "#FF7256", "#FF4040", "#CD3333", "#8B2323"))+
  theme_void()+labs(x="", y="")+
  guides(fill = guide_legend(title='Change in cancer cases'))+
  theme(legend.position = 'right')
p2
library(eoffice)
topptx(p2,"change in cacer cases_Deaths.pptx")
Deaths_val<-country_asr$val
write.csv(Deaths_val,"change in cacer cases_Deaths_val.csv")

## map for EAPC
## EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Deaths')

EAPC <- EAPC[,c(3,8,9)]

country <- case_2021$location  ###获取国家名称
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
country_asr <- EAPC_cal

country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == 'Democratic Socialist Sri Lanka'] = 'Sri Lanka'
country_asr$location[country_asr$location == 'Independent State of Papua New Guinea'] = 'Papua New Guinea'
country_asr$location[country_asr$location == 'Democratic Timor-Leste'] = 'Timor-Leste'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == 'Kyrgyz Republic'] = 'Kyrgyzstan'
country_asr$location[country_asr$location == "Democratic People's Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Korea"] = 'South Korea'
country_asr$location[country_asr$location == 'Federated States of Micronesia'] = 'Micronesia'
country_asr$location[country_asr$location == 'Independent State of Samoa'] = 'Samoa'
country_asr$location[country_asr$location == 'Socialist Viet Nam'] = 'Vietnam'
country_asr$location[country_asr$location == "People's China"] = 'China'
country_asr$location[country_asr$location == 'Union of Myanmar'] = 'Myanmar'
country_asr$location[country_asr$location == 'Principality of Andorra'] = 'Andorra'
country_asr$location[country_asr$location == 'Federal Germany'] = 'Germany'
country_asr$location[country_asr$location == 'Argentine Republic'] = 'Argentina'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'Brunei Darussalam'] = 'Brunei'
country_asr$location[country_asr$location == 'French Republic'] = 'France'
country_asr$location[country_asr$location == 'State of Israel'] = 'Israel'
country_asr$location[country_asr$location == 'Grand Duchy of Luxembourg'] = 'Luxembourg'
country_asr$location[country_asr$location == 'Slovak Republic'] = 'Slovakia'
country_asr$location[country_asr$location == 'Swiss Confederation'] = 'Switzerland'
country_asr$location[country_asr$location == 'Hellenic Republic'] = 'Greece'
country_asr$location[country_asr$location == 'Commonwealth of Dominica'] = 'Dominica'
country_asr$location[country_asr$location == 'Eastern Uruguay'] = 'Uruguay'
country_asr$location[country_asr$location == 'Portuguese Republic'] = 'Portugal'
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'United Great Britain and Northern Ireland'] = 'UK'
country_asr$location[country_asr$location == 'Bolivarian Venezuela'] = 'Venezuela'
country_asr$location[country_asr$location == 'Commonwealth of Bahamas'] = 'Bahamas'
country_asr$location[country_asr$location == 'State of Kuwait'] = 'Kuwait'
country_asr$location[country_asr$location == 'United States Virgin Islands'] = 'Virgin Islands'
country_asr$location[country_asr$location == 'Togolese Republic'] = 'Togo'
country_asr$location[country_asr$location == 'Democratic Sao Tome and Principe'] = 'Sao Tome and Principe'
country_asr$location[country_asr$location == 'Federal Nigeria'] = 'Nigeria'
country_asr$location[country_asr$location == 'Islamic Mauritania'] = 'Mauritania'
country_asr$location[country_asr$location == 'Federal Somalia'] = 'Somalia'
country_asr$location[country_asr$location == 'United Tanzania'] = 'Tanzania'
country_asr$location[country_asr$location == 'Federal Democratic Ethiopia'] = 'Ethiopia'
country_asr$location[country_asr$location == 'State of Eritrea'] = 'Eritrea'
country_asr$location[country_asr$location == 'Democratic Congo'] = 'Democratic Republic of the Congo'
country_asr$location[country_asr$location == 'Union of Comoros'] = 'Comoros'
country_asr$location[country_asr$location == 'Gabonese Republic'] = 'Gabon'
country_asr$location[country_asr$location == 'Islamic Pakistan'] = 'Pakistan'
country_asr$location[country_asr$location == 'Islamic Afghanistan'] = 'Afghanistan'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == 'Federal Democratic Nepal'] = 'Nepal'
country_asr$location[country_asr$location == 'Arab Egypt'] = 'Egypt'
country_asr$location[country_asr$location == 'Sultanate of Oman'] = 'Oman'
country_asr$location[country_asr$location == 'State of Qatar'] = 'Qatar'
country_asr$location[country_asr$location == 'Lebanese Republic'] = 'Lebanon'
country_asr$location[country_asr$location == 'United Mexican States'] = 'Mexico'
country_asr$location[country_asr$location == 'State of Libya'] = 'Libya'
country_asr$location[country_asr$location == 'Islamic Iran'] = 'Iran'
country_asr$location[country_asr$location == 'Federative Brazil'] = 'Brazil'
country_asr$location[country_asr$location == 'Plurinational State of Bolivia'] = 'Bolivia'
country_asr$location[country_asr$location == 'Hashemite Jordan'] = 'Jordan'
country_asr$location[country_asr$location == 'Principality of Monaco'] = 'Monaco'
country_asr$location[country_asr$location == "People's Bangladesh"] = 'Bangladesh'
country_asr$location[country_asr$location == "People's Democratic Algeria"] = 'Algeria'

country_asr$location[country_asr$location == 'Syrian Arab Republic'] = 'Syria'
country_asr$location[country_asr$location == 'Eswatini'] = 'Swaziland'
country_asr$location[country_asr$location == 'Cabo Verde'] = 'Cape Verde'
country_asr$location[country_asr$location == "Côte d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == 'Tokelau'] = 'New Zealand'
country_asr$location[country_asr$location == ''] = ''

country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"][2] = 'Tobago'

country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigua'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigua",])
country_asr$location[country_asr$location == "Antigua"][2] = 'Barbuda'

country_asr$location[country_asr$location == "Saint Vincent and Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"][2] = 'Grenadines'

country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"][2] = 'Nevis'


worldData <- map_data('world')
total <- full_join(worldData,country_asr,by = c('region'='location'),relationship="many-to-many")



p <- ggplot()
p1 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group = group,fill=EAPC),
                       colour="black",size = .2) + 
  scale_fill_gradient2(low = "forestgreen",mid = 'white', high = "red",
                       midpoint = 0)+
  theme_void()+labs(x="", y="")+
  guides(fill = guide_colorbar(title='EAPC'))+
  theme(legend.position = 'right') 
p1
library(eoffice)
topptx(p1,"EPAC_Deaths.pptx")
