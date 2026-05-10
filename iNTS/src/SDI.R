setwd('G:/11') ##设置工作路径
library(reshape)
library(ggplot2)
library(ggrepel)

EC <- read.csv('region.csv',header = T)  ## 读取我们的数据
order_SDI <- read.csv('order.csv',header = F)
SDI <- read.csv('GBD_SDI_1990-2019.csv',header = T)

## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式
SDI <- melt(SDI,id='Location')
SDI$variable <- as.numeric(gsub('\\X',replacement = '', SDI$variable))
names(SDI) <- c('location','year','SDI')


### 获取EC的1990-2019的标准发病率数据
EC <- subset(EC, EC$age=='Age-standardized' & 
               EC$metric== 'Rate' &
               EC$measure=='Deaths')
EC <- EC[,c(2,7,8)]
names(EC)[3] <- 'ASR'

### 合并SDI与ASR数据
EC_ASR_SDI <- merge(EC,SDI,by=c('location','year'))

EC_ASR_SDI$location <- factor(EC_ASR_SDI$location, 
                              levels=order_SDI$V1, 
                              ordered=TRUE) ## location图例按照我们的顺序排列

##开始作图，主变量为ASR以及SDI,图形的颜色和形状根据不同区域来调整即可
### 同时以所有数据画出拟合曲线
p<-ggplot(EC_ASR_SDI, aes(SDI,ASR)) + geom_point(aes(color = location, shape= location))+
  scale_shape_manual(values = 1:22) + 
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) 
p
library(eoffice)
topptx(p,"SDI_Deaths.pptx")

#### figure 4B
EC <- read.csv('nation.csv',header = T)  ## 读取我们的数据
SDI <- read.csv('GBD_SDI_1990-2019.csv',header = T)

## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式，
SDI <- melt(SDI,id='Location')
SDI$variable <- as.numeric(gsub('\\X',replacement = '', SDI$variable))
names(SDI) <- c('location','year','SDI')
SDI <- SDI[SDI$year== 2019,] ###只取2019年的数据

### 获取EC的2019的标准发病率数据
EC <- subset(EC, EC$age=='Age-standardized' & 
               EC$metric== 'Rate' &
               EC$measure=='Deaths' &
               EC$year=='2019')
EC <- EC[,c(3,8,9)]
names(EC)[3] <- 'ASR'

### 调整SDI与EC里location命名一致
EC$location[EC$location == "Taiwan (Province of China)"] = 'Taiwan'
EC$location[EC$location == 'Democratic Socialist Sri Lanka'] = 'Sri Lanka'
EC$location[EC$location == 'Independent State of Papua New Guinea'] = 'Papua New Guinea'
EC$location[EC$location == 'Democratic Timor-Leste'] = 'Timor-Leste'
EC$location[EC$location == "Lao People's Democratic Republic"] = 'Laos'
EC$location[EC$location == 'Kyrgyz Republic'] = 'Kyrgyzstan'
EC$location[EC$location == "Democratic People's Korea"] = 'North Korea'
EC$location[EC$location == "Korea"] = 'South Korea'
EC$location[EC$location == 'Federated States of Micronesia'] = 'Micronesia'
EC$location[EC$location == 'Independent State of Samoa'] = 'Samoa'
EC$location[EC$location == 'Socialist Viet Nam'] = 'Vietnam'
EC$location[EC$location == "People's China"] = 'China'
EC$location[EC$location == 'Union of Myanmar'] = 'Myanmar'
EC$location[EC$location == 'Principality of Andorra'] = 'Andorra'
EC$location[EC$location == 'Federal Germany'] = 'Germany'
EC$location[EC$location == 'Argentine Republic'] = 'Argentina'
EC$location[EC$location == 'Russian Federation'] = 'Russia'
EC$location[EC$location == 'Brunei Darussalam'] = 'Brunei'
EC$location[EC$location == 'French Republic'] = 'France'
EC$location[EC$location == 'State of Israel'] = 'Israel'
EC$location[EC$location == 'Grand Duchy of Luxembourg'] = 'Luxembourg'
EC$location[EC$location == 'Slovak Republic'] = 'Slovakia'
EC$location[EC$location == 'Swiss Confederation'] = 'Switzerland'
EC$location[EC$location == 'Hellenic Republic'] = 'Greece'
EC$location[EC$location == 'Commonwealth of Dominica'] = 'Dominica'
EC$location[EC$location == 'Eastern Uruguay'] = 'Uruguay'
EC$location[EC$location == 'Portuguese Republic'] = 'Portugal'
EC$location[EC$location == 'United States of America'] = 'USA'
EC$location[EC$location == 'United Great Britain and Northern Ireland'] = 'UK'
EC$location[EC$location == 'Bolivarian Venezuela'] = 'Venezuela'
EC$location[EC$location == 'Commonwealth of Bahamas'] = 'Bahamas'
EC$location[EC$location == 'State of Kuwait'] = 'Kuwait'
EC$location[EC$location == 'United States Virgin Islands'] = 'Virgin Islands'
EC$location[EC$location == 'Togolese Republic'] = 'Togo'
EC$location[EC$location == 'Democratic Sao Tome and Principe'] = 'Sao Tome and Principe'
EC$location[EC$location == 'Federal Nigeria'] = 'Nigeria'
EC$location[EC$location == 'Islamic Mauritania'] = 'Mauritania'
EC$location[EC$location == 'Federal Somalia'] = 'Somalia'
EC$location[EC$location == 'United Tanzania'] = 'Tanzania'
EC$location[EC$location == 'Federal Democratic Ethiopia'] = 'Ethiopia'
EC$location[EC$location == 'State of Eritrea'] = 'Eritrea'
EC$location[EC$location == 'Democratic Congo'] = 'Democratic Republic of the Congo'
EC$location[EC$location == 'Union of Comoros'] = 'Comoros'
EC$location[EC$location == 'Gabonese Republic'] = 'Gabon'
EC$location[EC$location == 'Islamic Pakistan'] = 'Pakistan'
EC$location[EC$location == 'Islamic Afghanistan'] = 'Afghanistan'
EC$location[EC$location == 'Congo'] = 'Republic of Congo'
EC$location[EC$location == 'Federal Democratic Nepal'] = 'Nepal'
EC$location[EC$location == 'Arab Egypt'] = 'Egypt'
EC$location[EC$location == 'Sultanate of Oman'] = 'Oman'
EC$location[EC$location == 'State of Qatar'] = 'Qatar'
EC$location[EC$location == 'Lebanese Republic'] = 'Lebanon'
EC$location[EC$location == 'United Mexican States'] = 'Mexico'
EC$location[EC$location == 'State of Libya'] = 'Libya'
EC$location[EC$location == 'Islamic Iran'] = 'Iran'
EC$location[EC$location == 'Federative Brazil'] = 'Brazil'
EC$location[EC$location == 'Plurinational State of Bolivia'] = 'Bolivia'
EC$location[EC$location == 'Hashemite Jordan'] = 'Jordan'
EC$location[EC$location == 'Principality of Monaco'] = 'Monaco'
EC$location[EC$location == "People's Bangladesh"] = 'Bangladesh'
EC$location[EC$location == "People's Democratic Algeria"] = 'Algeria'

EC$location[EC$location == 'Syrian Arab Republic'] = 'Syria'
EC$location[EC$location == 'Eswatini'] = 'Swaziland'
EC$location[EC$location == 'Cabo Verde'] = 'Cape Verde'
EC$location[EC$location == "Côte d'Ivoire"] = 'Ivory Coast'
EC$location[EC$location == 'Tokelau'] = 'New Zealand'
EC$location[EC$location == ''] = ''

EC$location[EC$location == "Trinidad and Tobago"] = 'Trinidad'
EC <- rbind(EC,EC[EC$location == "Trinidad",])
EC$location[EC$location == "Trinidad"][2] = 'Tobago'

EC$location[EC$location == "Antigua and Barbuda"] = 'Antigua'
EC <- rbind(EC,EC[EC$location == "Antigua",])
EC$location[EC$location == "Antigua"][2] = 'Barbuda'

EC$location[EC$location == "Saint Vincent and Grenadines"] = 'Saint Vincent'
EC <- rbind(EC,EC[EC$location == "Saint Vincent",])
EC$location[EC$location == "Saint Vincent"][2] = 'Grenadines'

EC$location[EC$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
EC <- rbind(EC,EC[EC$location == "Saint Kitts",])
EC$location[EC$location == "Saint Kitts"][2] = 'Nevis'

###合并两者数据
EC_ASR_SDI_2019 <- merge(EC,SDI,by=c('location','year'))

##作图
p<-ggplot(EC_ASR_SDI_2019, aes(SDI,ASR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  theme(legend.position="none")
p
library(eoffice)
topptx(p,"ASR~SDI_Deaths.pptx")
