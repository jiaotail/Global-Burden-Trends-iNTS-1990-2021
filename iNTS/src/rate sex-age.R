####年龄段折线图####
library(ggplot2)# 加载ggplot2包，用于数据可视化
library(reshape2)# 加载reshape2包，用于数据重塑
library(dplyr)# 加载dplyr包，用于数据操作
library(readxl)# 加载readxl包，用于读取Excel文件

#设置工作空间，读取数据
setwd("G:/11")# 设置工作目录到指定路径

#读取数据
IBD_ints <- read.csv('sex-age.csv',header = T)# 读取CSV文件到IBD_ints数据框
str(IBD_ints$metric_name)# 查看metric_name列的结构
table(IBD_ints$metric_name)# 统计metric_name列的频数
table(IBD_ints$measure_name)# 统计measure_name列的频数

####第一步 不同年龄段患病率折线图####
#分组年龄，一般为5岁一个年龄组
age1 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")###20个年龄组

#提取患病率的数据
data1<- subset(IBD_ints,IBD_ints$year==2021 &#提取2021年
                 (IBD_ints$age_name%in% age1 ) &#提取不同年龄段
                 IBD_ints$sex_name!="Both"&#把男女分开
                 IBD_ints$location_name=='Global'&#提取中国数据
                 IBD_ints$metric_name=='Rate'&#提取10人患病人数
                 IBD_ints$measure_name=='Incidence')#提取患病率数据
str(data1)# 查看data1的结构
#提取分析的数据
data1<-data1[,c("sex_name","age_name","val","upper","lower")]# 选择需要的列

#年龄数据的处理，替代year 文字
data1$age_name<-gsub(" years","",data1$age_name)# 移除age_name中的" years"

#因子化,按年龄排序
data1$age_name<- factor(data1$age_name, levels = c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                                   "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+"))

# 按照 sex_name 和 age_name 进行排序
data1 <- data1[order(data1$sex_name, data1$age_name),]

#取小2位数点
data1$val<-round(data1$val,2)# 将val列四舍五入到小数点后两位

#性别进行因子化
data1$Sex<-as.factor(data1$sex_name)# 将sex_name列转换为因子

custom_colors <- c("Female"="steelblue","Male"="#e31a1c")
#画不同年龄段折线图
p3 <- ggplot(data = data1,aes(x=data1$age_name,y=data1$val,color=Sex, group = Sex))+
  geom_line(linewidth = 1.2) +
  geom_point(size = 2 , fill = "white")+
  geom_ribbon(aes(ymin = data1$lower, ymax = data1$upper),  # 添加置信区间
              alpha = 0.1, color = NA)+
  scale_color_manual(values = custom_colors) +
  labs(x ='Age', y ='The Rate of Deaths ')+
  scale_y_continuous(
    labels = abs, 
    expand = expansion(mult = c(0.2, 0.2)))
p3
#library(eoffice)
topptx(p3,"Rate of Global Deaths.pptx")
data1<- subset(IBD_ints,IBD_ints$year==2021 &#提取2021年
                 (IBD_ints$age_name%in% age1 ) &#提取不同年龄段
                 IBD_ints$sex_name!="Both"&#把男女分开
                 IBD_ints$location_name=='Global'&#提取中国数据
                 IBD_ints$metric_name=='Rate'&#提取10人患病人数
                 IBD_ints$measure_name=='Deaths')#提取患病率数据


