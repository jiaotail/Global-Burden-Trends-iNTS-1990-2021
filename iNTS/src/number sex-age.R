# 加载ggplot2包，用于数据可视化
library(ggplot2)
# 加载reshape2包，用于数据重塑
library(reshape2)
# 加载dplyr包，用于数据操作
library(dplyr)
# 注释掉的代码，用于安装readxl包，但这里没有执行安装
#install.packages('readxl')
# 加载readxl包，用于读取Excel文件
library('readxl')

# 设置工作目录到指定路径
setwd("G:/11")

# 读取CSV文件中的数据
IBD_ints <- read.csv('sex-age.csv',header = T)
# 查看metric_name列的结构
str(IBD_ints$metric_name)
# 统计metric_name列的频数
table(IBD_ints$metric_name)
# 统计measure_name列的频数
table(IBD_ints$measure_name)
# 获取age_name列的唯一值
unique(IBD_ints$age_name)

#### 第一步 不同年龄段患病人数金字塔 ####
# 定义年龄分组，每5岁一个组
age1 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84 years","85-89 years","90-94 years","95+ years") ###20个年龄组

# 提取2021年的患病人数数据
data1<- subset(IBD_ints,IBD_ints$year==2021 &
                 (IBD_ints$age_name%in% age1 ) &
                 IBD_ints$sex_name!="Both"&
                 IBD_ints$location_name=='Global'&
                 IBD_ints$metric_name=='Number'&
                 IBD_ints$measure_name=='Incidence')

# 查看data1的结构
str(data1)
# 提取分析需要的列
data1<-data1[,c("sex_name","age_name","val","upper","lower")]

# 处理年龄数据，去掉" years"文字
data1$age_name<-gsub(" years","",data1$age_name)

# 因子化年龄，并按年龄排序
data1$age_name<- factor(data1$age_name, levels = c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                                    "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+"))

# 按照性别和年龄排序
data1 <- data1[order(data1$sex_name, data1$age_name),]

# 提取整数部分
data1$val<-round(data1$val,0)

# 将性别转换为因子
data1$Sex<-as.factor(data1$sex_name)

# 设置颜色
custom_colors <- c("Male"="steelblue","Female"="#e31a1c")

# 创建患病人数金字塔图
p1<-ggplot(data1, aes(x = factor(age_name,levels = unique(age_name)),
                      y = ifelse(Sex =="Male", val, -val),
                      fill = Sex)) +
  scale_fill_manual(values = custom_colors) +
  geom_bar(stat='identity')+ 
  coord_flip()+
  labs(x ='Age', y ='The Numbers of Incidence ')+ 
  geom_text(
    aes(label=val,# 显示数值
        hjust = ifelse(Sex =="Male", -0.4, 1.1))# 数值的位置
  ) +
  scale_y_continuous(
    labels = abs, 
    expand = expansion(mult = c(0.2, 0.2)))

# 显示图表p1
p1
#library(eoffice)
topptx(p1,"Numbers of Global Incidence.pptx")
#### 第一步 不同年龄段发病人数金字塔 ####
# 提取2021年的发病人数数据
data1<- subset(IBD_ints,IBD_ints$year==2021&
                 (IBD_ints$age_name%in% age1 ) &
                 IBD_ints$sex_name!="Both"&
                 IBD_ints$location_name=='High SDI'&
                 IBD_ints$metric_name=='Number'&
                 IBD_ints$measure_name=='Incidence')

