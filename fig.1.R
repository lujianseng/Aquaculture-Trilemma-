rm(list=ls())
fig1a<-read.csv("C:/Users/张宇洋/Desktop/fig1.a.csv")
str(fig1a)
library(reshape2)
long_data <- melt(fig1a, id.vars = "年份",
                  measure.vars = c("生产力", "生产力.1"),
                  variable.name = "池塘类型",
                  value.name = "生产力")
long_data$池塘类型 <- ifelse(long_data$池塘类型 == "生产力", "Mariculture", "Inland aquaculture")
library(ggplot2)
p1<-ggplot(data=long_data,aes(x=年份,y=生产力,color=池塘类型,fill=池塘类型))+
  geom_point(size=3)+geom_smooth(method="lm",linetype=2)+theme_classic()+xlim(2003,2021)+
  scale_color_manual(values = c("Mariculture" = "blue", "Inland aquaculture" = "red"),
                     name = "type") +
  scale_fill_manual(values = c("Mariculture" = "blue", "Inland aquaculture" = "red"),
                    name = "type")+
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))+xlab("Years")+ylab("Pond Productivity (t/ha)")

p1
fig1b<-read.csv("C:/Users/张宇洋/Desktop/fig1.b.csv")
str(fig1b)
library(reshape2)
long_datab <- melt(fig1b, id.vars = "年份",
                  measure.vars = c("比率", "比率.1"),
                  variable.name = "池塘类型",
                  value.name = "比率")
long_datab$池塘类型 <- ifelse(long_datab$池塘类型 == "比率", "Inland aquaculture", "Mariculture")
library(ggplot2)
p2<-ggplot(data=long_datab,aes(x=年份,y=比率,color=池塘类型,fill=池塘类型))+
  geom_point(size=3)+geom_smooth(method="lm",linetype=2)+theme_classic()+xlim(2003,2021)+
  scale_color_manual(values = c("Mariculture" = "blue", "Inland aquaculture" = "red"),
                     name = "type") +
  scale_fill_manual(values = c("Mariculture" = "blue", "Inland aquaculture" = "red"),
                    name = "type")+
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))+xlab("Years")+ylab("RAS ratios (%)")

p2




############################################################################
library(reshape2)
library(tidyverse)
my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2)]
pie(rep(1,8), col=sample(my_pal, 8))

fig1c <- data.frame(
    能耗 = c("平均", "标准差"),
    Semi_intensive = c(9.389, 0.6482),
    Intensive = c(17.998, 1.3469),
    RAS = c(67.961, 15.779)
  )


fig1c

data_transposed <- t(fig1c)
colnames(data_transposed) <- data_transposed[1, ]

# 去掉第一行，它已经被用作列名
data_transposed <- data_transposed[-1, ]

# 将数据转换为数据框
fig1c_t <- as.data.frame(data_transposed, stringsAsFactors = FALSE)
fig1c_t$index <- rownames(fig1c_t)
fig1c_t$平均 <- as.numeric(fig1c_t$平均)
fig1c_t$标准差 <- as.numeric(fig1c_t$标准差)
fig1c_t$index<-factor(fig1c_t$index,levels=c("Semi_intensive", "Intensive","RAS" ))
####barplot
p3<-ggplot(fig1c_t, aes(x = index, y = 平均, fill = index)) +
  geom_col(position = "dodge", width = 0.8)+ylim(0,85) +
  scale_fill_manual(values = my_pal, guide = "none")+
  geom_errorbar(aes(ymin = 平均-标准差, ymax = 平均+标准差), width = 0.25,linewidth=1)+
  theme_classic()+
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))+xlab("")+ylab("Energy use (MJ/Kg)")


p3
###################################
fig1d <- data.frame(
  TN = c("均值", "标准差"),
  FP = c(1.0386, 0.3299),
  SP = c(1.5893, 1.3845),
  FRAS = c(7.0619, 1.4217),
  SRAS = c(8.1665, 4.2948)
)

fig1d

data_transposed <- t(fig1d)
colnames(data_transposed) <- data_transposed[1, ]

# 去掉第一行，它已经被用作列名
data_transposed <- data_transposed[-1, ]

# 将数据转换为数据框
fig1d_t <- as.data.frame(data_transposed, stringsAsFactors = FALSE)
fig1d_t$index <- rownames(fig1d_t)
fig1d_t$均值 <- as.numeric(fig1d_t$均值)
fig1d_t$标准差 <- as.numeric(fig1d_t$标准差)
fig1d_t$index<-factor(fig1d_t$index,levels=c("FP","SP", "FRAS", "SRAS"))
####barplot
p4<-ggplot(fig1d_t, aes(x = index, y = 均值, fill = index)) +
   geom_col(position = "dodge", width = 0.8)+ylim(0,13) +
  scale_fill_manual(values = my_pal, guide = "none")+
  geom_errorbar(aes(ymin = 均值-标准差, ymax = 均值+标准差), width = 0.25,linewidth=1)+
  theme_classic()+
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))+xlab("")+ylab("TN discharge (g/Kg)")
p4
###########################################
TP <- data.frame(
  TP = c("均值", "标准差"),
  FP = c(0.0842, 0.0268),
  SP = c(0.2534, 0.1318),
  FRAS = c(0.7499, 0.151),
  SRAS = c(1.6338, 0.1536)
)

TP

data_transposed <- t(TP)
colnames(data_transposed) <- data_transposed[1, ]

# 去掉第一行，它已经被用作列名
data_transposed <- data_transposed[-1, ]

# 将数据转换为数据框
fig1e <- as.data.frame(data_transposed, stringsAsFactors = FALSE)
fig1e$index <- rownames(fig1e)
fig1e$均值 <- as.numeric(fig1e$均值)
fig1e$标准差 <- as.numeric(fig1e$标准差)
fig1e$index<-factor(fig1e$index,levels=c("FP","SP", "FRAS", "SRAS"))
####barplot
p5<-ggplot(fig1e, aes(x = index, y = 均值, fill = index)) +
  geom_col(position = "dodge", width = 0.8)+ylim(0,2) +
  scale_fill_manual(values = my_pal, guide = "none")+
  geom_errorbar(aes(ymin = 均值-标准差, ymax = 均值+标准差), width = 0.25,linewidth=1)+
  theme_classic()+
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))+xlab("")+ylab("TP discharge (g/Kg)")

p5



######################################
fig1_f <- data.frame(
    能耗 = c("Extensive", "Semi-intensive", "RAS", "Net cage"),
    MJ_per_kg = c(6.15, 71.93, 94.83, 40.82),
    鱼能耗标准差 = c(6.39, 5.91, 86.6, 18.3)
  )

fig1_f
fig1_f$能耗<-factor(fig1_f$能耗,levels=c("Extensive","Semi-intensive","RAS","Net cage" ))
p6<-ggplot(fig1_f, aes(x = 能耗, y = MJ_per_kg, fill = 能耗)) +
  geom_col(position = "dodge", width = 0.8)+ylim(-1,200) +
  scale_fill_manual(values = my_pal, guide = "none")+
  geom_errorbar(aes(ymin = MJ_per_kg-鱼能耗标准差, ymax = MJ_per_kg+鱼能耗标准差), width = 0.25,linewidth=1)+
  theme_classic()+
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))+xlab("")+ylab("TP discharge (g/Kg)")

p6


fig1_g <- data.frame(
  排N = c("Extensive", "Semi-intensive", "RAS", "Net cage"),
  g_per_kg = c(46.57, 57.02, 63.43, 66.44),
  鱼N排放标准差 = c(15.1, 32.5, 27, 26.99)
)


fig1_g
fig1_g$排N<-factor(fig1_g$排N,levels=c("Extensive","Semi-intensive","RAS","Net cage" ))

p7<-ggplot(fig1_g, aes(x = 排N, y = g_per_kg, fill = 排N)) +
  geom_col(position = "dodge", width = 0.8)+ylim(0,100) +
  scale_fill_manual(values = my_pal, guide = "none")+
  geom_errorbar(aes(ymin = g_per_kg-鱼N排放标准差, ymax = g_per_kg+鱼N排放标准差), width = 0.25,linewidth=1)+
  theme_classic()+
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))+xlab("")+ylab("TP discharge (g/Kg)")

p7



################图合并
library(patchwork)

(p1+p2+plot_layout(guides = 'collect'))/(p3+p4+p5)/(p6+p7)+ plot_annotation(tag_levels = 'A')


