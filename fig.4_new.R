# 导入ggplot2库
library(ggplot2)
library(tidyr)

data_a <- data.frame(
  Year = c(2021, 2030, 2050),
  BAU = c(0.4336, 0.44035, 0.4894),
  Adjusted = c(0.4336, 0.44035, 0.4811)
)

# 将宽数据转换为长数据
combined_data_a <- gather(data_a, Type, Production, -Year)
combined_data_a$Year<-as.factor(combined_data_a$Year)



pa<-ggplot(combined_data_a, aes(x = Year, y = Production, group = Type, color = Type, shape = Type, linetype = Type)) +
  geom_point(size = 4, position = position_dodge(width = 0.1)) +
  geom_line(size = 0.8, position = position_dodge(width = 0.1)) +
  labs(x = "", y = "Energy uses (kwh/kg)") +
  scale_color_manual(values = c("BAU" = "#E41A1C", "Adjusted" = "#377EB8")) +
  scale_linetype_manual(values = c("BAU" = "solid", "Adjusted" = "dashed")) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0.4,0.43,0.46,0.49), limits = c(0.4, 0.5)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
pa
#################################################################################

data_b <- data.frame(
  Year = c(2021, 2030, 2050),
  BAU = c(4.7, 4.8956, 5.547),
  Adjusted = c(4.7, 4.8956, 5.165)
)


# 将宽数据转换为长数据
combined_data_b <- gather(data_b, Type, Production, -Year)
combined_data_b$Year<-as.factor(combined_data_b$Year)



pb<-ggplot(combined_data_b, aes(x = Year, y = Production, group = Type, color = Type, shape = Type, linetype = Type)) +
  geom_point(size = 4, position = position_dodge(width = 0.1)) +
  geom_line(size = 0.8, position = position_dodge(width = 0.1)) +
  labs(x = "", y = "TN discharge (g/kg)") +
  scale_color_manual(values = c("BAU" = "#E41A1C", "Adjusted" = "#377EB8")) +
  scale_linetype_manual(values = c("BAU" = "solid", "Adjusted" = "dashed")) +
  theme_minimal() +
  scale_y_continuous( limits = c(4, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
pb
#################################################################################

data_c <- data.frame(
  Year = c(2021, 2030, 2050),
  BAU = c(2.274, 2.224, 2.155),
  Adjusted = c(2.274, 2.575, 1.964)
)

# 将宽数据转换为长数据
combined_data_c <- gather(data_c, Type, Production, -Year)
combined_data_c$Year<-as.factor(combined_data_c$Year)



pc<-ggplot(combined_data_c, aes(x = Year, y = Production, group = Type, color = Type, shape = Type, linetype = Type)) +
  geom_point(size = 4, position = position_dodge(width = 0.1)) +
  geom_line(size = 0.8, position = position_dodge(width = 0.1)) +
  labs(x = "", y = expression("Freshwater uses (m"^3/"kg)")) +
  scale_color_manual(values = c("BAU" = "#E41A1C", "Adjusted" = "#377EB8")) +
  scale_linetype_manual(values = c("BAU" = "solid", "Adjusted" = "dashed")) +
  theme_minimal() +
  scale_y_continuous(breaks = c(1.6,1.9,2.2,2.5), limits = c(1.6, 2.6)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18) )+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
pc
#################################################################################

data_d <- data.frame(
  Year = c(2021, 2030, 2050),
  BAU = c(0.08296, 0.08063, 0.07698),
  Adjusted = c(0.08296, 0.08063, 0.0715)
)
# 将宽数据转换为长数据
combined_data_d <- gather(data_d, Type, Production, -Year)
combined_data_d$Year<-as.factor(combined_data_d$Year)



pd<-ggplot(combined_data_d, aes(x = Year, y = Production, group = Type, color = Type, shape = Type, linetype = Type)) +
  geom_point(size = 4, position = position_dodge(width = 0.1)) +
  geom_line(size = 0.8, position = position_dodge(width = 0.1)) +
  labs(x = "", y = expression("Land uses (m"^2/"kg)")) +
  scale_color_manual(values = c("BAU" = "#E41A1C", "Adjusted" = "#377EB8")) +
  scale_linetype_manual(values = c("BAU" = "solid", "Adjusted" = "dashed")) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0.07,0.08,0.09), limits = c(0.07, 0.09)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18) )+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
pd
#########################
data_e <- data.frame(
  Year = c(2021, 2030, 2050),
  BAU = c(51.23, 64.1445, 90.58),
  Adjusted = c(51.23, 64.1448, 90.78)
)

# 将宽数据转换为长数据
combined_data_e <- gather(data_e, Type, Production, -Year)
combined_data_e$Year<-as.factor(combined_data_e$Year)

pe<-ggplot(combined_data_e, aes(x = Year, y = Production, group = Type, color = Type, shape = Type, linetype = Type)) +
  geom_point(size = 4, position = position_dodge(width = 0.1)) +
  geom_line(size = 0.8, position = position_dodge(width = 0.1)) +
  labs(x = "", y = "Production (Mt)") +
  scale_color_manual(values = c("BAU" = "#E41A1C", "Adjusted" = "#377EB8")) +
  scale_linetype_manual(values = c("BAU" = "solid", "Adjusted" = "dashed")) +
  theme_minimal() +
  scale_y_continuous(breaks = c(50, 70, 90), limits = c(45, 95)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18) )+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
pe



############################################
data_f <- data <- data.frame(
  Year = c(2021, 2030, 2050, 2060),
  BAU = c(17.7705, 22.25, 31.49, 39.06),
  Adjusted = c(17.7705, 20.3348, 18.7809, 15.13)
)

# 将宽数据转换为长数据
combined_data_f <- gather(data_f, Type, Production, -Year)
combined_data_f$Year<-as.factor(combined_data_f$Year)

pf<-ggplot(combined_data_f, aes(x = Year, y = Production, group = Type, color = Type, shape = Type, linetype = Type)) +
  geom_point(size = 4, position = position_dodge(width = 0.1)) +
  geom_line(size = 0.8, position = position_dodge(width = 0.1)) +
  labs(x = "", y = expression("CO"[2] ~ " emission (B Kg)")) +
  scale_color_manual(values = c("BAU" = "#E41A1C", "Adjusted" = "#377EB8")) +
  scale_linetype_manual(values = c("BAU" = "solid", "Adjusted" = "dashed")) +
  theme_minimal() +
  scale_y_continuous(limits = c(10, 40)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
pf




library(patchwork)

pu<-(pa+pb+pc+pd)+ plot_layout(ncol= 4)

pg<-pe+pf+plot_layout(ncol= 2)

total<-pu/pg+plot_annotation(tag_levels = 'A')+ 
  plot_layout(guides = 'collect')
total

pdf_file <- "output_plot.pdf"
ggsave(pdf_file, plot = total, width =14, height = 9)  # 设置宽度和高度  
