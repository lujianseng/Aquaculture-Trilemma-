rm(list=ls())
library(reshape2)
library(ggplot2)
library(ggsci)
library("scales")

show_col(pal_futurama("planetexpress")(12))
cols<-c("#FF6F00FF","#C71000FF","#008EA0FF","#8A4198FF","#5A9599FF","#FF6348FF","#84D7E1FF","#FF95A8FF",
        "#3D3B25FF","#ADE2D0FF")

fig3_a <- data.frame(
  Years = c(2021, 2030, 2050),
  FAL = c(0.333859, 0, 0),
  FEN = c(0.698996, 1.005829281, 1.6354318),
  RAS = c(0.758289, 1.0875791, 1.9198088),
  FOA = c(0.376065, 0.632072339, 1.311719),
  FAP = c(23.2652, 28.74007, 39.22363),
  nFAP = c(6.08735, 6.974593, 8.740889),
  PFA = c(3.9669, 6.578085, 12.77747),
  nFAL = c(3.7058, 3.814532, 3.718568),
  nFEN = c(12.037, 15.31203, 21.25102)
)
fig3_at<-t(fig3_a)
colnames(fig3_at) <- fig3_at[1, ]

# 去掉第一行，它已经被用作列名
fig3_at <- fig3_at[-1, ]

# 将数据转换为数据框
fig3_at <- as.data.frame(fig3_at, stringsAsFactors = FALSE)
fig3_at$index <- rownames(fig3_at)
fig3_ad<-melt(fig3_at,id.vars = "index")
fig3_ad$index<-factor(fig3_ad$index,levels=c( "FAL","FEN","RAS","FOA","FAP","nFAP","PFA","nFAL","nFEN"))
p1<-ggplot(data=fig3_ad, aes(variable, value, fill=index)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  # scale_fill_simpsons( name="")+
  scale_fill_brewer(palette="RdYlBu", name="") +
   # scale_fill_manual(values = cols, name="") +
  xlab("") + 
  ylab("Production (M t)") +
  theme(axis.title.y=element_text(hjust=1, vjust=0)) +  # 调整y轴标签位置
  theme(legend.position="top", legend.direction="horizontal") +
  guides(fill=guide_legend(nrow=1, bycol=TRUE)) +
  theme(panel.grid.major.x=element_line(color="gray", size=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  coord_flip()+theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
                     axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))




p1
##########
fig3_b<- data.frame(
  Years = c(2021, 2030, 2050),
  FAL = c(1.055, 0, 0),
  FEN = c(2.2088, 3.1784, 5.168),
  RAS = c(6.567, 9.4184, 16.6255),
  FOA = c(1.1884, 1.9973, 4.154),
  FAP = c(8.6081, 10.6338, 14.5127),
  nFAP = c(2.2523, 2.5806, 3.2341),
  PFA = c(0.0674, 0.1118, 0.2172),
  nFAL = c(0.063, 0.06485, 0.06322),
  nFEN = c(0.2046, 0.2603, 0.3613)
)

fig3_bt<-t(fig3_b)
colnames(fig3_bt) <- fig3_bt[1, ]

# 去掉第一行，它已经被用作列名
fig3_bt <- fig3_bt[-1, ]

# 将数据转换为数据框
fig3_bt <- as.data.frame(fig3_bt, stringsAsFactors = FALSE)
fig3_bt$index <- rownames(fig3_bt)
fig3_bd<-melt(fig3_bt,id.vars = "index")
fig3_bd$index<-factor(fig3_bd$index,levels=c( "FAL","FEN","RAS","FOA","FAP","nFAP","PFA","nFAL","nFEN"))

p2<-ggplot(data=fig3_bd, aes(variable, value, fill=index)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  scale_fill_brewer(palette="RdYlBu", name="") +
  xlab("") + 
  ylab("Energy consumption (B Kwh)") +
  guides(fill=guide_legend(nrow=1, bycol=TRUE)) +
  theme(panel.grid.major.y=element_line(color="gray", size=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme(legend.position = "none")+theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
                                        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
p2
##########
fig3_c<- data.frame(
  Years = c(2021, 2030, 2050),
  FAL = c(12.5174, 0, 0),
  FEN = c(51.2909, 73.8057, 120.0047),
  RAS = c(4.2017, 6.0263, 10.6377),
  FOA = c(27.5949, 46.3802, 96.2513),
  FAP = c(125.1202, 154.5641, 210.9447),
  nFAP = c(0, 0, 0),
  PFA = c(20.0527, 33.2522, 64.5901),
  nFAL = c(0, 0, 0),
  nFEN = c(0, 0, 0)
)

fig3_ct<-t(fig3_c)
colnames(fig3_ct) <- fig3_ct[1, ]

# 去掉第一行，它已经被用作列名
fig3_ct <- fig3_ct[-1, ]

# 将数据转换为数据框
fig3_ct <- as.data.frame(fig3_ct, stringsAsFactors = FALSE)
fig3_ct$index <- rownames(fig3_ct)
fig3_cd<-melt(fig3_ct,id.vars = "index")
fig3_cd$index<-factor(fig3_cd$index,levels=c( "FAL","FEN","RAS","FOA","FAP","nFAP","PFA","nFAL","nFEN"))

p3<-ggplot(data=fig3_cd, aes(variable, value, fill=index)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  scale_fill_brewer(palette="RdYlBu", name="") +
  xlab("") + 
  ylab("TN discharge (B g)") +
  guides(fill=guide_legend(nrow=1, bycol=TRUE)) +
  theme(panel.grid.major.y=element_line(color="gray", size=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme(legend.position = "none")+theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
                                        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
p3
##########
fig3_d<- data.frame(
  Years = c(2021, 2030, 2050),
  FAL = c(0.8313, 0, 0),
  FEN = c(1.1184, 1.6093, 2.6167),
  RAS = c(1.1374, 1.6314, 2.8797),
  FOA = c(0.6017, 1.0113, 2.0988),
  FAP = c(102.033, 126.044, 172.021),
  nFAP = c(10.744, 12.31, 15.428),
  PFA = c(0.03967, 0.06578, 0.1278),
  nFAL = c(0.003706, 0.003815, 0.003719),
  nFEN = c(0, 0, 0)
)
fig3_dt<-t(fig3_d)
colnames(fig3_dt) <- fig3_dt[1, ]

# 去掉第一行，它已经被用作列名
fig3_dt <- fig3_dt[-1, ]

# 将数据转换为数据框
fig3_dt <- as.data.frame(fig3_dt, stringsAsFactors = FALSE)
fig3_dt$index <- rownames(fig3_dt)
fig3_dd<-melt(fig3_dt,id.vars = "index")
fig3_dd$index<-factor(fig3_dd$index,levels=c( "FAL","FEN","RAS","FOA","FAP","nFAP","PFA","nFAL","nFEN"))

p4<-ggplot(data=fig3_dd, aes(variable, value, fill=index)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  scale_fill_brewer(palette="RdYlBu",  name="") +
  xlab("") + 
  ylab(expression(paste("Freshwater use (B ", m^3, ")")))+
  guides(fill=guide_legend(nrow=1, bycol=TRUE)) +
  theme(panel.grid.major.y=element_line(color="gray", size=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme(legend.position = "none")+theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
                                        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))

p4
##########
fig3_e<-  data <- data.frame(
  Years = c(2021, 2030, 2050),
  FAL = c(0, 0, 0),
  FEN = c(0, 0, 0),
  RAS = c(0.08796, 0.12616, 0.2227),
  FOA = c(0, 0, 0),
  FAP = c(3.071, 3.7937, 5.1775),
  nFAP = c(1.08355, 1.2415, 1.5559),
  PFA = c(0.003967, 0.006578, 0.01278),
  nFAL = c(0.003706, 0.003815, 0.003719),
  nFEN = c(0, 0, 0)
)

fig3_et<-t(fig3_e)
colnames(fig3_et) <- fig3_et[1, ]

# 去掉第一行，它已经被用作列名
fig3_et <- fig3_et[-1, ]

# 将数据转换为数据框
fig3_et <- as.data.frame(fig3_et, stringsAsFactors = FALSE)
fig3_et$index <- rownames(fig3_et)
fig3_ed<-melt(fig3_et,id.vars = "index")
fig3_ed$index<-factor(fig3_ed$index,levels=c( "FAL","FEN","RAS","FOA","FAP","nFAP","PFA","nFAL","nFEN"))

p5<-ggplot(data=fig3_ed, aes(variable, value, fill=index)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  scale_fill_brewer(palette="RdYlBu", name="") +
  xlab("") + 
  ylab(expression(paste("Land use (B ", m^2, ")")))+
  guides(fill=guide_legend(nrow=1, bycol=TRUE)) +
  theme(panel.grid.major.y=element_line(color="gray", size=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme(legend.position = "none")+theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
                                        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
p5

library(patchwork)

total<-p1/(p2+p3+p4+p5+ plot_layout(ncol= 4))+ plot_layout(heights =c(1,2))+plot_annotation(tag_levels = 'A')
total

pdf_file <- "output_plot.pdf"
ggsave(pdf_file, plot = total, width =12, height = 8.5)  # 设置宽度和高度





# 设置新的主题
library(ggthemr)
############
tableau_colours <- c('#1F77B4', '#FF7F0E', '#2CA02C', '#D62728', '#9467BD', '#8C564B', '#CFECF9', '#7F7F7F', '#BCBD22', '#17BECF')
# you have to add a colour at the start of your palette for outlining boxes, we'll use a grey:
tableau_colours <- c("#555555", tableau_colours)
# remove previous effects:
ggthemr_reset()
# Define colours for your figures with define_palette
tableau <- define_palette(
  swatch = tableau_colours, # colours for plotting points and bars
  gradient = c(lower = tableau_colours[1L], upper = tableau_colours[2L]), #upper and lower colours for continuous colours
  background = "#EEEEEE" #defining a grey-ish background 
)
# set the theme for your figures:
ggthemr(tableau)

p1<-ggplot(data=fig3_ad, aes(variable, value, fill=index)) +
  geom_bar(position="stack", stat="identity") +
  xlab("") + 
  ylab("Production (M t)") +
  theme(axis.title.y=element_text(hjust=1, vjust=0)) +  # 调整y轴标签位置
  theme(legend.position="top", legend.direction="horizontal") +
  guides(fill=guide_legend(nrow=1, bycol=TRUE)) +
  theme(panel.grid.major.x=element_line(color="gray", size=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  coord_flip()+theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
                     axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))




p1

p2<-ggplot(data=fig3_bd, aes(variable, value, fill=index)) +
  geom_bar(position="stack", stat="identity") +
  xlab("") + 
  ylab("Energy consumption (B Kwh)") +
  guides(fill=guide_legend(nrow=1, bycol=TRUE)) +
  theme(panel.grid.major.y=element_line(color="gray", size=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme(legend.position = "none")+theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
                                        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
p2
p3<-ggplot(data=fig3_cd, aes(variable, value, fill=index)) +
  geom_bar(position="stack", stat="identity") +
  xlab("") + 
  ylab("TN discharge (B g)") +
  guides(fill=guide_legend(nrow=1, bycol=TRUE)) +
  theme(panel.grid.major.y=element_line(color="gray", size=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme(legend.position = "none")+theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
                                        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
p3
p4<-ggplot(data=fig3_dd, aes(variable, value, fill=index)) +
  geom_bar(position="stack", stat="identity") +
  xlab("") + 
  ylab(expression(paste("Freshwater use (B ", m^3, ")")))+
  guides(fill=guide_legend(nrow=1, bycol=TRUE)) +
  theme(panel.grid.major.y=element_line(color="gray", size=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme(legend.position = "none")+theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
                                        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))

p4

p5<-ggplot(data=fig3_ed, aes(variable, value, fill=index)) +
  geom_bar(position="stack", stat="identity") +
  xlab("") + 
  ylab(expression(paste("Land use (B ", m^2, ")")))+
  guides(fill=guide_legend(nrow=1, bycol=TRUE)) +
  theme(panel.grid.major.y=element_line(color="gray", size=0.5)) +
  scale_y_continuous(expand=c(0, 0)) +
  theme(legend.position = "none")+theme(axis.title.x =element_text(size=18), axis.title.y=element_text(size=18),
                                        axis.text=element_text(size=16))+
  theme(legend.title=element_text(face="italic",
                                  family="Times",
                                  colour="black", 
                                  size=18))+
  theme(legend.text = element_text(face = "italic",
                                   family = "Times",
                                   colour = "black", 
                                   size = 18))
