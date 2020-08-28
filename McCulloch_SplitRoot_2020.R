#Code for analysis and figures for split-root experiment
#with patches of soil nutrients in Robinia psuedoacacia
#Experiment preformed in the Brown University grennhouse

#libraries####
library ("tidyverse")
library ("infer")
library("plyr")
library("dplyr")
library("ggplot2")
library("ggpubr")

#Whole-plant analyses and figure####

data <- read.csv("Agg_GHdata.csv")

#Calculating response variables
data$nmols_eth <- (1*((data$eth.ppm.a*60)/(293.15*0.0821)))
data$trt <-as.factor(data$trt)
data$tot_mass <- (data$r.wgt+data$l.mass+data$s.mass)
#nitrogenase efficiency
data$SNFrate <- (data$nmols_eth/data$nod.wgt.a)
#ARA per seedling
data$ARA <-(data$SNFrate*data$tot.nod.wgt)
#ARA per g of seedling
data$ARAgs <- data$ARA/data$tot_mass
#Average nodule mass
data$avg.mass <- (data$tot.nod.wgt/data$nod.num)

#Control comparisons
data_c <- data %>%
  filter(trt.c =="3")
data_c1 <- data %>%
  filter(trt.c =="4")
data_c <- rbind(data_c,data_c1)

#Control seedling analyses
##Wilcox ran sum test for variables
wilcox.test(tot_mass ~ trt, data=data_c)
wilcox.test(tot.nod.wgt ~ trt, data=data_c)
wilcox.test(SNFrate ~ trt, data=data_c)
wilcox.test(ARA ~ trt, data=data_c)
wilcox.test(ARAgs ~ trt, data=data_c)
wilcox.test((tot.nod.wgt/r.wgt) ~ trt, data=data_c)

#removing controls from dataframe#
data_noc1 <- data %>%
  filter(trt.c == "1") 
data_noc2 <- data %>%
  filter(trt.c == "2") 
data_noc <- rbind(data_noc1,data_noc2)

#Experimental seedling analyses 
##Wilcox ran sum test for variables
wilcox.test(tot_mass ~ trt, data=data_noc)
wilcox.test(tot.nod.wgt ~ trt, data=data_noc)
wilcox.test(SNFrate ~ trt, data=data_noc)
wilcox.test(ARA ~ trt, data=data_noc)
wilcox.test(ARAgs ~ trt, data=data_noc)
wilcox.test((tot.nod.wgt/r.wgt) ~ trt, data=data_noc)
wilcox.test(avg.mass ~ trt, data=data_noc)

#figure 2####
a<-ggplot(data=data_noc, aes(x=trt, y=tot_mass, fill = trt), stat="identity", colour = "grey25") +
  geom_boxplot()  +
  scale_fill_manual(values=c("coral3","skyblue3"))+
  xlab ("") +
  ylab ("")  +
  theme_pubr()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin=unit(c(1,1,1,1.2),"cm"))+
  geom_jitter(aes(x=trt, y=tot_mass, fill = trt), width = 0.15 )+
  guides(fill=FALSE)

b<-ggplot(data=data_noc, aes(x=trt, y=tot.nod.wgt, fill = trt), stat="identity", colour = "grey25") +
  geom_boxplot()  +
  scale_fill_manual(values=c("coral3","skyblue3"))+
  xlab ("") +
  ylab ("")  +
  theme_pubr()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin=unit(c(1,1,1,1.2),"cm"))+
  geom_jitter(aes(x=trt, y=tot.nod.wgt, fill = trt), width = 0.15 )+
  guides(fill=FALSE)

c<-ggplot(data=data_noc, aes(x=trt, y=SNFrate, fill = trt), stat="identity", colour = "grey25") +
  geom_boxplot()  +
  scale_fill_manual(values=c("coral3","skyblue3"))+
  xlab ("") +
  ylab ("")  +
  theme_pubr()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin=unit(c(1,1,1,1.2),"cm"))+
  geom_jitter(aes(x=trt, y=SNFrate, fill = trt), width = 0.15 )+
  guides(fill=FALSE)

e<-ggplot(data=data_noc, aes(x=trt, y=ARAgs, fill = trt), stat="identity", colour = "grey25") +
  geom_boxplot()  +
  scale_fill_manual(values=c("coral3","skyblue3"))+
  xlab ("") +
  ylab ("")  +
  theme_pubr()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin=unit(c(1,1,1,1.2),"cm"))+
  geom_jitter(aes(x=trt, y=ARAgs, fill = trt), width = 0.15 )+
  guides(fill=FALSE)

ggarrange(a, b, c, e + rremove("x.text"), 
          labels = c("a", "b", "c", "d"),
          common.legend = TRUE,
          ncol = 2, nrow = 2, align = "v")
ggsave(file = "GH_manuscript_fig2.0.tiff",
       width = 25, height = 20, units = "cm", device = "tiff", dpi = 300)

#Split-root analyses and figure####

data <- read.csv ("GH_Data.csv", header=TRUE)

#Calculating response variables
data$nmols_eth <- (1*((data$eth.ppm.a*60)/(293.15*0.0821)))
data$trt.2 <- factor(data$trt.2, levels =  c("PH","noPH","PL","noPL"))
#nitrogenase activity
data$SNFrate <- (data$nmols_eth/data$nod.wgt.a)
#ARA per seedling
data$ARA <-(data$SNFrate*data$tot.nod.wgt)
#ARA per g of seedling
data$ARAgr <- data$ARA/data$r.wgt
#Avg nodule mass
data$avg.mass <- (data$tot.nod.wgt/data$nod.num)

#####Statistics split-root
kruskal.test(r.wgt ~ trt.2, data = data)
pairwise.wilcox.test(data$r.wgt, data$trt.2,
                     p.adjust.method = "BH")
kruskal.test(tot.nod.wgt ~ trt.2, data = data)
pairwise.wilcox.test(data$tot.nod.wgt, data$trt.2,
                     p.adjust.method = "BH")
kruskal.test(SNFrate ~ trt.2, data = data)
pairwise.wilcox.test(data$SNFrate, data$trt.2,
                     p.adjust.method = "BH")
kruskal.test(ARA ~ trt.2, data = data)
pairwise.wilcox.test(data$ARA, data$trt.2,
                     p.adjust.method = "BH")
kruskal.test(ARAgr ~ trt.2, data = data)
pairwise.wilcox.test(data$ARAgr, data$trt.2,
                     p.adjust.method = "BH")
kruskal.test((tot.nod.wgt/r.wgt) ~ trt.2, data = data)
pairwise.wilcox.test((data$tot.nod.wgt/data$r.wgt), data$trt.2,
                     p.adjust.method = "BH")
kruskal.test(avg.mass ~ trt.2, data = data)
pairwise.wilcox.test(data$avg.mass, data$trt.2,
                     p.adjust.method = "BH")

#figure 3####
a = ggplot(data, aes(x=trt.2, y=r.wgt, fill = trt.2), stat="identity", colour = "grey25") +
  geom_boxplot(notch= F) +
  scale_fill_manual(values=c("coral4","coral1","skyblue4","skyblue1"))+
  xlab ("") +
  ylab ("")  +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,.5,1),"cm"))+
  geom_jitter(aes(x=trt.2, y=r.wgt, fill = trt.2), width = 0.15 )+
  guides(fill=FALSE)

b = ggplot(data, aes(x=trt.2, y=tot.nod.wgt, fill = trt.2), stat="identity", colour = "grey25") +
  geom_boxplot(notch= F) +
  scale_fill_manual(values=c("coral4","coral1","skyblue4","skyblue1"))+
  xlab ("") +
  ylab ("")  +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,.5,1),"cm"))+
  geom_jitter(aes(x=trt.2, y=tot.nod.wgt, fill = trt.2), width = 0.15 )+
  guides(fill=FALSE)

c = ggplot(data, aes(x=trt.2, y=SNFrate, fill = trt.2), stat="identity", colour = "grey25") +
  geom_boxplot(notch= F) +
  scale_fill_manual(values=c("coral4","coral1","skyblue4","skyblue1"))+
  xlab ("") +
  ylab ("")  +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,.5,1),"cm"))+
  geom_jitter(aes(x=trt.2, y=SNFrate, fill = trt.2), width = 0.15 )+
  guides(fill=FALSE)

e = ggplot(data, aes(x=trt.2, y=ARAgr, fill = trt.2), stat="identity", colour = "grey25") +
  geom_boxplot(notch= F) +
  scale_fill_manual(values=c("coral4","coral1","skyblue4","skyblue1"))+
  xlab ("") +
  ylab ("")  +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,.5,1),"cm"))+
  geom_jitter(aes(x=trt.2, y=ARAgr, fill = trt.2), width = 0.15 )+
  guides(fill=FALSE)


ggarrange(a, b, c, e + rremove("x.text"), 
          labels = c("a", "b", "c", "d"),
          common.legend = TRUE,
          ncol = 2, nrow = 2)
ggsave(file = "GH_manuscript_fig3.0.tiff",
       width = 32, height = 20, units = "cm", device = "tiff", dpi=300)
