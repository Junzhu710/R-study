##############################################################################
#                                     ______________________________         #
#                                    /                               \       #
#   Junzhu Zou                      |                                |       #
#                                   |        | |                     |       #
#   PhD Candidate                   |        |b|        / /          |       #
#   Horticulture                    |   __\  |a|    \/ / /           |       #
#                                   |      \ |m| /   \/ /            |       #
#   email: junzhuzou010@163.com     |        |b|/    / /             |       #
#       / junzhu710@gmail.com       |        |o|    / /              |       #
#                                   |      \ |o|   / /               |       #
#                                   |        | |  / /                |       #
#                                   |        | | / /                 |       #
#   R version: 4.0.3                |    \\\\| |/ /|////////         |       #
#                                   |  ~~~~~~~ ~~~ ~~~~~~~~~~~~~~~~  |       #
#   Date: 2021-09-14                 \_______________________________/       #
#                                                                            #
##############################################################################

#This script is to do PCA(Principal Component Analysis) and draw the plot (with optional ellipses).#
#本代码可进行PCA分析和PCA散点图（可选择性加椭圆)绘制#
#参考数据为PCA.csv，非真实数据，仅供代码运行参考#

#启动画图library，没有需要运行install.packages("ggplot2")后再运行下列#
library(ggplot2)

#GET DATA，设定你的工作路径#
#下行为载入工作路径，可用sesseion/set working directory/choose directory替代#
#setwd("D:/Workfiles/")#
dat<- read.csv("PCA.csv")

#整理data，改掉乱码，将第一列名字自动更改为row.name#
dat <- edit(dat)
row.names(dat)<- dat[,1]
dat<- dat[,-1]
str(dat)

#主成分分析#
pca <- prcomp(t(dat), scale = TRUE)
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,1)

#查看主成分柱形图#
barplot(pca.var.per, xlab = "Principal Component", ylab="Percent Variation")
pca.var.per
pca.data <- data.frame(Sample=rownames(pca$x),
                       PC1=pca$x[,1],
                       PC2=pca$x[,2],
                       PC3=pca$x[,3])

#导出PCA分析结果文件#
pca2<-do.call("rbind",pca)
write.table(pca2, "pca2.txt", sep = ",", quote = FALSE)

#以上结果可用其他分析软件得出#

#确定group,将独立样本名改回分组名#
dat1 <- read.csv("PCA2.csv")
groupdata <- dat1[,1]
#准备作图文件，将Sample改为GROUP#
pca.data$Sample <- groupdata
data_pca <- edit(pca.data)
data_pca$GROUP <- as.factor(data_pca$GROUP)

#画图！#
ggplot(data=data_pca,aes(x=PC1,y=PC2,color=GROUP,shape=GROUP))+
  #点形状#
  scale_shape_manual(values = 1:nlevels(data_pca$GROUP))+
  #点大小#
  geom_point(size=3)+
  #添加椭圆,可不加，如需椭圆删掉井字符内的内容#  stat_ellipse(type = "norm")+
  #个人惯用主题，删除底纹，可根据个人喜好调整#
  theme_bw()+theme(panel.grid=element_blank())+
  #x轴显示PC1得分#
  xlab(paste("PC1(",pca.var.per[1],"%","variance)",sep=""))+
  #y轴显示PC2得分#
  ylab(paste("PC2(",pca.var.per[2],"%","variance)",sep=""))

