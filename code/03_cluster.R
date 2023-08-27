library(tidyverse)
four <- data[,c("pv_inc","pf_inc","tem","pre","sun","itns","irs","mda","tmda","Agri")]
four <- na.omit(four)
names(four)[names(four) =="pv_inc"] <-"P.vivax"
names(four)[names(four) =="pf_inc"] <-"P.falciparum"
cor_matr = cor(four)
library(Hmisc)
rcorr(as.matrix(four))
symnum(cor_matr)
library(corrplot)
pdf("D:/malaria_hainan/figs/Fig_corralation.pdf", width=8, height=8)
corrplot(cor_matr, method = c("number"), type="lower", order="original", addshade = c("negative"), tl.col="black", tl.srt=45)
dev.off()
library(PerformanceAnalytics)
chart.Correlation(four,histogram = TRUE,pch=19)
