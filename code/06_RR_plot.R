load("D:/malaria_hainan/output/pf/newmodel1.1.RData")

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_itns", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_itns, coef = coef[indt], vcov=vcov[indt,indt], #to = 1,
                   model.link = "log", by = 0.01, bylag = 0.25, cen = 0) 

group <- NA
itns <- predt$predvar
itns_allRR <- predt$allRRfit
itns_allRRlow <- predt$allRRlow
itns_allRRhigh <- predt$allRRhigh

itns_pf <- data.frame(group, itns, itns_allRR, itns_allRRlow, itns_allRRhigh)
itns_pf[is.na(itns_pf[,1]),1] <- "P.falciparum" 

load("D:/malaria_hainan/output/pf/newmodel1.2.RData")
# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_irs", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_irs, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", by = 30, bylag = 0.25, cen = 0) 

group <- NA
irs <- predt$predvar
irs_allRR <- predt$allRRfit
irs_allRRlow <- predt$allRRlow
irs_allRRhigh <- predt$allRRhigh

irs_pf <- data.frame(group, irs, irs_allRR, irs_allRRlow, irs_allRRhigh)
irs_pf[is.na(irs_pf[,1]),1] <- "P.falciparum" 

load("D:/malaria_hainan/output/pv/newmodel1.1.RData")

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_itns", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_itns, coef = coef[indt], vcov=vcov[indt,indt], #to = 1,
                   model.link = "log", by = 0.01, bylag = 0.25, cen = 0) 

itns <- predt$predvar
itns_allRR <- predt$allRRfit
itns_allRRlow <- predt$allRRlow
itns_allRRhigh <- predt$allRRhigh

itns_pv <- data.frame(group, itns, itns_allRR, itns_allRRlow, itns_allRRhigh)
itns_pv[is.na(itns_pv[,1]),1] <- "P.vivax" 

load("D:/malaria_hainan/output/pv/newmodel1.2.RData")
# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_irs", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_irs, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", by = 30, bylag = 0.25, cen = 0) 

irs <- predt$predvar
irs_allRR <- predt$allRRfit
irs_allRRlow <- predt$allRRlow
irs_allRRhigh <- predt$allRRhigh

irs_pv <- data.frame(group, irs, irs_allRR, irs_allRRlow, irs_allRRhigh)
irs_pv[is.na(irs_pv[,1]),1] <- "P.vivax" 

itns_plot <- rbind(itns_pv,itns_pf)
irs_plot <- rbind(irs_pv,irs_pf)

itns_plot$itns <- 1/itns_plot$itns
itns_pv$itns <- 1/itns_pv$itns
itns_pf$itns <- 1/itns_pf$itns
itns_pv <- arrange(itns_pv, by = itns)
itns_pf <- arrange(itns_pf, by = itns)

itnsplot <- itns_pv
itnsplot$itns_allRR <- itns_pv$itns_allRR-itns_pf$itns_allRR
itnsplot$itns_allRRhigh <- itns_pv$itns_allRRhigh-itns_pf$itns_allRRhigh
itnsplot$itns_allRRlow <- itns_pv$itns_allRRlow-itns_pf$itns_allRRlow

irsplot <- irs_pv
irsplot$irs_allRR <- irs_pv$irs_allRR-irs_pf$irs_allRR
irsplot$irs_allRRhigh <- irs_pv$irs_allRRhigh-irs_pf$irs_allRRhigh
irsplot$irs_allRRlow <- irs_pv$irs_allRRlow-irs_pf$irs_allRRlow

plot(diff(itns_pv$itns_allRR))
itns_pf[itns_pf$itns_allRR >= "0.80000",]

pdf("D:/malaria_hainan/figs/Fig itns_RR.pdf", width=6, height=4)
ggplot(itns_plot, aes(x = itns, color = group, fill = group)) +
  geom_ribbon(aes(ymin = itns_allRRlow, ymax = itns_allRRhigh), alpha = 0.3, linetype = 0) +  # alpha 修改透明度
  geom_line(aes(y = itns_allRR))+
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2))+xlim(10,0) +
  geom_hline(aes(yintercept=1),linetype = 2, color="gray50") +
  geom_vline(aes(xintercept=1.8),linetype = 2, color="gray50") +
  geom_vline(aes(xintercept=3.0),linetype = 2, color="gray50") +
  theme_bw(base_family = "Times") +
  scale_color_manual(values = c('#2c8bbe','#b50f63')) +
  scale_fill_manual(values = c('#2c8bbe','#b50f63')) + 
  labs(x = "Insecticide-treated mosquito nets per capita", y = "RR",
       color = "", fill = "")
dev.off()


pdf("D:/malaria_hainan/figs/Fig irs_RR.pdf", width=6, height=4)
ggplot(irs_plot, aes(x = log(irs+1), color = group, fill = group)) +
  geom_ribbon(aes(ymin = irs_allRRlow, ymax = irs_allRRhigh), alpha = 0.3, linetype = 0) +  # alpha 修改透明度
  geom_line(aes(y = irs_allRR))+
  geom_hline(aes(yintercept=1),linetype = 2, color="gray50") +
  theme_bw(base_family = "Times") +
  scale_color_manual(values = c('#2c8bbe','#b50f63')) +
  scale_fill_manual(values = c('#2c8bbe','#b50f63')) + 
  labs(x = "Indoor residual spraying", y = "RR",
       color = "", fill = "")           
dev.off()


load("D:/malaria_hainan/output/pf/newmodel1.4.RData")

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix


# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_tmda", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_tmda, coef = coef[indt], vcov=vcov[indt,indt], from = 0,
                   model.link = "log", by = 0.01, bylag = 0.25, cen = 0) 

group <- NA
tmda <- predt$predvar
tmda_allRR <- predt$allRRfit
tmda_allRRlow <- predt$allRRlow
tmda_allRRhigh <- predt$allRRhigh

tmda_pf <- data.frame(group, tmda, tmda_allRR, tmda_allRRlow, tmda_allRRhigh)
tmda_pf[is.na(tmda_pf[,1]),1] <- "P.falciparum" 

load("D:/malaria_hainan/output/pf/newmodel1.3.RData")

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_mda", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_mda, coef = coef[indt], vcov=vcov[indt,indt], from = 0,
                   model.link = "log", by = 0.01, bylag = 0.25, cen = 0) 

mda <- predt$predvar
mda_allRR <- predt$allRRfit
mda_allRRlow <- predt$allRRlow
mda_allRRhigh <- predt$allRRhigh

mda_pf <- data.frame(group, mda, mda_allRR, mda_allRRlow, mda_allRRhigh)
mda_pf[is.na(mda_pf[,1]),1] <- "P.falciparum" 

load("D:/malaria_hainan/output/pv/newmodel1.4.RData")

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix


# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_tmda", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_tmda, coef = coef[indt], vcov=vcov[indt,indt], from = 0,
                   model.link = "log", by = 0.01, bylag = 0.25, cen = 0) 

tmda <- predt$predvar
tmda_allRR <- predt$allRRfit
tmda_allRRlow <- predt$allRRlow
tmda_allRRhigh <- predt$allRRhigh

tmda_pv <- data.frame(group, tmda, tmda_allRR, tmda_allRRlow, tmda_allRRhigh)
tmda_pv[is.na(tmda_pv[,1]),1] <- "P.vivax" 

load("D:/malaria_hainan/output/pv/newmodel1.3.RData")

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_mda", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_mda, coef = coef[indt], vcov=vcov[indt,indt], from = 0,
                   model.link = "log", by = 0.01, bylag = 0.25, cen = 0) 

mda <- predt$predvar
mda_allRR <- predt$allRRfit
mda_allRRlow <- predt$allRRlow
mda_allRRhigh <- predt$allRRhigh

mda_pv <- data.frame(group, mda, mda_allRR, mda_allRRlow, mda_allRRhigh)
mda_pv[is.na(mda_pv[,1]),1] <- "P.vivax" 

tmda_plot <- rbind(tmda_pf,tmda_pv)
mda_plot <- rbind(mda_pf,mda_pv)

tmdaplot <- tmda_pv
tmdaplot$tmda_allRR <- tmda_pv$tmda_allRR-tmda_pf$tmda_allRR
tmdaplot$tmda_allRRhigh <- tmda_pv$tmda_allRRhigh-tmda_pf$tmda_allRRhigh
tmdaplot$tmda_allRRlow <- tmda_pv$tmda_allRRlow-tmda_pf$tmda_allRRlow

mdaplot <- mda_pv
mdaplot$mda_allRR <- mda_pv$mda_allRR-mda_pf$mda_allRR
mdaplot$mda_allRRhigh <- mda_pv$mda_allRRhigh-mda_pf$mda_allRRhigh
mdaplot$mda_allRRlow <- mda_pv$mda_allRRlow-mda_pf$mda_allRRlow


pdf("D:/malaria_hainan/figs/Fig tmda_RR.pdf", width=6, height=4)
ggplot(tmda_plot, aes(x = tmda, color = group, fill = group)) +
  geom_ribbon(aes(ymin = tmda_allRRlow, ymax = tmda_allRRhigh), alpha = 0.3, linetype = 0) +  # alpha 修改透明度
  geom_line(aes(y = tmda_allRR)) +
  geom_hline(aes(yintercept=1),linetype = 2, color="gray50") +
  geom_vline(aes(xintercept=0.8),linetype = 2, color="gray50") +
  theme_bw(base_family = "Times") +
  scale_color_manual(values = c('#2c8bbe','#b50f63')) +
  scale_fill_manual(values = c('#2c8bbe','#b50f63')) + 
  labs(x = "Targeted drug administration", y = "RR",
       color = "", fill = "")           
dev.off()

pdf("D:/malaria_hainan/figs/Fig mda_RR.pdf", width=6, height=4)
ggplot(mda_plot, aes(x = mda, color = group, fill = group)) +
  geom_ribbon(aes(ymin = mda_allRRlow, ymax = mda_allRRhigh), alpha = 0.3, linetype = 0) +  # alpha 修改透明度
  geom_line(aes(y = mda_allRR)) +
  geom_hline(aes(yintercept=1),linetype = 2, color="gray50") +
  geom_vline(aes(xintercept=0.8),linetype = 2, color="gray50") +
  theme_bw(base_family = "Times") +
  scale_color_manual(values = c('#2c8bbe','#b50f63')) +
  scale_fill_manual(values = c('#2c8bbe','#b50f63')) + 
  labs(x = "Mass drug administration", y = "RR",
       color = "", fill = "")           
dev.off()
