load("D:/malaria_hainan/output/pf/mda_4.RData")
model_pf <- model
load("D:/malaria_hainan/output/pv/mda_4.RData")
model_pv <- model

# Select the hydrometeorological  model (without interactions) 
model <- model_pf

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_mda", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_mda, coef = coef[indt], vcov=vcov[indt,indt], from = 0,
                   model.link = "log", by = 0.01, bylag = 0.25, cen = 0) 

group <- NA
mda <- predt$predvar
mda_allRR <- predt$allRRfit
mda_allRRlow <- predt$allRRlow
mda_allRRhigh <- predt$allRRhigh

mda_pf <- data.frame(group, mda, mda_allRR, mda_allRRlow, mda_allRRhigh)
mda_pf[is.na(mda_pf[,1]),1] <- "P.falciparum" 

# Select the hydrometeorological  model (without interactions) 
model <- model_pv

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_mda", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_mda, coef = coef[indt], vcov=vcov[indt,indt], from = 0,
                   model.link = "log", by = 0.01, bylag = 0.25, cen = 0) 

group <- NA
mda <- predt$predvar
mda_allRR <- predt$allRRfit
mda_allRRlow <- predt$allRRlow
mda_allRRhigh <- predt$allRRhigh

mda_pv <- data.frame(group, mda, mda_allRR, mda_allRRlow, mda_allRRhigh)
mda_pv[is.na(mda_pv[,1]),1] <- "P.vivax" 

mda_plot <- rbind(mda_pf,mda_pv)

pdf("D:/malaria_hainan/figs/Fig mda_exact_RR2.pdf", width=6, height=4)
ggplot(mda_plot, aes(x = mda, color = group, fill = group)) +
  geom_ribbon(aes(ymin = mda_allRRlow, ymax = mda_allRRhigh), alpha = 0.3, linetype = 0) +  # alpha 修改透明度
  geom_line(aes(y = mda_allRR)) +
  ylim(0,1.5) +
  theme_bw(base_family = "Times") + 
  geom_hline(aes(yintercept=1),linetype = 2, color="gray50") +
  scale_color_manual(values = c('#2c8bbe','#b50f63')) +
  scale_fill_manual(values = c('#2c8bbe','#b50f63')) +
  geom_vline(aes(xintercept=0.8),linetype = 2, color="gray50") + 
  labs(x = "Mass drug administration(Piperaquine.Primaquine.Chloroquine)", y = "RR",
       color = "", fill = "")           
dev.off()

load("D:/malaria_hainan/output/pf/mda_3.RData")
model_pf <- model
load("D:/malaria_hainan/output/pv/mda_3.RData")
model_pv <- model

# Select the hydrometeorological  model (without interactions) 
model <- model_pf

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_mda", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_mda, coef = coef[indt], vcov=vcov[indt,indt], from = 0,
                   model.link = "log", by = 0.01, bylag = 0.25, cen = 0) 

group <- NA
mda <- predt$predvar
mda_allRR <- predt$allRRfit
mda_allRRlow <- predt$allRRlow
mda_allRRhigh <- predt$allRRhigh

mda_pf <- data.frame(group, mda, mda_allRR, mda_allRRlow, mda_allRRhigh)
mda_pf[is.na(mda_pf[,1]),1] <- "P.falciparum" 

# Select the hydrometeorological  model (without interactions) 
model <- model_pv

# extract full coef and vcov and create indicators for each term
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with Tmin crossbasis
indt <- grep("basis_mda", model$names.fixed)

# extract predictions from the Tmin DLNM centred on overall mean Tmin (19 deg C)
predt <- crosspred(basis_mda, coef = coef[indt], vcov=vcov[indt,indt], from = 0,
                   model.link = "log", by = 0.01, bylag = 0.25, cen = 0) 

group <- NA
mda <- predt$predvar
mda_allRR <- predt$allRRfit
mda_allRRlow <- predt$allRRlow
mda_allRRhigh <- predt$allRRhigh

mda_pv <- data.frame(group, mda, mda_allRR, mda_allRRlow, mda_allRRhigh)
mda_pv[is.na(mda_pv[,1]),1] <- "P.vivax" 

mda_plot <- rbind(mda_pf,mda_pv)

pdf("D:/malaria_hainan/figs/Fig mda_exact_RR1.pdf", width=6, height=4)
ggplot(mda_plot, aes(x = mda, color = group, fill = group)) +
  geom_ribbon(aes(ymin = mda_allRRlow, ymax = mda_allRRhigh), alpha = 0.3, linetype = 0) +  # alpha 修改透明度
  geom_line(aes(y = mda_allRR)) +
  ylim(0,1.5) +
  theme_bw(base_family = "Times") + 
  geom_hline(aes(yintercept=1),linetype = 2, color="gray50") +
  scale_color_manual(values = c('#2c8bbe','#b50f63')) +
  scale_fill_manual(values = c('#2c8bbe','#b50f63')) +
  geom_vline(aes(xintercept=0.8),linetype = 2, color="gray50") + 
  labs(x = "Mass drug administration(Piperaquine.Primaquine)", y = "RR",
       color = "", fill = "")           
dev.off()


load("D:/malaria_hainan/output/pf/tmda_4.RData")
model_pf <- model
load("D:/malaria_hainan/output/pv/tmda_4.RData")
model_pv <- model

# Select the hydrometeorological  model (without interactions) 
model <- model_pf

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

# Select the hydrometeorological  model (without interactions) 
model <- model_pv

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

tmda_pv <- data.frame(group, tmda, tmda_allRR, tmda_allRRlow, tmda_allRRhigh)
tmda_pv[is.na(tmda_pv[,1]),1] <- "P.vivax" 

tmda_plot <- rbind(tmda_pf,tmda_pv)

pdf("D:/malaria_hainan/figs/Fig tmda_exact_RR2.pdf", width=6, height=4)
ggplot(tmda_plot, aes(x = tmda, color = group, fill = group)) +
  geom_ribbon(aes(ymin = tmda_allRRlow, ymax = tmda_allRRhigh), alpha = 0.3, linetype = 0) +  # alpha 修改透明度
  geom_line(aes(y = tmda_allRR)) +
  #ylim(0,5) +
  theme_bw(base_family = "Times") + 
  geom_hline(aes(yintercept=1),linetype = 2, color="gray50") +
  geom_vline(aes(xintercept=0.8),linetype = 2, color="gray50") +
  scale_color_manual(values = c('#2c8bbe','#b50f63')) +
  scale_fill_manual(values = c('#2c8bbe','#b50f63')) +
  labs(x = "Targeted drug administration(Piperaquine.Primaquine.Chloroquine)", y = "RR",
       color = "", fill = "")           
dev.off()

load("D:/malaria_hainan/output/pf/tmda_3.RData")
model_pf <- model
load("D:/malaria_hainan/output/pv/tmda_3.RData")
model_pv <- model

# Select the hydrometeorological  model (without interactions) 
model <- model_pf

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

# Select the hydrometeorological  model (without interactions) 
model <- model_pv

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

tmda_pv <- data.frame(group, tmda, tmda_allRR, tmda_allRRlow, tmda_allRRhigh)
tmda_pv[is.na(tmda_pv[,1]),1] <- "P.vivax" 

tmda_plot <- rbind(tmda_pf,tmda_pv)

pdf("D:/malaria_hainan/figs/Fig tmda_exact_RR1.pdf", width=6, height=4)
ggplot(tmda_plot, aes(x = tmda, color = group, fill = group)) +
  geom_ribbon(aes(ymin = tmda_allRRlow, ymax = tmda_allRRhigh), alpha = 0.3, linetype = 0) +  # alpha 修改透明度
  geom_line(aes(y = tmda_allRR)) +
  #ylim(0,2) +
  theme_bw(base_family = "Times") + 
  geom_hline(aes(yintercept=1),linetype = 2, color="gray50") +
  geom_vline(aes(xintercept=0.8),linetype = 2, color="gray50") +
  scale_color_manual(values = c('#2c8bbe','#b50f63')) +
  scale_fill_manual(values = c('#2c8bbe','#b50f63')) +
  labs(x = "Targeted drug administration(Piperaquine.Primaquine)", y = "RR",
       color = "", fill = "")           
dev.off()
