# define cross-basis matrix (combining nonlinear exposure and lag functions)
basis_tem <- crossbasis(lag_tem,argvar = list(fun = "ns", knots = equalknots(data$tem, 1)), arglag = list(fun = "ns",knots = lagknot))
basis_pre <- crossbasis(lag_pre,argvar = list(fun = "ns", knots = equalknots(data$pre, 1)),arglag = list(fun = "ns",knots = lagknot))
basis_sun <- crossbasis(lag_sun,argvar = list(fun = "ns", knots = equalknots(data$sun, 1)),arglag = list(fun = "ns",knots = lagknot))
basis_itns <- crossbasis(lag_itns,argvar = list(fun = "ns"),arglag = list(fun = "ns",knots = lagknot))
basis_irs <- crossbasis(lag_irs,argvar = list(fun = "ns"),arglag = list(fun = "ns",knots = lagknot))
basis_mda <- crossbasis(lag_mda,argvar = list(fun = "ns"),arglag = list(fun = "ns",knots = lagknot))
basis_tmda <- crossbasis(lag_tmda,argvar = list(fun = "ns"),arglag = list(fun = "ns",knots = lagknot))
basis_agri <- crossbasis(lag_agri,argvar = list(fun = "ns"),arglag = list(fun = "ns",knots = lagknot))

sun_ind1 <- data$sun - quantile(data$sun, p = 0.75, na.rm = T) # highly urbanised 
sun_ind2 <- data$sun - quantile(data$sun, p = 0.50, na.rm = T) # intermediate
sun_ind3 <- data$sun - quantile(data$sun, p = 0.25, na.rm = T) # more rural

agri_ind1 <- data$Agri - quantile(data$Agri, p = 0.75, na.rm = T) # highly urbanised 
agri_ind2 <- data$Agri - quantile(data$Agri, p = 0.50, na.rm = T) # intermediate
agri_ind3 <- data$Agri - quantile(data$Agri, p = 0.25, na.rm = T) # more rural

sun_basis1_itns <- basis_itns*sun_ind1
sun_basis2_itns <- basis_itns*sun_ind2
sun_basis3_itns <- basis_itns*sun_ind3

sun_basis1_tmda <- basis_tmda*sun_ind1
sun_basis2_tmda <- basis_tmda*sun_ind2
sun_basis3_tmda <- basis_tmda*sun_ind3

sun_basis1_irs <- basis_irs*sun_ind1
sun_basis2_irs <- basis_irs*sun_ind2
sun_basis3_irs <- basis_irs*sun_ind3

sun_basis1_mda <- basis_mda*sun_ind1
sun_basis2_mda <- basis_mda*sun_ind2
sun_basis3_mda <- basis_mda*sun_ind3

agri_basis1_itns <- basis_itns*agri_ind1
agri_basis2_itns <- basis_itns*agri_ind2
agri_basis3_itns <- basis_itns*agri_ind3

agri_basis1_tmda <- basis_tmda*agri_ind1
agri_basis2_tmda <- basis_tmda*agri_ind2
agri_basis3_tmda <- basis_tmda*agri_ind3

agri_basis1_irs <- basis_irs*agri_ind1
agri_basis2_irs <- basis_irs*agri_ind2
agri_basis3_irs <- basis_irs*agri_ind3

agri_basis1_mda <- basis_mda*agri_ind1
agri_basis2_mda <- basis_mda*agri_ind2
agri_basis3_mda <- basis_mda*agri_ind3

colnames(basis_tem) = paste0("basis_tem.", colnames(basis_tem))
colnames(basis_pre) = paste0("basis_pre.", colnames(basis_pre))
colnames(basis_sun) = paste0("basis_sun.", colnames(basis_sun))
colnames(basis_itns) = paste0("basis_itns.", colnames(basis_itns))
colnames(basis_irs) = paste0("basis_irs.", colnames(basis_irs))
colnames(basis_mda) = paste0("basis_mda.", colnames(basis_mda))
colnames(basis_tmda) = paste0("basis_tmda.", colnames(basis_tmda))
colnames(basis_agri) = paste0("basis_agri.", colnames(basis_agri))

colnames(sun_basis1_itns) = paste0("sun_basis1_itns.", colnames(sun_basis1_itns))
colnames(sun_basis2_itns) = paste0("sun_basis2_itns.", colnames(sun_basis2_itns))
colnames(sun_basis3_itns) = paste0("sun_basis3_itns.", colnames(sun_basis3_itns))
colnames(sun_basis1_tmda) = paste0("sun_basis1_tmda.", colnames(sun_basis1_tmda))
colnames(sun_basis2_tmda) = paste0("sun_basis2_tmda.", colnames(sun_basis2_tmda))
colnames(sun_basis3_tmda) = paste0("sun_basis3_tmda.", colnames(sun_basis3_tmda))
colnames(sun_basis1_irs) = paste0("sun_basis1_irs.", colnames(sun_basis1_irs))
colnames(sun_basis2_irs) = paste0("sun_basis2_irs.", colnames(sun_basis2_irs))
colnames(sun_basis3_irs) = paste0("sun_basis3_irs.", colnames(sun_basis3_irs))
colnames(sun_basis1_mda) = paste0("sun_basis1_mda.", colnames(sun_basis1_mda))
colnames(sun_basis2_mda) = paste0("sun_basis2_mda.", colnames(sun_basis2_mda))
colnames(sun_basis3_mda) = paste0("sun_basis3_mda.", colnames(sun_basis3_mda))

colnames(agri_basis1_itns) = paste0("agri_basis1_itns.", colnames(agri_basis1_itns))
colnames(agri_basis2_itns) = paste0("agri_basis2_itns.", colnames(agri_basis2_itns))
colnames(agri_basis3_itns) = paste0("agri_basis3_itns.", colnames(agri_basis3_itns))
colnames(agri_basis1_tmda) = paste0("agri_basis1_tmda.", colnames(agri_basis1_tmda))
colnames(agri_basis2_tmda) = paste0("agri_basis2_tmda.", colnames(agri_basis2_tmda))
colnames(agri_basis3_tmda) = paste0("agri_basis3_tmda.", colnames(agri_basis3_tmda))
colnames(agri_basis1_irs) = paste0("agri_basis1_irs.", colnames(agri_basis1_irs))
colnames(agri_basis2_irs) = paste0("agri_basis2_irs.", colnames(agri_basis2_irs))
colnames(agri_basis3_irs) = paste0("agri_basis3_irs.", colnames(agri_basis3_irs))
colnames(agri_basis1_mda) = paste0("agri_basis1_mda.", colnames(agri_basis1_mda))
colnames(agri_basis2_mda) = paste0("agri_basis2_mda.", colnames(agri_basis2_mda))
colnames(agri_basis3_mda) = paste0("agri_basis3_mda.", colnames(agri_basis3_mda))

sun <- data$sun
agri <- data$Agri

#updata baseformula
baseformula <- Y ~ 1 +
  f(S, model = "bym2", replicate = T2, graph = "D:/malaria_hainan/map.graph", scale.model = TRUE, hyper = precision.prior) + 
  basis_tem + basis_pre

crossformula1.1 <- update.formula(baseformula, ~. + basis_itns + sun_basis1_itns + sun + basis_agri)
crossformula1.2 <- update.formula(baseformula, ~. + basis_itns + sun_basis2_itns + sun + basis_agri)
crossformula1.3 <- update.formula(baseformula, ~. + basis_itns + sun_basis3_itns + sun + basis_agri)

crossformula1.4 <- update.formula(baseformula, ~. + basis_tmda + sun_basis1_tmda + sun + basis_agri)
crossformula1.5 <- update.formula(baseformula, ~. + basis_tmda + sun_basis2_tmda + sun + basis_agri)
crossformula1.6 <- update.formula(baseformula, ~. + basis_tmda + sun_basis3_tmda + sun + basis_agri)

crossformula1.7 <- update.formula(baseformula, ~. + basis_irs + sun_basis1_irs + sun + basis_agri)
crossformula1.8 <- update.formula(baseformula, ~. + basis_irs + sun_basis2_irs + sun + basis_agri)
crossformula1.9 <- update.formula(baseformula, ~. + basis_irs + sun_basis3_irs + sun + basis_agri)

crossformula1.10 <- update.formula(baseformula, ~. + basis_mda + sun_basis1_mda + sun + basis_agri)
crossformula1.11 <- update.formula(baseformula, ~. + basis_mda + sun_basis2_mda + sun + basis_agri)
crossformula1.12 <- update.formula(baseformula, ~. + basis_mda + sun_basis3_mda + sun + basis_agri)

crossformula2.1 <- update.formula(baseformula, ~. + basis_itns + agri_basis1_itns + agri + basis_sun)
crossformula2.2 <- update.formula(baseformula, ~. + basis_itns + agri_basis2_itns + agri + basis_sun)
crossformula2.3 <- update.formula(baseformula, ~. + basis_itns + agri_basis3_itns + agri + basis_sun)

crossformula2.4 <- update.formula(baseformula, ~. + basis_tmda + agri_basis1_tmda + agri + basis_sun)
crossformula2.5 <- update.formula(baseformula, ~. + basis_tmda + agri_basis2_tmda + agri + basis_sun)
crossformula2.6 <- update.formula(baseformula, ~. + basis_tmda + agri_basis3_tmda + agri + basis_sun)

crossformula2.7 <- update.formula(baseformula, ~. + basis_irs + agri_basis1_irs + agri + basis_sun)
crossformula2.8 <- update.formula(baseformula, ~. + basis_irs + agri_basis2_irs + agri + basis_sun)
crossformula2.9 <- update.formula(baseformula, ~. + basis_irs + agri_basis3_irs + agri + basis_sun)

crossformula2.10 <- update.formula(baseformula, ~. + basis_mda + agri_basis1_mda + agri + basis_sun)
crossformula2.11 <- update.formula(baseformula, ~. + basis_mda + agri_basis2_mda + agri + basis_sun)
crossformula2.12 <- update.formula(baseformula, ~. + basis_mda + agri_basis3_mda + agri + basis_sun)

# create a list of formulas
formulas <- list(
  crossformula1.1,crossformula1.2,crossformula1.3,crossformula1.4,crossformula1.5,crossformula1.6,
  crossformula1.7,crossformula1.8,crossformula1.9,crossformula1.10,crossformula1.11,crossformula1.12,
  crossformula2.1,crossformula2.2,crossformula2.3,crossformula2.4,crossformula2.5,crossformula2.6,
  crossformula2.7,crossformula2.8,crossformula2.9,crossformula2.10,crossformula2.11,crossformula2.12)

# create model label string
lab <- c(
  "crossmodel1.1","crossmodel1.2","crossmodel1.3","crossmodel1.4","crossmodel1.5","crossmodel1.6",
  "crossmodel1.7","crossmodel1.8","crossmodel1.9","crossmodel1.10","crossmodel1.11","crossmodel1.12",
  "crossmodel2.1","crossmodel2.2","crossmodel2.3","crossmodel2.4","crossmodel2.5","crossmodel2.6",
  "crossmodel2.7","crossmodel2.8","crossmodel2.9","crossmodel2.10","crossmodel2.11","crossmodel2.12")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("D:/malaria_hainan/output/pv/", lab[i],".RData"))})

# create table to store DIC
table1 <- data.frame(Model  = c(
  "highest sun-itns","high sun-itns","low sun-itns",
  "highest sun-tmda","high sun-tmda","low sun-tmda",
  "highest sun-irs","high sun-irs","low sun-irs",
  "highest sun-mda","high sun-mda","low sun-mda",
  "highest agri-itns","high agri-itns","low agri-itns",
  "highest agri-tmda","high agri-tmda","low agri-tmda",
  "highest agri-irs","high agri-irs","low agri-irs",
  "highest agri-mda","high agri-mda","low agri-mda"), 
  DIC = NA,
  logscore = NA)

for(i in 1:length(formulas))
{
  load(paste0("D:/malaria_hainan/output/pv/",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 3)
  table1$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table 
table1

fwrite(table1, file = "D:/malaria_hainan/figs/table_pv03.csv", quote = FALSE, 
       row.names = FALSE) 

bestfit <- which.min(table1$DIC)
bestfit
