library(caret)
# set indicator to zero at point of interest (centring point)
# re-parameterise model to extract different predictions
dummy1 <- dummyVars("~irs_drug",data = data)
dummy1 <- data.frame(predict(dummy1, newdata = data))
print(dummy1)
irs_dummy_ind1 <- dummy1$irs_drugDDT# highly urbanised 
irs_dummy_ind2 <- dummy1$irs_drugDDT.DDVP# intermediate
irs_dummy_ind3 <- dummy1$irs_drugDDT.decamethrin# more rural
irs_dummy_ind4 <- dummy1$irs_drugDDT.Lindane# more rural

dummy2 <- dummyVars("~mda_drug",data = data)
dummy2 <- data.frame(predict(dummy2, newdata = data))
print(dummy2)
mda_dummy_ind1 <- dummy2$mda_drugChloroquine.Primaquine# highly urbanised 
mda_dummy_ind2 <- dummy2$mda_drugPaludrine.Primaquine.Cycloguanil.Pyrimethamine# intermediate
mda_dummy_ind3 <- dummy2$mda_drugPiperaquine.Primaquine# more rural
mda_dummy_ind4 <- dummy2$mda_drugPiperaquine.Primaquine.Chloroquine# more rural
mda_dummy_ind5 <- dummy2$mda_drugPiperaquine.Primaquine.Pyrimethamine# more rural

# Multiply each cross-basis variable by the linear terms (see Gasparrini et al. EHP 2015)
# note: exploit the column by column product 

# multiply the PDSI cross-basis variables by the urban linear terms
irs_dummy_basis1_irs <- basis_irs*irs_dummy_ind1
irs_dummy_basis2_irs <- basis_irs*irs_dummy_ind2
irs_dummy_basis3_irs <- basis_irs*irs_dummy_ind3
irs_dummy_basis4_irs <- basis_irs*irs_dummy_ind4

# multiply the PDSI cross-basis variables by the water shortage linear terms
mda_dummy_basis1_mda <- basis_mda*mda_dummy_ind1
mda_dummy_basis2_mda <- basis_mda*mda_dummy_ind2
mda_dummy_basis3_mda <- basis_mda*mda_dummy_ind3
mda_dummy_basis4_mda <- basis_mda*mda_dummy_ind4
mda_dummy_basis5_mda <- basis_mda*mda_dummy_ind5

# multiply the PDSI cross-basis variables by the water shortage linear terms
mda_dummy_basis1_tmda <- basis_tmda*mda_dummy_ind1
mda_dummy_basis2_tmda <- basis_tmda*mda_dummy_ind2
mda_dummy_basis3_tmda <- basis_tmda*mda_dummy_ind3
mda_dummy_basis4_tmda <- basis_tmda*mda_dummy_ind4
mda_dummy_basis5_tmda <- basis_tmda*mda_dummy_ind5

# assign unique column names to cross-basis matrix for inla() model
# note: not necessary for glm(), gam() or glm.nb() models
colnames(basis_tem) = paste0("basis_tem.", colnames(basis_tem))
colnames(basis_sun) = paste0("basis_sun.", colnames(basis_sun))
colnames(basis_itns) = paste0("basis_itns.", colnames(basis_itns))
colnames(basis_agri) = paste0("basis_agri.", colnames(basis_agri))

colnames(irs_dummy_basis1_irs) = paste0("irs_dummy_basis1_irs.", colnames(irs_dummy_basis1_irs))
colnames(irs_dummy_basis2_irs) = paste0("irs_dummy_basis2_irs.", colnames(irs_dummy_basis2_irs))
colnames(irs_dummy_basis3_irs) = paste0("irs_dummy_basis3_irs.", colnames(irs_dummy_basis3_irs))
colnames(irs_dummy_basis4_irs) = paste0("irs_dummy_basis4_irs.", colnames(irs_dummy_basis4_irs))

colnames(mda_dummy_basis1_mda) = paste0("mda_dummy_basis1_mda.", colnames(mda_dummy_basis1_mda))
colnames(mda_dummy_basis2_mda) = paste0("mda_dummy_basis2_mda.", colnames(mda_dummy_basis2_mda))
colnames(mda_dummy_basis3_mda) = paste0("mda_dummy_basis3_mda.", colnames(mda_dummy_basis3_mda))
colnames(mda_dummy_basis4_mda) = paste0("mda_dummy_basis4_mda.", colnames(mda_dummy_basis4_mda))
colnames(mda_dummy_basis5_mda) = paste0("mda_dummy_basis5_mda.", colnames(mda_dummy_basis5_mda))

colnames(mda_dummy_basis1_tmda) = paste0("mda_dummy_basis1_tmda.", colnames(mda_dummy_basis1_tmda))
colnames(mda_dummy_basis2_tmda) = paste0("mda_dummy_basis2_tmda.", colnames(mda_dummy_basis2_tmda))
colnames(mda_dummy_basis3_tmda) = paste0("mda_dummy_basis3_tmda.", colnames(mda_dummy_basis3_tmda))
colnames(mda_dummy_basis4_tmda) = paste0("mda_dummy_basis4_tmda.", colnames(mda_dummy_basis4_tmda))
colnames(mda_dummy_basis5_tmda) = paste0("mda_dummy_basis5_tmda.", colnames(mda_dummy_basis5_tmda))

# baseline model
baseformula <- Y ~ 1 +
  f(S, model = "bym2", replicate = T2, graph = "D:/malaria_hainan/map.graph", scale.model = TRUE, hyper = precision.prior) + 
  basis_sun + basis_tem + basis_agri + basis_pre

# best fitting model0 + interaction between pdsi cross-basis and urbanisation
formula1.1 <- update.formula(baseformula, ~. + irs_dummy_basis1_irs)
formula1.2 <- update.formula(baseformula, ~. + irs_dummy_basis2_irs)
formula1.3 <- update.formula(baseformula, ~. + irs_dummy_basis3_irs)
formula1.4 <- update.formula(baseformula, ~. + irs_dummy_basis4_irs)

# best fitting model0 + interaction between pdsi cross-basis and water supply shortage
formula2.1 <- update.formula(baseformula, ~. + mda_dummy_basis1_mda)
formula2.2 <- update.formula(baseformula, ~. + mda_dummy_basis2_mda)
formula2.3 <- update.formula(baseformula, ~. + mda_dummy_basis3_mda)
formula2.4 <- update.formula(baseformula, ~. + mda_dummy_basis4_mda)
formula2.5 <- update.formula(baseformula, ~. + mda_dummy_basis5_mda)

# best fitting model0 + interaction between pdsi cross-basis and water supply shortage
formula3.1 <- update.formula(baseformula, ~. + mda_dummy_basis1_tmda)
formula3.2 <- update.formula(baseformula, ~. + mda_dummy_basis2_tmda)
formula3.3 <- update.formula(baseformula, ~. + mda_dummy_basis3_tmda)
formula3.4 <- update.formula(baseformula, ~. + mda_dummy_basis4_tmda)
formula3.5 <- update.formula(baseformula, ~. + mda_dummy_basis5_tmda)

# create a list of formulas
formulas <- list(baseformula,
                 formula1.1, formula1.2, formula1.3, formula1.4, 
                 formula2.1, formula2.2, formula2.3, formula2.4, formula2.5,
                 formula3.1, formula3.2, formula3.3, formula3.4, formula3.5)

# create model label string
lab <- c("tem+sun+agri+pre",
         "irs_1", "irs_2", "irs_3", "irs_4",
         "mda_1", "mda_2", "mda_3", "mda_4", "mda_5",
         "tmda_1", "tmda_2", "tmda_3", "tmda_4", "tmda_5")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("D:/malaria_hainan/output/pv/", lab[i],".RData"))})

# create table to store DIC
table1 <- data.frame(Model  = c("tem+sun+agri+pre",
                                "irs_1", "irs_2", "irs_3", "irs_4",
                                "mda_1", "mda_2", "mda_3", "mda_4", "mda_5",
                                "tmda_1", "tmda_2", "tmda_3", "tmda_4", "tmda_5"), 
                     DIC = NA,
                     logscore = NA)

for(i in 1:length(formulas))
{
  load(paste0("D:/malaria_hainan/output/pv/",lab[i],".RData"))
  table1$DIC[i] <- round(model$dic$dic, 3)
  table1$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table 
# note: high, intermediate and low urban (water) models are different parametrisations of the same urban (water) interaction model 
table1

fwrite(table1, file = "D:/malaria_hainan/figs/table_pv04.csv", quote = FALSE, 
       row.names = FALSE) 

