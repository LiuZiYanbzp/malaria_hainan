# baseline model
baseformula <- Y ~ 1 + f(S, model = "bym2", replicate = T2, graph = "D:/malaria_hainan/map.graph", 
                         scale.model = TRUE, hyper = precision.prior) 

formula1.1 <- update.formula(baseformula, ~. + basis_tem)
formula1.2 <- update.formula(baseformula, ~. + basis_pre)
formula1.3 <- update.formula(baseformula, ~. + basis_sun)
formula1.4 <- update.formula(baseformula, ~. + basis_agri)

formula2.1 <- update.formula(baseformula, ~. + basis_tem + basis_pre)
formula2.2 <- update.formula(baseformula, ~. + basis_tem + basis_sun)
formula2.3 <- update.formula(baseformula, ~. + basis_tem + basis_agri)
formula2.4 <- update.formula(baseformula, ~. + basis_pre + basis_sun)
formula2.5 <- update.formula(baseformula, ~. + basis_pre + basis_agri)
formula2.6 <- update.formula(baseformula, ~. + basis_sun + basis_agri)

formula3.1 <- update.formula(baseformula, ~. + basis_tem + basis_pre + basis_sun)
formula3.2 <- update.formula(baseformula, ~. + basis_tem + basis_pre + basis_agri)
formula3.3 <- update.formula(baseformula, ~. + basis_tem + basis_sun + basis_agri)
formula3.4 <- update.formula(baseformula, ~. + basis_pre + basis_sun + basis_agri)

formula4.1 <- update.formula(baseformula, ~. + basis_tem + basis_pre + basis_sun + basis_agri)

formulas <- list(baseformula,
                 formula1.1,formula1.2,formula1.3,formula1.4,
                 formula2.1,formula2.2,formula2.3,formula2.4,formula2.5,formula2.6,
                 formula3.1,formula3.2,formula3.3,formula3.4,
                 formula4.1)

# create model label string
lab <- c("basemodel",
         "model1.1","model1.2","model1.3","model1.4",
         "model2.1","model2.2","model2.3","model2.4","model2.5","model2.6",
         "model3.1","model3.2","model3.3","model3.4",
         "model4.1")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("D:/malaria_hainan/output/pv/", lab[i],".RData"))})

# create table to store DIC
table1 <- data.frame(Model  = c(
  "base",
  "tem","pre","sun","agri",
  "tem+pre","tem+sun","tem+agri","pre+sun","pre+agri","sun+agri",
  "tem+pre+sun","tem+pre+agri","tem+sun+agri","pre+sun+agri",
  "tem+pre+sun+agri"), 
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

fwrite(table1, file = "D:/malaria_hainan/figs/table_pv01.csv", quote = FALSE, 
       row.names = FALSE) 

bestfit <- which.min(table1$DIC)
bestfit

#updata baseformula
baseformula <- Y ~ 1 +
  f(S, model = "bym2", replicate = T2, graph = "D:/malaria_hainan/map.graph", scale.model = TRUE, hyper = precision.prior) + 
  basis_sun + basis_agri +basis_pre + basis_tem

newformula1.1 <- update.formula(baseformula, ~. + basis_itns)
newformula1.2 <- update.formula(baseformula, ~. + basis_irs)
newformula1.3 <- update.formula(baseformula, ~. + basis_mda)
newformula1.4 <- update.formula(baseformula, ~. + basis_tmda)

newformula2.1 <- update.formula(baseformula, ~. + basis_itns + basis_irs)
newformula2.2 <- update.formula(baseformula, ~. + basis_itns + basis_mda)
newformula2.3 <- update.formula(baseformula, ~. + basis_itns + basis_tmda)
newformula2.4 <- update.formula(baseformula, ~. + basis_irs + basis_mda)
newformula2.5 <- update.formula(baseformula, ~. + basis_irs + basis_tmda)
newformula2.6 <- update.formula(baseformula, ~. + basis_mda + basis_tmda)

newformula3.1 <- update.formula(baseformula, ~. + basis_itns + basis_irs + basis_mda)
newformula3.2 <- update.formula(baseformula, ~. + basis_itns + basis_irs + basis_tmda)
newformula3.3 <- update.formula(baseformula, ~. + basis_itns + basis_mda + basis_tmda)
newformula3.4 <- update.formula(baseformula, ~. + basis_irs + basis_mda + basis_tmda)

newformula4.1 <- update.formula(baseformula, ~. + basis_itns + basis_irs + basis_mda + basis_tmda)

# create a list of formulas
formulas <- list(baseformula,
                 newformula1.1,newformula1.2,newformula1.3,newformula1.4,
                 newformula2.1,newformula2.2,newformula2.3,newformula2.4,newformula2.5,newformula2.6,
                 newformula3.1,newformula3.2,newformula3.3,newformula3.4,
                 newformula4.1)

# create model label string
lab <- c("basenewmodel",
         "newmodel1.1","newmodel1.2","newmodel1.3","newmodel1.4",
         "newmodel2.1","newmodel2.2","newmodel2.3","newmodel2.4","newmodel2.5","newmodel2.6",
         "newmodel3.1","newmodel3.2","newmodel3.3","newmodel3.4",
         "newmodel4.1")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas), 
                 function(i) {
                   model <- mymodel(formulas[[i]], df)
                   save(model, file = paste0("D:/malaria_hainan/output/pv/", lab[i],".RData"))})

# create table to store DIC
table1 <- data.frame(Model  = c(
  "base",
  "1","2","3","4",
  "12","13","14","23","24","34",
  "123","124","134","234",
  "1234"), 
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

fwrite(table1, file = "D:/malaria_hainan/figs/table_pv02.csv", quote = FALSE, 
       row.names = FALSE) 

bestfit <- which.min(table1$DIC)
bestfit
