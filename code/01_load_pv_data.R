#####################################load data##################################
# install INLA
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

# load INLA
library(INLA)

#  select other packages
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "dlnm", "tsModel", "hydroGOF","RColorBrewer", 
              "geofacet", "ggpubr", "ggthemes")

# install.packages
# lapply(packages, install.packages, character.only = TRUE)

# load packages
lapply(packages, library, character.only = TRUE)

# load shape file for Brazil
map <- read_sf("D:/malaria_hainan/shp/County_Poly.shp")
# dim(map)

# Create adjacency matrix
nb.map <- poly2nb(as_Spatial(map$geometry))
g.file <- "D:/malaria_hainan/map.graph"
if (!file.exists(g.file)) nb2INLA(g.file, nb.map)

grid <- read.csv("D:/malaria_hainan/shp/br_states_grid.csv")
# head(grid)

# load data
data <- DT
# head(data)

# Create lagged variables
# set maximum lag
nlag = 3

# t
lag_tem <- tsModel::Lag(data$tem, group = data$county_code, k = 0:nlag)
# p
lag_pre <- tsModel::Lag(data$pre, group = data$county_code, k = 0:nlag)
# sun
lag_sun <- tsModel::Lag(data$sun, group = data$county_code, k = 0:nlag)
# itns
lag_itns <- tsModel::Lag(data$itns, group = data$county_code, k = 0:nlag)
# irs
lag_irs <- tsModel::Lag(data$irs, group = data$county_code, k = 0:nlag)
# mda
lag_mda <- tsModel::Lag(data$mda, group = data$county_code, k = 0:nlag)
# tmda
lag_tmda <- tsModel::Lag(data$tmda, group = data$county_code, k = 0:nlag)
# agri
lag_agri <- tsModel::Lag(data$Agri, group = data$county_code, k = 0:nlag)

# total number of years
nyear <- length(unique(data$year))
# total number of states
ncounty <- length(unique(data$county_code))

lagknot = equalknots(0:nlag, 2)

# define cross-basis matrix (combining nonlinear exposure and lag functions)
basis_tem <- crossbasis(lag_tem,argvar = list(fun = "ns", knots = equalknots(data$tem, 1)), arglag = list(fun = "ns",knots = lagknot))
basis_pre <- crossbasis(lag_pre,argvar = list(fun = "ns", knots = equalknots(data$pre, 1)),arglag = list(fun = "ns",knots = lagknot))
basis_sun <- crossbasis(lag_sun,argvar = list(fun = "ns", knots = equalknots(data$sun, 1)),arglag = list(fun = "ns",knots = lagknot))
basis_itns <- crossbasis(lag_itns,argvar = list(fun = "ns"),arglag = list(fun = "ns",knots = lagknot))
basis_irs <- crossbasis(lag_irs,argvar = list(fun = "ns"),arglag = list(fun = "ns",knots = lagknot))
basis_mda <- crossbasis(lag_mda,argvar = list(fun = "ns"),arglag = list(fun = "ns",knots = lagknot))
basis_tmda <- crossbasis(lag_tmda,argvar = list(fun = "ns"),arglag = list(fun = "ns",knots = lagknot))
basis_agri <- crossbasis(lag_agri,argvar = list(fun = "ns"),arglag = list(fun = "ns",knots = lagknot))

colnames(basis_tem) = paste0("basis_tem.", colnames(basis_tem))
colnames(basis_pre) = paste0("basis_pre.", colnames(basis_pre))
colnames(basis_sun) = paste0("basis_sun.", colnames(basis_sun))
colnames(basis_itns) = paste0("basis_itns.", colnames(basis_itns))
colnames(basis_irs) = paste0("basis_irs.", colnames(basis_irs))
colnames(basis_mda) = paste0("basis_mda.", colnames(basis_mda))
colnames(basis_tmda) = paste0("basis_tmda.", colnames(basis_tmda))
colnames(basis_agri) = paste0("basis_agri.", colnames(basis_agri))

# create indices for INLA models
# note: for INLA models an index should start with 1 and with the max value equal to the length of unique values

# create microregion index 
data$index <- rep(1:ncounty, nyear)

# create state index
# state length
k <- unique(data$county_code)

for (j in 1:ncounty)
{
  data$county_index[data$county_code == k[j]] <- j 
}

# create year index
# set first year
data$year_index <- data$year - 1958 

# set up data and priors for INLA model

# set data for models
Y   <- data$pv_case
N   <- length(Y)
E   <- data$test_population
T1  <- data$year
T2  <- data$year_index
S   <- data$county_index

# create dataframe for model testing
df <- data.frame(Y, E, T1, T2, S)

# define priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)))

# inla model function

# include formula and set defaults for data, family (to allow other prob dist models e.g. Poisson) and config (to allow for sampling)
mymodel <- function(formula, data = df, family = "nbinomial", config = FALSE)
  
{
  model <- inla(formula = formula, data = data, family = family, offset = log(E),
                control.inla = list(strategy = 'adaptive'), 
                control.compute = list(dic = TRUE, config = config, 
                                       cpo = TRUE, return.marginals = FALSE),
                control.fixed = list(correlation.matrix = TRUE, 
                                     prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE), 
                verbose = FALSE)
  model <- inla.rerun(model)
  return(model)
}
