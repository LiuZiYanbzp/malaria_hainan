library(ggstatsplot)
library(tidyverse)

load("D:/malaria_hainan/output/pv/newmodel1.4.RData")
model0 <- model
load("D:/malaria_hainan/output/pv/crossmodel2.4.RData")
#highest_sun
model1.1 <- model
load("D:/malaria_hainan/output/pv/crossmodel2.5.RData")
#high_sun
model1.2 <- model
load("D:/malaria_hainan/output/pv/crossmodel2.6.RData")
#low_sun
model1.3 <- model
load("D:/malaria_hainan/output/pf/newmodel1.4.RData")
model00 <- model
load("D:/malaria_hainan/output/pf/crossmodel2.4.RData")
#highest_sun
model2.1 <- model
load("D:/malaria_hainan/output/pf/crossmodel2.5.RData")
#high_sun
model2.2 <- model
load("D:/malaria_hainan/output/pf/crossmodel2.6.RData")
#low_sun
model2.3 <- model

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.25)

# create model name and label strings
mod.name <- c("model0", "model1.1", "model1.2", "model1.3", "model00", "model2.1", "model2.2", "model2.3")
lab <- c(1:8) #  this avoid plotting the intermediate scenario, to view all change final c to d

# make table to save relative risks for wet and dry scenarios 
table1 <- as.data.frame(matrix(NA, 808, 6))
colnames(table1) <- c("pla", "type", "var", "allrrfit", "allrrhigh", "allrrlow")
table1$pla <- "tmda"
table1$type[c(1:101)] <- "P.vivax-tmda Overall"
table1$type[c(102:202)] <- "P.vivax-tmda High"
table1$type[c(203:303)] <- "P.vivax-tmda Intermediate"
table1$type[c(304:404)] <- "P.vivax-tmda Low"
table1$type[c(405:505)] <- "P.falciparum-tmda Overall"
table1$type[c(506:606)] <- "P.falciparum-tmda High"
table1$type[c(607:707)] <- "P.falciparum-tmda Intermediate"
table1$type[c(708:808)] <- "P.falciparum-tmda Low"

for (j in 1:length(mod.name))
{
  
  model <- eval(parse(text = as.name(mod.name[j]))) 
  
  # extract coefficients and variance-covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # create indicators for terms associated with PDSI cross-basis
  indp <- grep("basis_tmda",model$names.fixed)
  
  # extract predictions from the PDSI DLNM centred on zero (normal conditions)
  predp <- crosspred(basis_tmda, coef = coef[indp], vcov = vcov[indp,indp],
                     model.link = "log", by = 0.01, bylag = 0.25, cen = 0)
  
  table1$var[(j*101-100):(j*101)] <- predp$predvar
  table1$allrrfit[(j*101-100):(j*101)] <- predp$allRRfit
  table1$allrrhigh[(j*101-100):(j*101)] <- predp$allRRhigh
  table1$allrrlow[(j*101-100):(j*101)] <- predp$allRRlow
  
}
tmda_table <- table1
pv_tmda_high_table <- table1[grep(pattern = "P.vivax-tmda High",table1$type),]
pv_tmda_media_table <- table1[grep(pattern = "P.vivax-tmda Intermediate",table1$type),]
pv_tmda_low_table <- table1[grep(pattern = "P.vivax-tmda Low",table1$type),]
pf_tmda_high_table <- table1[grep(pattern = "P.falciparum-tmda High",table1$type),]
pf_tmda_media_table <- table1[grep(pattern = "P.falciparum-tmda Intermediate",table1$type),]
pf_tmda_low_table <- table1[grep(pattern = "P.falciparum-tmda Low",table1$type),]

pf_table <- rbind(pf_tmda_high_table,pf_tmda_media_table,pf_tmda_low_table)
pv_table <- rbind(pv_tmda_high_table,pv_tmda_media_table,pv_tmda_low_table)
pf_tmda_table <- pf_table
pv_tmda_table <- pv_table

load("D:/malaria_hainan/output/pv/newmodel1.3.RData")
model0 <- model
load("D:/malaria_hainan/output/pv/crossmodel2.10.RData")
#highest_sun
model1.1 <- model
load("D:/malaria_hainan/output/pv/crossmodel2.11.RData")
#high_sun
model1.2 <- model
load("D:/malaria_hainan/output/pv/crossmodel2.12.RData")
#low_sun
model1.3 <- model
load("D:/malaria_hainan/output/pf/newmodel1.3.RData")
model00 <- model
load("D:/malaria_hainan/output/pf/crossmodel2.10.RData")
#highest_sun
model2.1 <- model
load("D:/malaria_hainan/output/pf/crossmodel2.11.RData")
#high_sun
model2.2 <- model
load("D:/malaria_hainan/output/pf/crossmodel2.12.RData")
#low_sun
model2.3 <- model

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.25)

# create model name and label strings
mod.name <- c("model0", "model1.1", "model1.2", "model1.3", "model00", "model2.1", "model2.2", "model2.3")
lab <- c(1:8) #  this avoid plotting the intermediate scenario, to view all change final c to d

# make table to save relative risks for wet and dry scenarios 
table1 <- as.data.frame(matrix(NA, 808, 6))
colnames(table1) <- c("pla", "type", "var", "allrrfit", "allrrhigh", "allrrlow")
table1$pla <- "mda"
table1$type[c(1:101)] <- "P.vivax-mda Overall"
table1$type[c(102:202)] <- "P.vivax-mda High"
table1$type[c(203:303)] <- "P.vivax-mda Intermediate"
table1$type[c(304:404)] <- "P.vivax-mda Low"
table1$type[c(405:505)] <- "P.falciparum-mda Overall"
table1$type[c(506:606)] <- "P.falciparum-mda High"
table1$type[c(607:707)] <- "P.falciparum-mda Intermediate"
table1$type[c(708:808)] <- "P.falciparum-mda Low"

for (j in 1:length(mod.name))
{
  
  model <- eval(parse(text = as.name(mod.name[j]))) 
  
  # extract coefficients and variance-covariance matrix
  coef <- model$summary.fixed$mean
  vcov <- model$misc$lincomb.derived.covariance.matrix
  
  # create indicators for terms associated with PDSI cross-basis
  indp <- grep("basis_mda",model$names.fixed)
  
  # extract predictions from the PDSI DLNM centred on zero (normal conditions)
  predp <- crosspred(basis_mda, coef = coef[indp], vcov = vcov[indp,indp],
                     model.link = "log", by = 0.01, bylag = 0.25, cen = 0)
  
  table1$var[(j*101-100):(j*101)] <- predp$predvar
  table1$allrrfit[(j*101-100):(j*101)] <- predp$allRRfit
  table1$allrrhigh[(j*101-100):(j*101)] <- predp$allRRhigh
  table1$allrrlow[(j*101-100):(j*101)] <- predp$allRRlow
  
}

mda_table <- table1
pv_mda_high_table <- table1[grep(pattern = "P.vivax-mda High",table1$type),]
pv_mda_media_table <- table1[grep(pattern = "P.vivax-mda Intermediate",table1$type),]
pv_mda_low_table <- table1[grep(pattern = "P.vivax-mda Low",table1$type),]
pf_mda_high_table <- table1[grep(pattern = "P.falciparum-mda High",table1$type),]
pf_mda_media_table <- table1[grep(pattern = "P.falciparum-mda Intermediate",table1$type),]
pf_mda_low_table <- table1[grep(pattern = "P.falciparum-mda Low",table1$type),]

pf_table <- rbind(pf_mda_high_table,pf_mda_media_table,pf_mda_low_table)
pv_table <- rbind(pv_mda_high_table,pv_mda_media_table,pv_mda_low_table)
pf_mda_table <- pf_table
pv_mda_table <- pv_table

plt_1 <- ggbetweenstats(
  data = pf_mda_table,
  x = type,
  y = allrrfit) + 
  # Add labels and title
  labs(
    y = "Overall relative risk") + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 15, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA)
  )

plt_2 <- ggbetweenstats(
  data = pf_tmda_table,
  x = type,
  y = allrrfit) + 
  # Add labels and title
  labs(
    y = "Overall relative risk") + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 15, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA)
  )

plt_3 <- ggbetweenstats(
  data = pv_mda_table,
  x = type,
  y = allrrfit) + 
  # Add labels and title
  labs(
    y = "Overall relative risk") + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 15, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA)
  )

plt_4 <- ggbetweenstats(
  data = pv_tmda_table,
  x = type,
  y = allrrfit) + 
  # Add labels and title
  labs(
    y = "Overall relative risk") + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 15, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA)
  )

library(showtext)
font_add('Arial','/Library/Fonts/Arial.ttf') 
showtext_auto() 

ggsave(
  filename = here::here("mda_pf.pdf"),
  plot = plt_1,
  width = 6,
  height = 4,
  device = "pdf"
)

ggsave(
  filename = here::here("tmda_pf.pdf"),
  plot = plt_2,
  width = 6,
  height = 4,
  device = "pdf"
)

ggsave(
  filename = here::here("mda_pv.pdf"),
  plot = plt_3,
  width = 6,
  height = 4,
  device = "pdf"
)

ggsave(
  filename = here::here("tmda_pv.pdf"),
  plot = plt_4,
  width = 6,
  height = 4,
  device = "pdf"
)
