library(readxl)
#library(albersusa)
library(biscale)
library(sf)
library(tidyverse)
library(hrbrthemes)
library(ggtext)
library(scatterpie)
library(ggnewscale)
library(ggplot2)
library(plyr)
library("maptools")
library(scatterpie)
library(ggmap)
library(ggspatial)
library("ggsci")

data$case_inc <- data$case*10000/data$population
case_inc <- aggregate(data$case_inc, by=list(type=data$county),mean,na.rm = T)
case_inc
names(case_inc)[1] <- "county"
names(case_inc)[2] <- "caseinc"


pv_inc <- aggregate(data$pv_inc, by=list(type=data$county),mean,na.rm = T)
pv_inc
pf_inc <- aggregate(data$pf_inc, by=list(type=data$county),mean,na.rm = T)
pf_inc
data$mix_inc <- data$mix/data$test_population
mix_inc <- aggregate(data$mix_inc, by=list(type=data$county),mean,na.rm = T)
mix_inc
county <- aggregate(data$county_code, by=list(type=data$county),mean,na.rm = T)
county
county$code <- county$x
county$county <- county$type
county <- county[,-c(1,2)]
pv_inc <- cbind(county,pv_inc)
pf_inc <- cbind(county,pf_inc)
mix_inc <- cbind(county,mix_inc)
pv_inc$pvinc <- pv_inc$x
pf_inc$pfinc <- pf_inc$x
mix_inc$mixinc <- mix_inc$x
pv_inc <- pv_inc[,-c(3,4)]
pf_inc <- pf_inc[,-c(3,4)]
mix_inc <- mix_inc[,-c(3,4)]
case_inc <- cbind(county$code,case_inc)
names(case_inc)[1] <- "code"
inc_rate <- cbind(pv_inc,pf_inc$pfinc,mix_inc$mixinc)
names(inc_rate)[4] <- "pfinc"
names(inc_rate)[5] <- "mixinc"
#inc_rate$rate <- inc_rate$pvinc/inc_rate$pfinc
inc_rate <- inc_rate%>% mutate(allinc = pvinc+pfinc+mixinc)
long <- c(110.7703,110.2507,109.7119,109.9836,109.4018,110.3081,110.0504,109.0005,109.3460,110.3984,
          109.8133,108.8394,110.2779,109.0657,109.5101,109.6402,109.9366,109.4193)
lat <- c(19.74602,20.00657,19.80154,19.68675,19.56534,19.48539,19.34333,19.21361,19.20865,19.20974,
         19.01932,18.99074,18.83408,18.65932,18.76157,18.58254,18.56821,18.38864)
jw <- data.frame(long,lat)
inc_rate <- cbind(inc_rate,jw)
map_inc <- full_join(map, inc_rate, by = c("countyind" = "code"))

hainan_map=rgdal::readOGR("D:/malaria_hainan/shp/County_Poly.shp")
hainan_map_frame <- fortify(hainan_map)
ggplot(hainan_map_frame,aes(x=long,y=lat,group=group))+
  geom_polygon(fill="white",colour="black")+
  coord_map("polyconic")+
  theme(
    panel.grid=element_blank(),
    panel.background=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    legend.position=c(0.2,0.3)
  )
interim <- hainan_map@data
interim <-data.frame(interim,id=seq(0:17)-1)
interim$id <- as.character(interim$id)
hainan_map_data<-full_join(interim,hainan_map_frame,by = c("id" = "id"))
hainan_heatmap_data<-full_join(hainan_map_data,case_inc,by = c("countyind" = "code"))
ggplot(hainan_heatmap_data,aes(x=long,y=lat,group=group,fill=caseinc))+
  geom_polygon(colour="grey40")+
  scale_fill_gradient(low="#f2f2f2",high="#a6a6a6")+
  coord_map("polyconic")+
  theme(
    panel.grid=element_blank(),
    panel.background=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    legend.position='right'
  )

midpos <- function(x) mean(range(x,na.rm=TRUE))
centres <- ddply(hainan_map_data,.(countyind),colwise(midpos,.(long,lat)))
mapdata<-merge(centres,inc_rate,by.x="countyind",by.y="code",all.y=TRUE)
mapdata$county<-as.factor(mapdata$county)
value<-names(mapdata)[5:7]
ggplot(hainan_map_data,aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="grey") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  # spatial-aware automagic north arrow
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering)+
  geom_scatterpie(data=mapdata,aes(x=long.y, y=lat.y,group=county,r=allinc*1.4),cols=value,color=NA, alpha=.8) +
  coord_equal()+
  scale_fill_manual(values = c('#2c8bbe','#b50f63','#E64B35FF'))+
  geom_scatterpie_legend(mapdata$allinc*1.5, n = 4, x = 110.5,y=18.25,
                         labeller=function(x) x/100) +
  theme_nothing(legend=TRUE)

library(tidyverse)
library(tidytext)
library(ggtext)
library(lubridate)
require(ggplot2)
library(gtable)
library(grid)
library(ggsci)
library(reshape2)

plot <- read.csv("D:/malaria_hainan/data/province.csv", header = T)
incidence <- plot[,c(1,13)]
incidence$plasmodium <- "Malaria incidence"

pv <- plot[,c(1,6)]
pf <- plot[,c(1,7)]
pm <- plot[,c(1,8)]
mix <- plot[,c(1,9)]

names(pv) <- c("year", "rate")
names(pf) <- c("year", "rate")
names(pm) <- c("year", "rate")
names(mix) <- c("year", "rate")

pv$pla <- "Plasmodium vivax"
pf$pla <- "Plasmodium falciparum"
pm$pla <- "Plasmodium malariae"
mix$pla <- "Mixed"

plot <- rbind(pv,pf,pm,mix)

pdf("D:/malaria_hainan/figs/Fig 1_a.pdf", width=20, height=8)
ggplot()+
  geom_col(data = incidence,aes(year,incidence,fill=plasmodium),width = 0.9)+
  scale_fill_manual(values = c('#AD002A99'))+
  geom_point(data = plot,
             aes(year,rate*5000,
                 color=pla,group=pla),
             size=3.5)+
  geom_smooth(data = plot,
              aes(year,rate*5000,
                  color=pla,group=pla),
              linewidth = 2,
              method = "gam")+
  scale_color_manual(values = c('#E64B35FF','#2c8bbe','#91D1C2FF','#b50f63'))+
  labs(x='Year',y='Clinical morbidity (1/10,000)')+
  theme_test(base_size = 20)+
  theme(legend.position = 'none',
        panel.border = element_rect(size=1,fill = 'transparent'),
        axis.text = element_text(color='black'))+
  geom_rect(aes(xmin=2002,xmax=2004,ymin=905,ymax=915),
            fill='#b50f63',color='#b50f63')+
  geom_rect(aes(xmin=2002,xmax=2004,ymin=875,ymax=885),
            fill='#91D1C2FF',color='#91D1C2FF')+
  geom_rect(aes(xmin=2002,xmax=2004,ymin=845,ymax=855),
            fill='#2c8bbe',color='#2c8bbe')+
  geom_rect(aes(xmin=2002,xmax=2004,ymin=815,ymax=825),
            fill='#E64B35FF',color='#E64B35FF')+
  geom_rect(aes(xmin=2002,xmax=2004,ymin=780,ymax=800),
            fill='#AD002A99',color='#AD002A99')+
  annotate(geom='text',x=2008,y=910,label='P.vivax',size=4.5)+
  annotate(geom='text',x=2008,y=880,label='P.malariae',size=4.5)+
  annotate(geom='text',x=2008,y=850,label='P.falciparum',size=4.5)+
  annotate(geom='text',x=2008,y=820,label='Mixed infections',size=4.5)+
  annotate(geom='text',x=2008,y=790,label='Clinical morbidity',size=4.5)+
  scale_y_continuous(limits = c(0,500),
                     breaks = seq(0,500,50),
                     expand = c(0,0),
                     sec.axis = sec_axis(~./50,
                                         name = 'Blood positive for Malaria infection (%)',
                                         breaks = seq(0,10,2)
                                         ))+
  geom_vline(xintercept = 1959,lty=3,lwd=1)+
  geom_vline(xintercept = 1985,lty=3,lwd=1)
dev.off()
