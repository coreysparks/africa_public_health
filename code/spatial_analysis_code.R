## libraries


library(dplyr)
library(haven)
library(survey)
library(lme4)
library(sf)
library(questionr)
library(splines)
library(car)





### DHS data


# individual recode
saf<-read_dta("../data/ZA_2016_DHS_02252021_1635_24890/ZAIR71DT/ZAIR71FL.DTA")
saf<-zap_labels(saf)

# mens recode
sam<-read_dta("../data/ZA_2016_DHS_02252021_1635_24890/ZAMR71DT/ZAMR71FL.DTA")
sam<-zap_labels(sam)


#women's health survey
saf_wh<-read_dta("../data/ZA_2016_DHS_02252021_1635_24890/ZAAH71DT/ZAAHW71FL.DTA")
saf_wh<-zap_labels(saf_wh)


# Men's health survey
saf_mh<-read_dta("../data/ZA_2016_DHS_02252021_1635_24890/ZAAH71DT/ZAAHM71FL.DTA")
saf_mh<-zap_labels(saf_mh)

##HIV data
safhiv<-read_dta("../data/ZA_2016_DHS_03182021_1437_24890/ZAAR71DT/ZAAR71FL.DTA")
safhiv<-zap_labels(safhiv)


## Merging data

#womens recode to health survey
saf2<-merge(saf, saf_wh, by=c("v001", "v002", "v003"), all.x = T)

#womens merge to hiv
saf2<-merge(saf2, safhiv, by.x=c("v001", "v002", "v003"), by.y = c("hivclust", "hivnumb" , "hivline"), all.x = T)

#mens recode to health survey
sam2<-merge(sam, saf_mh, by=c("mv001", "mv002", "mv003"), all.x = T)

#mens merge to hiv
sam2<-merge(sam2, safhiv, by.x=c("mv001", "mv002", "mv003"), by.y = c("hivclust", "hivnumb" , "hivline"), all.x = T)




### DHS Spatial data


#polygons
saf2_adm2<- st_read("../data/ZAF_adm/ZAF_adm2.shp")

saf2_adm2<- st_transform(saf2_adm2, crs = 32734 )

saf2_adm1<- st_read("../data/ZAF_adm/ZAF_adm1.shp")

saf2_adm1<- st_transform(saf2_adm1, crs = 32734 )

saf2_adm0<- st_read("../data/ZAF_adm/ZAF_adm0.shp")

saf2_adm0<- st_transform(saf2_adm0, crs = 32734 )

#psu locations
saf2_dhs_psu<- st_read("../data/ZA_2016_DHS_02262021_2058_24890/ZAGE71FL/ZAGE71FL.shp")

saf2_dhs_psu<- st_transform(saf2_dhs_psu, crs =32734 )

saf_regions<- st_read("../data/ZAF_adm/ZAF_adm1.shp")
saf_regions <- saf_regions%>%
  filter(NAME_1!= "Western Cape (isolated islands)")

saf_regions$dhs_reg <- car::Recode(saf_regions$ID_1, recodes = "1 = 2; 2 = 4; 3= 7; 4=5; 5=9; 6=8; 7=6;8=3; 10= 1")

## merge psu and adm2
saf2_sp_join <- st_intersection(saf2_adm2, saf2_dhs_psu)


## make figure 1


library(tmap); library(tmaptools)

saf2_adm2<-saf2_adm2%>%
  filter(ID_2!=313)
tmap_mode("plot")
f1<-tm_basemap("Esri.WorldGrayCanvas")+ 
  tm_shape(saf2_adm1, is.master = T)+
  tm_borders(col ="black",lwd=2, lty=1)+
  tm_text("NAME_1", bg.color="grey20", bg.alpha = .25,  auto.placement = 1,scale=1, root=4)+
  tm_shape(saf2_adm2, is.master = T, title.col="Admin")+
  tm_borders(col = "grey20", alpha = .4, lty = 3 )+
  tm_shape(saf2_dhs_psu)+
  tm_dots(size =.05)+
  tm_format(format = "World")+
  #  tm_legend(legend.show=T)+
  
  tm_scale_bar(position=c("left", "top"))+
  tm_compass(position =c("left", "top"))
f1
tmap_save(f1, filename = "../images/figure1.png" )



# figure 2

library(tmap); library(tmaptools)
library(dplyr)
library(tidyr)
library(ggplot2)
pstrat<- st_read("../poststrat_noisl.shp")
ps1<-pstrat
st_geometry(ps1)<-NULL
# pstrat<-pstrat%>%
#    filter(MUNI2016!="1991011")


ps1%>%
  group_by(NAME_1)%>%
  summarise(phiv = mean(ratehiv), pipv=mean(rateipv), pdrink = mean(ratecage), psex= mean(ratecond))%>%
  pivot_longer(cols = c(phiv, pipv, pdrink, psex))%>%
  ggplot()+
  geom_bar(aes(x = name, fill=NAME_1, y = value),
           stat="identity",
           position = "dodge")+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()

# ps1%>%
#   group_by(NAME_1)%>%
#   summarise(phiv = mean(rank(ratehiv)), pipv=mean(rank(rateipv)), pdrink = mean(rank(ratecage)), psex= mean(rank(ratecond)))%>%
#   pivot_longer(cols = c(phiv, pipv, pdrink, psex))%>%
#   ggplot()+
#   geom_bar(aes(x = name, fill=NAME_1, y = value),stat="identity", position = "dodge")



tmap_mode("plot")
f1<-tm_basemap("Esri.WorldGrayCanvas")+ 
  tm_shape(pstrat, is.master = T)+
  #tm_borders(col = "grey20", alpha = .4, lty = 3 )+
  tm_polygons("ratehiv", title="% with HIV", palette="Blues", style="jenks", n=5,legend.hist=T )+
  # tm_shape(saf2_dhs_psu)+
  # tm_dots(size =.05)+
  tm_format(format = "World", legend.outside=T)+
  tm_shape(saf2_adm1)+
  tm_borders(col ="black",lwd=2, lty=1)
# tm_text("NAME_1", bg.color="grey20", bg.alpha = .25,  auto.placement = 1,scale=1, root=4)+
#  tm_legend(legend.show=T)+

# tm_scale_bar(position=c("left", "top"))+
# tm_compass(position =c("left", "top"))

f2<-tm_basemap("Esri.WorldGrayCanvas")+ 
  tm_shape(pstrat, is.master = T)+
  #tm_borders(col = "grey20", alpha = .4, lty = 3 )+
  tm_polygons("rateipv", title="% with IPV", palette="Blues", style="jenks", n=5,legend.hist=T )+
  # tm_shape(saf2_dhs_psu)+
  # tm_dots(size =.05)+
  tm_format(format = "World", legend.outside=T)+
  tm_shape(saf2_adm1)+
  tm_borders(col ="black",lwd=2, lty=1)
# tm_text("NAME_1", bg.color="grey20", bg.alpha = .25,  auto.placement = 1,scale=1, root=4)+
#  tm_legend(legend.show=T)+

#tm_scale_bar(position=c("left", "top"))+
#tm_compass(position =c("left", "top"))

f3<-tm_basemap("Esri.WorldGrayCanvas")+ 
  tm_shape(pstrat, is.master = T)+
  #tm_borders(col = "grey20", alpha = .4, lty = 3 )+
  tm_polygons("ratecage", title="% with Problem\nDrinking", palette="Blues", style="jenks", n=5,legend.hist=T )+
  # tm_shape(saf2_dhs_psu)+
  # tm_dots(size =.05)+
  tm_format(format = "World", legend.outside=T)+
  tm_shape(saf2_adm1)+
  tm_borders(col ="black",lwd=2, lty=1)
# tm_text("NAME_1", bg.color="grey20", bg.alpha = .25,  auto.placement = 1,scale=1, root=4)+
#  tm_legend(legend.show=T)+

#  tm_scale_bar(position=c("left", "top"))+
# tm_compass(position =c("left", "top"))

f4<-tm_basemap("Esri.WorldGrayCanvas")+ 
  tm_shape(pstrat, is.master = T)+
  #tm_borders(col = "grey20", alpha = .4, lty = 3 )+
  tm_polygons("ratecond", title="% with Risky Sex", palette="Blues", style="jenks", n=5,legend.hist=T )+
  # tm_shape(saf2_dhs_psu)+
  # tm_dots(size =.05)+
  tm_format(format = "World", legend.outside=T)+
  tm_shape(saf2_adm1)+
  tm_borders(col ="black",lwd=2, lty=1)+
  # tm_text("NAME_1", bg.color="grey20", bg.alpha = .25,  auto.placement = 1,scale=1, root=4)+
  #  tm_legend(legend.show=T)+
  
  tm_scale_bar(position=c("left", "top"))+
  tm_compass(position =c("left", "top"))

f2_all<-tmap_arrange(f1, f2, f3, f4, ncol =2)

tmap_save(f2_all, filename = "../images/figure2.png" )


# figure 3

library(tmap); library(tmaptools)
pstrat<- st_read("../poststrat_cl.shp")
# pstrat<-pstrat%>%
#    filter(MUNI2016!="1991011")

pstrat <- pstrat%>%
  filter(NAME_1 != "Western Cape (isolated islands)")
ps1<-pstrat
st_geometry(ps1)<-NULL

library(spdep)
nbs <- poly2nb(pstrat, queen = T)
#nbs<-knn2nb(nbs)
wts<-nb2listw(nbs, style="B")



pstrat$hivg <- localG(pstrat$ratehiv, listw=wts)
pstrat$drinkg <- localG(pstrat$ratecage, listw=wts)
pstrat$ipvg <- localG(pstrat$rateipv, listw=wts)
pstrat$condg <- localG(pstrat$ratecond, listw=wts)

table(pstrat$hivg>3, pstrat$NAME_1)
table(pstrat$drinkg>3, pstrat$NAME_1)
table(pstrat$ipvg>3, pstrat$NAME_1)
table(pstrat$condg>3, pstrat$NAME_1)

table(pstrat$hivg< -3, pstrat$NAME_1)
table(pstrat$drinkg< -3, pstrat$NAME_1)
table(pstrat$ipvg< -3, pstrat$NAME_1)
table(pstrat$condg< -3, pstrat$NAME_1)


hivcl <- localmoran(pstrat$ratehiv, listw=wts)
hivcl_l <- as.character(attr(hivcl, "quadr")$mean)
pstrat$hivcl <- ifelse(hivcl[, 5]<.05,hivcl_l, NA)  
levels(pstrat$hivcl) <- levels(attr(hivcl, "quadr")$mean)

drinkcl <- localmoran(pstrat$ratecage, listw=wts)
drinkccl_l <- as.character(attr(drinkcl, "quadr")$mean)
pstrat$drinkccl <- ifelse(drinkcl[, 5]<.05,drinkccl_l, NA)  
levels(pstrat$drinkccl) <- levels(attr(hivcl, "quadr")$mean)

ipvcl <- localmoran(pstrat$rateipv, listw=wts)
ipvcl_l <- as.character(attr(ipvcl, "quadr")$mean)
pstrat$ipvcl <- ifelse(ipvcl[, 5]<.05,ipvcl_l, NA)  
levels(pstrat$ipvcl) <- levels(attr(hivcl, "quadr")$mean)

condcl <- localmoran(pstrat$ratecond, listw=wts)
condcl_l <- as.character(attr(condcl, "quadr")$mean)
pstrat$condcl <- ifelse(condcl[, 5]<.05,condcl_l, NA)  
levels(pstrat$condcl) <- levels(attr(hivcl, "quadr")$mean)


# pstrat$drinkcl <-  attr(localmoran(pstrat$ratecage, listw=wts), "quadr")$mean
# pstrat$ipvcl <-  attr(localmoran(pstrat$rateipv, listw=wts), "quadr")$mean
# pstrat$condcl <-  attr(localmoran(pstrat$ratecond, listw=wts), "quadr")$mean
#

MyPalette1 <- c("#CA0020", "#F4A582",  "#0571B0")
MyPalette2 <-c("#CA0020", "#F4A582", "#92C5DE", "#0571B0")

tmap_mode("plot")
f1<-tm_basemap("Esri.WorldGrayCanvas")+ 
  tm_shape(pstrat, is.master = T)+
  #tm_borders(col = "grey20", alpha = .4, lty = 3 )+
  tm_polygons("hivg", title="HIV Local Cluster")+
  # tm_shape(saf2_dhs_psu)+
  # tm_dots(size =.05)+
  tm_format(format = "World", legend.outside=T)+
  tm_shape(saf2_adm1)+
  tm_borders(col ="black",lwd=2, lty=1)

f1
# tm_text("NAME_1", bg.color="grey20", bg.alpha = .25,  auto.placement = 1,scale=1, root=4)+
#  tm_legend(legend.show=T)+

# tm_scale_bar(position=c("left", "top"))+
# tm_compass(position =c("left", "top"))

f2<-tm_basemap("Esri.WorldGrayCanvas")+ 
  tm_shape(pstrat, is.master = T)+
  #tm_borders(col = "grey20", alpha = .4, lty = 3 )+
  tm_polygons("ipvg", title="Local IPV Cluster" )+
  # tm_shape(saf2_dhs_psu)+
  # tm_dots(size =.05)+
  tm_format(format = "World", legend.outside=T)+
  tm_shape(saf2_adm1)+
  tm_borders(col ="black",lwd=2, lty=1)
# tm_text("NAME_1", bg.color="grey20", bg.alpha = .25,  auto.placement = 1,scale=1, root=4)+
#  tm_legend(legend.show=T)+

#tm_scale_bar(position=c("left", "top"))+
#tm_compass(position =c("left", "top"))

f3<-tm_basemap("Esri.WorldGrayCanvas")+ 
  tm_shape(pstrat, is.master = T)+
  #tm_borders(col = "grey20", alpha = .4, lty = 3 )+
  tm_polygons("drinkg", title="Local Problem\nDrinking Cluster")+
  # tm_shape(saf2_dhs_psu)+
  # tm_dots(size =.05)+
  tm_format(format = "World", legend.outside=T)+
  tm_shape(saf2_adm1)+
  tm_borders(col ="black",lwd=2, lty=1)
# tm_text("NAME_1", bg.color="grey20", bg.alpha = .25,  auto.placement = 1,scale=1, root=4)+
#  tm_legend(legend.show=T)+

#  tm_scale_bar(position=c("left", "top"))+
# tm_compass(position =c("left", "top"))

f4<-tm_basemap("Esri.WorldGrayCanvas")+ 
  tm_shape(pstrat, is.master = T)+
  #tm_borders(col = "grey20", alpha = .4, lty = 3 )+
  tm_polygons("condg", title="Local Risky Sex\n Cluster")+
  # tm_shape(saf2_dhs_psu)+
  # tm_dots(size =.05)+
  tm_format(format = "World", legend.outside=T)+
  tm_shape(saf2_adm1)+
  tm_borders(col ="black",lwd=2, lty=1)+
  # tm_text("NAME_1", bg.color="grey20", bg.alpha = .25,  auto.placement = 1,scale=1, root=4)+
  #  tm_legend(legend.show=T)+
  
  tm_scale_bar(position=c("left", "top"))+
  tm_compass(position =c("left", "top"))

f2_all<-tmap_arrange(f1, f2, f3, f4, ncol =2)

tmap_save(f2_all, filename = "../images/figure3.png" )


## figure 4

pstrat$hivg <- localG_perm(pstrat$ratehiv, listw=wts, nsim = 999)
pstrat$drinkg <- localG_perm(pstrat$ratecage, listw=wts, nsim = 999)
pstrat$ipvg <- localG_perm(pstrat$rateipv, listw=wts, nsim = 999)
pstrat$condg <- localG_perm(pstrat$ratecond, listw=wts, nsim = 999)


#localG_perm(pstrat$ratehiv, listw=wts, nsim = 999)

pstrat$hivg_s<-ifelse(pstrat$hivg >3.2, 1, 0)
pstrat$drinkg_s<-ifelse(pstrat$drinkg >3.2, 1, 0)
pstrat$ipvg_s<-ifelse(pstrat$ipvg >3.2, 1, 0)
pstrat$condg_s<-ifelse(pstrat$condg >3.2, 1, 0)

pstrat$sum_s <- pstrat$hivg_s + pstrat$drinkg_s + pstrat$ipvg_s + pstrat$condg_s


ps1<-pstrat
st_geometry(ps1)<-NULL
ss<-NA

for(i in 1:10){
  ss[i]<- kmeans(ps1[, 12:15], centers = i, nstart = 3)$withinss
}

plot(ss, type= "l")


R2s <- sapply(2:10,function(k){
  Clust <- kmeans(ps1[, 12:15],centers=k,iter.max = 150)
  R2 <- Clust$betweenss / Clust$totss
  return(R2)
})

Df <- data.frame(K=2:10,
                 R2 = R2s)
library(ggplot2)
ggplot(Df)+
  geom_line(aes(x=K,y=R2s))+
  geom_point(aes(x=K,y=R2s),color="red")+
  xlab("Number of groups")+
  ylab("R2 of classification")

out<- kmeans(ps1[, 12:15],
             centers = 2,
             nstart = 10,
             iter.max = 150)
out$centers

library(cluster)
out2<-pam(ps1[, 12:15],k = 2,nstart = 10)
out2$medoids

pstrat$cl <-kmeans(ps1[, 12:15],
                   centers = 2,
                   nstart = 3, 
                   iter.max = 150)$cluster

pstrat$pam<-out2$clustering

ps1<-pstrat
st_geometry(ps1)<-NULL

ps1%>%
  group_by(cl)%>%
  summarise(phiv = mean(ratehiv), pipv=mean(rateipv), pdrink = mean(ratecage), psex= mean(ratecond))%>%
  pivot_longer(cols = c(phiv, pipv, pdrink, psex))%>%
  ggplot()+
  geom_bar(aes(x = name, fill=factor(cl), y = value),
           stat="identity",
           position = "dodge")+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()

ps1%>%
  group_by(pam)%>%
  summarise(phiv = mean(ratehiv), pipv=mean(rateipv), pdrink = mean(ratecage), psex= mean(ratecond))%>%
  pivot_longer(cols = c(phiv, pipv, pdrink, psex))%>%
  ggplot()+
  geom_bar(aes(x = name, fill=factor(pam), y = value),
           stat="identity",
           position = "dodge")+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()

# pstrat$cl_lab <- ps1$clus <- ifelse(ps1$hivg==1 & ps1$drinkg==1 & ps1$ipvg ==1 & ps1$condg==1, 
#                    "All high",
#                    ifelse(ps1$hivg==1 & ps1$drinkg==1 & ps1$ipvg ==1, "HIV, Dr, IPV", 
#                           ifelse(ps1$hivg==1 & ps1$drinkg==1 , "HIV & Dr",
#                                  ifelse(ps1$drinkg==1 & ps1$ipvg ==1 & ps1$condg==1, "dr, ipv, cond", 
#                                         ifelse(ps1$drinkg==1 & ps1$ipvg ==1 , "dr & ipv", 
#                                                ifelse(ps1$ipvg ==1 & ps1$condg==1, "ipv&cond", "none"))))))
#   

# ifelse(pstrat$cl ==1, "No Indicators", 
#                      ifelse(pstrat$cl ==2, "All indicators","Drinking, IPV, Risky Sex"))
MyPalette2 <-c("#CA0020", "#F4A582", "#92C5DE", "grey80")



tmap_mode("plot")
f4<-tm_basemap("Esri.WorldGrayCanvas")+ 
  tm_shape(pstrat, is.master = T)+
  #tm_borders(col = "grey20", alpha = .4, lty = 3 )+
  tm_polygons("cl", title="Multivariate Cluster")+
  # tm_shape(saf2_dhs_psu)+
  # tm_dots(size =.05)+
  tm_format(format = "World", legend.outside=T)+
  tm_shape(saf2_adm1)+
  tm_borders(col ="black",lwd=2, lty=1)+
  tm_scale_bar(position=c("left", "top"))+
  tm_compass(position =c("left", "top"))

f4

tm_basemap("Esri.WorldGrayCanvas")+ 
  tm_shape(pstrat, is.master = T)+
  #tm_borders(col = "grey20", alpha = .4, lty = 3 )+
  tm_polygons("pam", title="Multivariate Cluster")+
  # tm_shape(saf2_dhs_psu)+
  # tm_dots(size =.05)+
  tm_format(format = "World", legend.outside=T)+
  tm_shape(saf2_adm1)+
  tm_borders(col ="black",lwd=2, lty=1)+
  tm_scale_bar(position=c("left", "top"))+
  tm_compass(position =c("left", "top"))

tmap_save(f4, filename = "../images/figure4.png" )


