# post stratification for outcome prevalence


library(sf)
library(mapview)


## Read in dhs points

ethpoly <- st_read("./data/ZA_2016_DHS_02262021_2058_24890/ZAGE71FL/ZAGE71FL.shp")

ethpoly$struct <- 1:dim(ethpoly)[1]

#plot(ethpoly["struct"])




## Read in dhs sample locations and ADM 2 regions.

# The adm2 shapefile can be found in the [Diva GIS international data repository](https://www.diva-gis.org/gdata), or from the [IPUMS International site](https://international.ipums.org/international/gis_harmonized_2nd.shtml) below I use the ADM2 level of administrative geography. 
# 
# These locations are not identified in the DHS, but by performing a spatial intersection, we can merge the DHS survey locations to the ADM 2 units
# 



eth_dots<-st_read("./data/ZA_2016_DHS_02262021_2058_24890/ZAGE71FL/ZAGE71FL.shp")
#eth_dots <- eth_dots[eth_dots$LATNUM>0,]

eth_adm1<-st_read("./data/ZAF_adm/ZAF_adm1.shp")
eth_adm1<- st_transform(eth_adm1, crs = 32734)
eth_adm2<-st_read("./data/geo3_za2016/geo3_za2016.shp")
eth_adm2$struct <- 1:dim(eth_adm2)[1]

# eth_adm2<-st_read("~/OneDrive - University of Texas at San Antonio/projects/AFRICA_ph/data/ZAF_adm/ZAF_adm2.shp")
# eth_adm2$struct <- 1:dim(eth_adm2)[1]





#merge dots to administrative data
eth_dots2000<-st_intersection(eth_dots, eth_adm2)
#eth_dots2000
names(eth_dots2000)
mapview(eth_dots2000["IPUM2016"])+mapview(eth_adm2["IPUM2016"])



library(haven)
dhs2000<-readRDS(file ="./data/saf_women.rds")
#dhs2000<-zap_labels(dhs2000)



## Merge survey data to sample locations


dhs2000m<-merge(dhs2000, eth_dots2000, by.x="v001", by.y="DHSCLUST", all.y=T)


dhs2000m$ipvany<-ifelse(dhs2000m$ipv.sex==1|dhs2000m$ipv.vio==1|dhs2000m$ipv.emot==1, 1, 0)
dhs2000m$age_cat <- cut(dhs2000m$age, breaks = 3)

library(lme4)
dhs2000m$agegrp <- as.numeric(cut(dhs2000m$age,
                                  breaks = c(15,19,24, 29, 34, 39, 44, 49)))


dhs2000m$urban <- ifelse(dhs2000m$rural ==1 , 0, 1)
dhs2000m$chil_cut <- cut(dhs2000m$chilborn, breaks = c(0,1, 2,4, 12), include.lowest = T)

dhs2000m$eth2<-ifelse(dhs2000m$eth%in%c("asian", "white"), "other", dhs2000m$eth)

# modf<-lmer(hiv_pos ~ 1+(1|IPUM2016/agegrp)+(1|agegrp)+eduprim+urban+chil_cut, weights = hpwt,  data = dhs2000m)
# mods<- glm(hiv_pos ~ 1+eduprim+urban+chil_cut, weights = hpwt, family = binomial, data = dhs2000m)
# summary(mod)

# test<-glm(nocondom ~ agegrp+eth2+edusecplus+urban+chil_cut,family=binomial, data=dhs2000m)
# vif(test)


library(rstanarm)
moda<- stan_glmer(hiv_pos ~ 1+(1|IPUM2016/agegrp)+(1|agegrp)+eth2+edusecplus+urban+chil_cut, weights = hpwt, family = binomial,
            data = dhs2000m, chains =3, cores = 3)#,
            # control = glmerControl(optimizer = c("bobyqa", "Nelder_Mead"),
            #                        optCtrl=list(maxfun=2e5)))

library(tidybayes)
summary(moda, pars = c("beta", "Sigma[agegrp:(Intercept),(Intercept)]",
                      "Sigma[IPUM2016:(Intercept),(Intercept)]",
                      "Sigma[agegrp:IPUM2016:(Intercept),(Intercept)]"
                      ),
        probs = c(0.025, 0.975),
        digits = 3)
VarCorr(moda)
summary(bayes_R2(moda, re.form = ~(1|IPUM2016/agegrp)+(1|agegrp)))


mod2<- stan_glmer(ipvany ~ 1+(1|IPUM2016/agegrp)+(1|agegrp)+eth2+edusecplus+urban+chil_cut, weights = dpwt, family = binomial, data = dhs2000m, chains =3, cores = 3)
summary(mod2,pars = c("beta"), digits = 3)
summary(bayes_R2(mod2, re.form = ~(1|IPUM2016/agegrp)+(1|agegrp)))


mod3<-stan_glmer(I(drink_freq%in%c("3_1_4week",   "4_5+week")) ~ 1+(1|IPUM2016/agegrp)+(1|agegrp)+eth2+edusecplus+urban+chil_cut, weights = pwt, family = binomial, data = dhs2000m, chains =3, cores = 3)
summary(mod3,pars = c("beta"), digits = 3)
summary(bayes_R2(mod3, re.form = ~(1|IPUM2016/agegrp)+(1|agegrp)))

summary(mod3, pars = c("beta", "Sigma[agegrp:(Intercept),(Intercept)]",
                       "Sigma[IPUM2016:(Intercept),(Intercept)]",
                       "Sigma[agegrp:IPUM2016:(Intercept),(Intercept)]"
),
probs = c(0.025, 0.975),
digits = 3)


mod4<- stan_glmer(nocondom ~ 1+(1|IPUM2016/agegrp)+(1|agegrp)+eth2+edusecplus+urban+chil_cut, weights = pwt, family = binomial, data = dhs2000m, chains =3, cores = 3)
summary(mod4,pars = c("beta"), digits = 3)
summary(mod4, pars = c("beta", "Sigma[agegrp:(Intercept),(Intercept)]",
                       "Sigma[IPUM2016:(Intercept),(Intercept)]",
                       "Sigma[agegrp:IPUM2016:(Intercept),(Intercept)]"
),
probs = c(0.025, 0.975),
digits = 3)

summary(bayes_R2(mod4, re.form = ~(1|IPUM2016/agegrp)+(1|agegrp)))

# 
# summary(bayes_R2(moda))
# summary(bayes_R2(mod2))
# summary(bayes_R2(mod3))
# summary(bayes_R2(mod4))

# summary(loo_R2(moda))
# summary(loo_R2(mod2))
# summary(loo_R2(mod3))
# summary(loo_R2(mod4))


## model tables

mods <- list(moda, mod2, mod3, mod4)
modelsummary::modelsummary(mods, statistic = c("conf.int"))

#### do partner drink model ####

library(ipumsr)
        cens<- read_ipums_ddi(ddi_file = "./data/ipumsi_00014.xml")
        censd<-read_ipums_micro(cens); censd<-zap_labels(censd)
        library(tidyverse)
        
        cens_fem_sex <- censd %>%
          mutate(agegrp = case_when(.$AGE2 == 4 ~1,
                                    .$AGE2 ==12 ~2,
                                    .$AGE2 ==13~3,
                                    .$AGE2 ==14~4, 
                                    .$AGE2 ==15~5,
                                    .$AGE2 ==16~6, 
                                    .$AGE2 ==17~7), 
                 edusecplus = ifelse(EDATTAIN%in%3:4, 1, 0), 
                 chil_cut = cut(CHBORN, breaks = c(0,1, 2,4, 12), include.lowest = T), 
                 urban = ifelse (URBAN == 2, 1, 0), 
                 eth = car::Recode(RACE, recodes =  "21='black_african';10 = 'other';54 = 'colored'; 40 = 'other'; else = NA"))%>%
          filter(SEX == 2, AGE2 %in% c(4:17), is.na(chil_cut)==F )%>%
          group_by(GEO3_ZA2016, agegrp,eth, edusecplus, chil_cut, urban)%>%
          summarise(popn = sum(PERWT))
        
        head(cens_fem_sex)
        
        names(cens_fem_sex)<-c("IPUM2016", "agegrp","eth2", "edusecplus","chil_cut", "urban", "popn")

#hiv       
lp1<- posterior_epred(moda, newdata=cens_fem_sex, allow.new.levels = T,re.form = ~ (1|IPUM2016/agegrp)+(1|agegrp))

cens_fem_sex$predhiv <- colMeans(lp1)

cens_fem_sex$ratehiv <- (cens_fem_sex$predhiv*cens_fem_sex$popn)

#ipv
lp2<- posterior_epred(mod2, newdata=cens_fem_sex, allow.new.levels = T,
                      re.form = ~ (1|IPUM2016/agegrp)+(1|agegrp))

       cens_fem_sex$predipv <-colMeans(lp2)
        
        cens_fem_sex$rateipv <- (cens_fem_sex$predipv*cens_fem_sex$popn)
        
#cage
lp3 <-  posterior_epred(mod3, newdata=cens_fem_sex, allow.new.levels = T,
                        re.form = ~ (1|IPUM2016/agegrp)+(1|agegrp))

cens_fem_sex$predcage <-colMeans(lp3)
        
        cens_fem_sex$ratecage <- (cens_fem_sex$predcage*cens_fem_sex$popn)

#condom use
lp4<-posterior_epred(mod4, newdata=cens_fem_sex, allow.new.levels = T,
                     re.form = ~ (1|IPUM2016/agegrp)+(1|agegrp))

        cens_fem_sex$predcond <-colMeans(lp4)
        
        cens_fem_sex$ratecond <- (cens_fem_sex$predcond*cens_fem_sex$popn)
        
out_a<-cens_fem_sex%>%
          group_by(IPUM2016)%>%
          summarise(ratehiv = sum(ratehiv)/sum(popn),
                     rateipv =sum(rateipv)/sum(popn),
                     ratecage = sum(ratecage)/sum(popn),
                     ratecond =sum(ratecond)/sum(popn)
                    )

head(out_a)
        
        eth_adm2$IPUM2016<-as.numeric(eth_adm2$IPUM2016)
        preddat<-left_join(eth_adm2, out_a, by =c( "IPUM2016" = "IPUM2016"))
        
        mapview(preddat["ratehiv"])+
          mapview(preddat["rateipv"])+
          mapview(preddat["ratecage"])+
          mapview(preddat["ratecond"])

pd2<-preddat; st_geometry(pd2)<-NULL   
cor(pd2[ , c("ratehiv", "rateipv", "ratecage", "ratecond")])



preddat <- st_transform(preddat, crs = 32734)

library(spdep)
nbs <- poly2nb(preddat, queen=T)
#nbs<-knn2nb(nbs)
wts<-nb2listw(nbs, style="W")



preddat$hivg <- localG(preddat$ratehiv, listw=wts)
preddat$drinkg <- localG(preddat$ratecage, listw=wts)
preddat$ipvg <- localG(preddat$rateipv, listw=wts)
preddat$condg <- localG(preddat$ratecond, listw=wts)
#preddat$hivg <- localG(preddat$ratehiv, listw=wts)

names(preddat)
pd2<-preddat; st_geometry(pd2)<-NULL   

ss<-NA

for(i in 1:10){
        ss[i]<- kmeans(pd2[, 12:15], centers = i, nstart = 3)$withinss
}

plot(ss, type= "l")


R2s <- sapply(2:10,function(k){
        Clust <- kmeans(pd2[, 12:15],centers=k,iter.max = 150)
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
out<- kmeans(pd2[, 12:15], centers = 4, nstart = 10, iter.max = 150)
out
preddat$cl <-kmeans(pd2[, 12:15], centers = 2, nstart = 3, iter.max = 150)$cluster

#pdm <- st_intersection(preddat,eth_adm1 )

#prop.table(table(pdm$NAME_1, pdm$cl), margin = 2)

#mapview(preddat, zcol ="cl")

sf::st_write(preddat,dsn =  "./data/poststrat_cl.shp", delete_dsn =T)

#rm(list=ls()); gc()        
