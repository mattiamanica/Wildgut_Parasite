################################################################################
# This R script is distributed for scientifi purpose 
# We do not provide ANY WARRANTY, not even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# Code to fit regression analysis for
# "Loss of protozoan and metazoan intestinal symbiont biodiversity 
# in wild primates living in unprotected forests"
# by Barelli et al
#
# developer: Mattia Manica, mattia.manica@fmach.it

## graphic function #####
mytheme_classic <- function (base_size = 12, base_family = "") 
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme( axis.line = element_line(colour = "black"), 
           panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
           strip.background = element_rect(colour = "black", 
                                           size = 1), legend.key = element_blank())
}

# set random number 
set.seed(123456)

# import database
library(xlsx)
library(tidyverse)
db0 <- read.xlsx(file = "WILDGUT_data_parasites.xlsx",
                 sheetName = "EPG_Parasites_WILDGUT",
                 colClasses = c("numeric","character","character","character",
                                "numeric","numeric","numeric","character",
                                "Date","character","character",
                                rep("numeric",11)),
                 stringsAsFactors =TRUE)

str(db0)
nrow(db0)

levels(db0$Forest)[3] <- levels(db0$Forest)[2] 

table(db0$Species,db0$Forest)
table(db0$Group,db0$Forest)

db0$Date_col <- NULL
db0$Date_num <- NULL

# confronto della carica parassitaria, 
# ricchezza (tipo di parassiti per campione) 
# tra le due specie di scimmia (babbuini e colobi) 
# che vivono nella stessa foresta e 
# tra foreste diverse (una (MAGOMBERA) piccola, degradata e altamente antropizzata, mentre l'altra (MWANIHANA) più estesa e intatta).

names(db0)
db0$Forest2 = with(db0, factor(Forest, levels = rev(levels(db0$Forest))))

dbtab <-db0 %>% group_by(Species,Forest2) %>% 
  summarise(Iodamoeba      = 100*mean(EPG_Iodamoeba.>0),
            Entamoeba.coli = 100*mean(EPG_Entamoeba.coli.>0),
            Entamoeba      = 100*mean(EPG_Entamoeba.sp.>0),
            Blastocystis   = 100*mean(EPG_Blastocystis>0),
            Balantoiedes   = 100*mean(EPG_B..coli>0),
            Dicrocelid     = 100*mean(EPG_Dicrocelid>0),
            Strongyloides  = 100*mean(EPG_Strongyloides>0),
            Trichuris      = 100*mean(EPG_Trichuris>0),
            Strongylid     = 100*mean(EPG_Strongylid>0),
            Spirurid       = 100*mean(EPG_Spirurid>0)) %>%
  gather(key = "Symbionts", value = "Prevalence",-Species,-Forest2)


dbtab2 <-db0 %>% mutate(Forest2 = "Overall") %>% group_by(Species,Forest2) %>% 
  summarise(Iodamoeba      = 100*mean(EPG_Iodamoeba.>0),
            Entamoeba.coli = 100*mean(EPG_Entamoeba.coli.>0),
            Entamoeba      = 100*mean(EPG_Entamoeba.sp.>0),
            Blastocystis   = 100*mean(EPG_Blastocystis>0),
            Balantoiedes   = 100*mean(EPG_B..coli>0),
            Dicrocelid     = 100*mean(EPG_Dicrocelid>0),
            Strongyloides  = 100*mean(EPG_Strongyloides>0),
            Trichuris      = 100*mean(EPG_Trichuris>0),
            Strongylid     = 100*mean(EPG_Strongylid>0),
            Spirurid       = 100*mean(EPG_Spirurid>0)) %>%
  gather(key = "Symbionts", value = "Prevalence",-Species,-Forest2)



dbtab
print(dbtab,n=50)

dbtab3 <- bind_rows(dbtab,dbtab2)

dbtab3 %>% ggplot(., aes(x= Species, y=Prevalence,fill=Forest2))+
  facet_wrap(~Symbionts ) + geom_bar(stat = "identity",col="black",position = position_dodge())+
  ylim(c(0,100))+
  coord_flip()+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values = c("white","grey90","grey50"))




hi <- function(x){
  n = length(x)
  p = mean(x>0)
  p + 1.96*sqrt(p*(1-p)/n)
}


dbhi <-db0 %>% group_by(Species,Forest2) %>% 
  summarise(Iodamoeba      = 100*hi(EPG_Iodamoeba.>0),
            Entamoeba.coli = 100*hi(EPG_Entamoeba.coli.>0),
            Entamoeba      = 100*hi(EPG_Entamoeba.sp.>0),
            Blastocystis   = 100*hi(EPG_Blastocystis>0),
            Balantoiedes   = 100*hi(EPG_B..coli>0),
            Dicrocelid     = 100*hi(EPG_Dicrocelid>0),
            Strongyloides  = 100*hi(EPG_Strongyloides>0),
            Trichuris      = 100*hi(EPG_Trichuris>0),
            Strongylid     = 100*hi(EPG_Strongylid>0),
            Spirurid       = 100*hi(EPG_Spirurid>0)) %>%
  gather(key = "Symbionts", value = "hi",-Species,-Forest2)


dbhi2 <-db0 %>% mutate(Forest2 = "Overall") %>% group_by(Species,Forest2) %>% 
  summarise(Iodamoeba      = 100*hi(EPG_Iodamoeba.>0),
            Entamoeba.coli = 100*hi(EPG_Entamoeba.coli.>0),
            Entamoeba      = 100*hi(EPG_Entamoeba.sp.>0),
            Blastocystis   = 100*hi(EPG_Blastocystis>0),
            Balantoiedes   = 100*hi(EPG_B..coli>0),
            Dicrocelid     = 100*hi(EPG_Dicrocelid>0),
            Strongyloides  = 100*hi(EPG_Strongyloides>0),
            Trichuris      = 100*hi(EPG_Trichuris>0),
            Strongylid     = 100*hi(EPG_Strongylid>0),
            Spirurid       = 100*hi(EPG_Spirurid>0)) %>%
  gather(key = "Symbionts", value = "hi",-Species,-Forest2)


dbhi3 <- bind_rows(dbhi,dbhi2)


dbtab3$hi <- dbhi3$hi



lo <- function(x){
  n = length(x)
  p = mean(x>0)
  p - 1.96*sqrt(p*(1-p)/n)
}


dblo <-db0 %>% group_by(Species,Forest2) %>% 
  summarise(Iodamoeba      = 100*lo(EPG_Iodamoeba.>0),
            Entamoeba.coli = 100*lo(EPG_Entamoeba.coli.>0),
            Entamoeba      = 100*lo(EPG_Entamoeba.sp.>0),
            Blastocystis   = 100*lo(EPG_Blastocystis>0),
            Balantoiedes   = 100*lo(EPG_B..coli>0),
            Dicrocelid     = 100*lo(EPG_Dicrocelid>0),
            Strongyloides  = 100*lo(EPG_Strongyloides>0),
            Trichuris      = 100*lo(EPG_Trichuris>0),
            Strongylid     = 100*lo(EPG_Strongylid>0),
            Spirurid       = 100*lo(EPG_Spirurid>0)) %>%
  gather(key = "Symbionts", value = "lo",-Species,-Forest2)


dblo2 <-db0 %>% mutate(Forest2 = "Overall") %>% group_by(Species,Forest2) %>% 
  summarise(Iodamoeba      = 100*lo(EPG_Iodamoeba.>0),
            Entamoeba.coli = 100*lo(EPG_Entamoeba.coli.>0),
            Entamoeba      = 100*lo(EPG_Entamoeba.sp.>0),
            Blastocystis   = 100*lo(EPG_Blastocystis>0),
            Balantoiedes   = 100*lo(EPG_B..coli>0),
            Dicrocelid     = 100*lo(EPG_Dicrocelid>0),
            Strongyloides  = 100*lo(EPG_Strongyloides>0),
            Trichuris      = 100*lo(EPG_Trichuris>0),
            Strongylid     = 100*lo(EPG_Strongylid>0),
            Spirurid       = 100*lo(EPG_Spirurid>0)) %>%
  gather(key = "Symbionts", value = "lo",-Species,-Forest2)


dblo3 <- bind_rows(dblo,dblo2)

dbtab3$lo <- dblo3$lo

dbtab3$lo[dbtab3$lo <0] <- 0
dbtab3$hi[dbtab3$hi >100] <- 100
levels(dbtab3$Species) <- c("Yellow\n baboon","Udzungwa\n red colobus")
dbtab3$Forest <- dbtab3$Forest2
dbtab3$Symbionts <- factor(dbtab3$Symbionts, levels = unique(dbtab3$Symbionts))
levels(dbtab3$Symbionts) <- c("Iodamoeba buetschlii","Entamoeba coli",
                              "Entamoeba sp.","Blastocystis sp.",
                              "Balantioides sp.","dicrocoeliid trematodes",
                              "Strongyloides sp.","Trichuris sp.",
                              "strongylid nematodes","spirurid nematodes")


dbtab3$Split <- dbtab3$Symbionts
levels(dbtab3$Split)   <- c("Protozoans","Protozoans","Protozoans","Protozoans","Protozoans",
                            "Metazoans","Metazoans","Metazoans","Metazoans","Metazoans")





library(fishualize)
library(viridis)
ggplot(dbtab3, aes(x= Species, y=Prevalence,fill=Forest))+
  facet_wrap(~Symbionts ,nrow=2,ncol=5) + 
  geom_bar(stat = "identity",col="black",position = position_dodge())+
  geom_errorbar(aes(x= Species, ymin=lo, ymax=hi),position = position_dodge(width=0.9),width=0.2)+
  ylim(c(0,100))+xlab("")+
  coord_flip()+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #scale_fill_fish( option = "Hypsypops_rubicundus", discrete = TRUE)+
  #scale_fill_manual(values = c("#33FF33","#00FFFF","#CCCCCC"))+
  scale_fill_viridis_d(begin=0.5,direction=-1)+
  theme(legend.position = "top")+ylab("Prevalence (%)")


ggsave("Figure2.tiff",dpi=300,width = 8,height=5)
ggsave("Figure2.pdf",dpi=300,width = 8,height=5)




db0$Richness2 <- apply(db0[,c(11:20)]>0,1,sum)
db0$Richness  <- apply(db0[,c(11:16)]>0,1,sum)
for(i in 1:nrow(db0)){
  p = (db0[i,c(11:16)]/sum(db0[i,c(11:16)]))
  db0$simps[i] <- sum(p^2)
}
for(i in 1:nrow(db0)){
  p <- (db0[i,c(11:20)]/sum(db0[i,c(11:20)]))
  db0$shan[i] <- -sum(p[p>0]*log(p[p>0]))
}
plot(db0$simps)
hist(db0$simps)
mean(db0$simps==0)

plot(db0$shan)
hist(db0$shan)
mean(db0$simps==0)


plot(db0$shan,db0$simps)
plot(db0$Richness,db0$simps)



db0$AltitudeSD <- (db0$Altitude - mean(db0$Altitude))/sd(db0$Altitude)
db0$Species <- relevel(db0$Species, ref = "Colobus")
db0$Forest_Species <- interaction(db0$Forest,db0$Species)# per dopo

db0s <- subset(db0,!is.na(db0$simps))
m0 <- lm(simps~Forest_Species , data=db0s)
m0 <- lm(simps~Forest*Species , data=db0s)
summary(m0)
plot(m0)

library(multcomp)
m3 <- glht(m0, linfct = mcp(Forest_Species = "Tukey"))
m3
#tiff("PairWise_v2.tiff", width = 6, height = 4, units = 'in', res = 300,compression = 'lzw')
pdf("FigureS4.pdf", width = 6, height = 4)
par(mar=c(3.5,9,2,2))
plot(m3,main="",xlab="")
mtext(text="95% family-wise confidence level",side=3,line=0.5,font=2,cex=1.2)
mtext(text="Linear Function",side=1,line=2,font=1,cex=1.2)
dev.off()







nrow(db0)
table(db0$Richness==0)
100*mean(db0$Richness==0)
table(db0$Richness==0,db0$Species)
100*tapply(db0$Richness==0,db0$Species,mean)


# full model richness
m0 <- glm(Richness~Forest*Species + AltitudeSD,family=poisson, data=db0)
m1 <- glm(Richness~Forest*Species ,family=poisson, data=db0)
AIC(m0,m1)

# model validation
sum(resid(m1,type="pearson")^2)/(nrow(db0)-length(coef(m1)))
plot(resid(m1,type="pearson"))
plot(fitted(m1),resid(m1,type="pearson"))
plot(db0$Species,resid(m1,type="pearson"))
plot(db0$Forest,resid(m1,type="pearson"))
plot(db0$Forest_Species,resid(m1,type="pearson"))
plot(db0$Altitude,resid(m1,type="pearson"))


# interpretation
# Colobus from MA to MW
exp(coef(m0)["ForestMW"])
exp(coef(m1)["ForestMW"])*sqrt(vcov(m1)[2,2])
exp(coef(m1))*sqrt(diag(vcov(m1)))
exp(coef(m1))
exp(confint.default(m1))

#db0$Forest <- relevel(db0$Forest, ref = "MW")
#db0$Species <- relevel(db0$Species, ref = "Baboon")
#db0$Species <- relevel(db0$Species, ref = "Colobus")

#m1 <- glm(Richness~Forest*Species,family=poisson, data=db0)
#exp(coef(m1))
#exp(confint.default(m1))

#db0$Forest <- relevel(db0$Forest, ref = "MA")



# multiple comparison (pairwise)
levels(db0$Forest_Species) <- c("MA|RC", "MW|RC","MA|YB","MW|YB")

m2 <- glm(Richness~Forest_Species ,family=poisson, data=db0)
summary(m2)


library(multcomp)
m3 <- glht(m2, linfct = mcp(Forest_Species = "Tukey"))
m3
#tiff("PairWise_v2.tiff", width = 6, height = 4, units = 'in', res = 300,compression = 'lzw')
pdf("Figure4.pdf", width = 6, height = 4)
par(mar=c(3.5,9,2,2))
plot(m3,main="",xlab="")
mtext(text="95% family-wise confidence level",side=3,line=0.5,font=2,cex=1.2)
mtext(text="Linear Function",side=1,line=2,font=1,cex=1.2)
dev.off()


X <- expand.grid(Forest_Species = levels(db0$Forest_Species))
X
Xmat <- model.matrix(~Forest_Species,data=X)
beta = coef(m2)

mu <- Xmat%*%beta
X$lambda <- exp(mu)
sd <- sqrt(diag(Xmat%*%vcov(m2)%*%t(Xmat)))
X$hi <- exp(mu+1.96*sd)
X$lo <- exp(mu-1.96*sd)
X


#tiff("Histogram_v3.tiff", width = 7, height = 10, units = 'in', res = 300,compression = 'lzw')
pdf("Figure3.pdf", width = 5, height = 6)
par(mfrow=c(2,1))
counts <- table(subset(db0,Species=="Colobus")$Forest,subset(db0,Species=="Colobus")$Richness)
counts <-cbind(as.matrix(counts),matrix(0,ncol=2,nrow=2))
colnames(counts)[5:6] <- c("4","5")
par(mar=c(1,4,0,1))
df.bar <- barplot(as.matrix(counts), ylab="",
                  xlab="", col=c("dodgerblue","orange"),
                  beside=TRUE,ylim=c(0,29))
mtext("Number of samples",side=2,line=2.5,cex=1.,font=2)
legend("right", rownames(counts),fill = c("dodgerblue","orange"), bty = "n")
df.bar
text(1,27,"A",cex=1)
text(15,27,substitute(italic("Procolobus gordonorum")),cex=1)
points(1.5+3*X$lambda[1:2],c(22,23),pch=19,col=c("dodgerblue","orange"))
segments(x0 = 1.5+3*X$lo[1], y0=22, x1 = 1.5+3*X$hi[1], y1 = 22,col="dodgerblue",lwd=2)
#segments(x0 = 3+X$lambda[1], y0=0, x1 = 3+X$lambda[1], y1 = 23,col="dodgerblue",lty="dashed")
segments(x0 = 1.5+3*X$lo[2], y0=23, x1 = 1.5+3*X$hi[2], y1 = 23,col="orange",lwd=2)
#segments(x0 = 3+X$lambda[2], y0=0, x1 = 3+X$lambda[2], y1 = 23,col="orange",lty="dashed")

counts <- table(subset(db0,Species=="Baboon")$Forest,subset(db0,Species=="Baboon")$Richness)
par(mar=c(4,4,0,1))
df.bar <- barplot(as.matrix(counts), ylab="",
                  xlab="", col=c("dodgerblue","orange"),
                  beside=TRUE,ylim=c(0,29))
df.bar
mtext("Host Parasite Richness",side=1,line=2.5,cex=1.,font=2)
mtext("Number of samples",side=2,line=2.5,cex=1.,font=2)
text(1,25,"B",cex=1)
text(15,25,substitute(italic("Papio cynocephalus")),cex=1)
points(1.5+3*X$lambda[3:4],c(22,23),pch=19,col=c("dodgerblue","orange"))
segments(x0 = 1.5+3*X$lo[3], y0=22, x1 = 1.5+3*X$hi[3], y1 = 22,col="dodgerblue",lwd=2)
segments(x0 = 1.5+3*X$lo[4], y0=23, x1 = 1.5+3*X$hi[4], y1 = 23,col="orange",lwd=2)
dev.off()


# single species ###########???

names(db0)
round(apply(db0[,11:20],2,mean),2)
round(apply(db0[,11:20],2,sum),2)
round(apply(db0[,11:20],2,median),2)

round(tapply(db0$EPG_Iodamoeba.>0,db0$Forest_Species,mean)*100,1)
round(tapply(db0$EPG_Iodamoeba.>0,db0$Species,mean)*100,1)

round(tapply(db0$EPG_Entamoeba.coli.>0,db0$Forest_Species,mean)*100,1)
round(tapply(db0$EPG_Entamoeba.coli.>0,db0$Species,mean)*100,1)

round(tapply(db0$EPG_Entamoeba.sp.>0,db0$Forest_Species,mean)*100,1)
round(tapply(db0$EPG_Entamoeba.sp.>0,db0$Species,mean)*100,1)

round(tapply(db0$EPG_Blastocystis>0,db0$Forest_Species,mean)*100,1)
round(tapply(db0$EPG_Blastocystis>0,db0$Species,mean)*100,1)

round(tapply(db0$EPG_Dicrocelid>0,db0$Forest_Species,mean)*100,1)
round(tapply(db0$EPG_Dicrocelid>0,db0$Species,mean)*100,1)





# EPG_Strongyloides #############
hist(db0$EPG_Strongyloides)
mean(db0$EPG_Strongyloides==0)

ggplot(db0)+geom_histogram(aes(log(db0$EPG_Strongyloides+1)))+facet_wrap(~Forest_Species)


db0$PA <- if_else(db0$EPG_Strongyloides>0,1,0)

s1 <- glm(PA~Species*Forest+AltitudeSD,family=binomial,data=db0)
s0 <- glm(PA~Species*Forest,family=binomial,data=db0)
AIC(s1,s0)
summary(s0)
library(broom)
out1 <- tidy(s0)





# egg intensity LM #######
db0p <- filter(db0,EPG_Strongyloides>0)
db0p$logS <- log(db0p$EPG_Strongyloides)

s0 <- lm(logS~Species+Forest,data=db0p)
s1 <- lm(logS~Species*Forest+AltitudeSD,data=db0p)

AIC(s0,s1)
summary(s1)

egs1 <- bind_cols(data.frame(Species = rep("Strongyloides",5) ),tidy(s1))

# egg shedding intensity EPG_Strongyloides ##########
db0%>% filter(EPG_Strongyloides>0) %>% 
  group_by(Species) %>%
  summarise(mean(EPG_Strongyloides),
            median(EPG_Strongyloides),
            quantile(EPG_Strongyloides,0.25),
            quantile(EPG_Strongyloides,0.75))%>%as.data.frame()


db0%>% filter(EPG_Strongyloides>0) %>% 
  group_by(Forest_Species) %>%
  summarise(mean(EPG_Strongyloides),
            median(EPG_Strongyloides),
            quantile(EPG_Strongyloides,0.25),
            quantile(EPG_Strongyloides,0.75))%>%as.data.frame()


# EPG_Trichuris #########

db0$PA <- if_else(db0$EPG_Trichuris>0,1,0)
db0p <- filter(db0,EPG_Trichuris>0)

s1 <- glm(PA~Species*Forest+AltitudeSD,family=binomial,data=db0)
s0 <- glm(PA~Species*Forest,family=binomial,data=db0)
AIC(s0,s1)
summary(s0)
library(broom)
out2 <- tidy(s0)


# EPG_Strongylid ##############

db0$PA <- if_else(db0$EPG_Strongylid>0,1,0)
db0p <- filter(db0,EPG_Strongylid>0)

s1 <- glm(PA~Species*Forest+AltitudeSD,family=binomial,data=db0)
s0 <- glm(PA~Species*Forest,family=binomial,data=db0)
AIC(s0,s1)
summary(s0)
library(broom)
out3 <- tidy(s0)

# EPG_B..coli ###########

db0$PA <- if_else(db0$EPG_B..coli>0,1,0)
db0p <- filter(db0,EPG_B..coli>0)

s1 <- glm(PA~Species*Forest+AltitudeSD,family=binomial,data=db0)
s0 <- glm(PA~Species*Forest,family=binomial,data=db0)
AIC(s0,s1)
summary(s0)
library(broom)
out4 <- tidy(s0)


# egg shedding intensity  EPG_B..coli ##########


db0%>% filter(EPG_B..coli>0) %>% 
  group_by(Species) %>%
  summarise(mean(EPG_B..coli),
            median(EPG_B..coli),
            quantile(EPG_B..coli,0.25),
            quantile(EPG_B..coli,0.75))%>%as.data.frame()


db0%>% filter(EPG_B..coli>0) %>% 
  group_by(Forest_Species) %>%
  summarise(mean(EPG_B..coli),
            median(EPG_B..coli),
            quantile(EPG_B..coli,0.25),
            quantile(EPG_B..coli,0.75))%>%as.data.frame()

db0p$logS <- log(db0p$EPG_B..coli)

s0 <- lm(logS~Species*Forest,data=db0p)
s1 <- lm(logS~Species*Forest+AltitudeSD,data=db0p)
AIC(s0,s1)
summary(s0)

library(broom)
library(tidyverse)

egs2 <- bind_cols(data.frame(Species = rep("Balantioides",4) ),tidy(s0))

finalegg <- bind_rows(egs1,egs2)

write.table(finalegg,quote = FALSE,row.names = FALSE,sep="|",
            file="eggs_final.txt")



# EPG_Spirurid ##########

db0$PA <- if_else(db0$EPG_Spirurid>0,1,0)
db0p <- filter(db0,EPG_Spirurid>0)

s1 <- glm(PA~Species*Forest+AltitudeSD,family=binomial,data=db0)
s0 <- glm(PA~Species*Forest,family=binomial,data=db0)
summary(s0)
AIC(s0,s1)
library(broom)
out5 <- tidy(s0)

library(tidyverse)
final <- bind_rows(out1,out2,out3,out4,out5)

write.table(final,quote = FALSE,row.names = FALSE,sep="|",
            file="PA_final.txt")






########################################
########################################
########################################
################# all species #######################
########################################
########################################
########################################

# full model richness
m0 <- glm(Richness2~Forest*Species + AltitudeSD,family=poisson, data=db0)
m1 <- glm(Richness2~Forest*Species ,family=poisson, data=db0)
AIC(m0,m1)
summary(m1)

# model validation
sum(resid(m1,type="pearson")^2)/(nrow(db0)-length(coef(m1)))
plot(resid(m1,type="pearson"))
plot(fitted(m1),resid(m1,type="pearson"))
plot(db0$Species,resid(m1,type="pearson"))
plot(db0$Forest,resid(m1,type="pearson"))
plot(db0$Forest_Species,resid(m1,type="pearson"))
plot(db0$Altitude,resid(m1,type="pearson"))


# interpretation
# Colobus from MA to MW
exp(coef(m0)["ForestMW"])
exp(coef(m1)["ForestMW"])*sqrt(vcov(m1)[2,2])
exp(coef(m1))*sqrt(diag(vcov(m1)))
exp(coef(m1))
exp(confint.default(m1))

db0$Forest <- relevel(db0$Forest, ref = "MW")
db0$Species <- relevel(db0$Species, ref = "Baboon")
db0$Species <- relevel(db0$Species, ref = "Colobus")

m1 <- glm(Richness2~Forest*Species,family=poisson, data=db0)
exp(coef(m1))
exp(confint.default(m1))

db0$Forest <- relevel(db0$Forest, ref = "MA")



# multiple comparison (pairwise)
levels(db0$Forest_Species) <- c("MA|RC", "MW|RC","MA|YB","MW|YB")

m2 <- glm(Richness2~Forest_Species ,family=poisson, data=db0)
summary(m2)


library(multcomp)
m3 <- glht(m2, linfct = mcp(Forest_Species = "Tukey"))
m3
#tiff("PairWise_v2.tiff", width = 6, height = 4, units = 'in', res = 300,compression = 'lzw')
pdf("FigureS2.pdf", width = 6, height = 4)
par(mar=c(3.5,9,2,2))
plot(m3,main="",xlab="")
mtext(text="95% family-wise confidence level",side=3,line=0.5,font=2,cex=1.2)
mtext(text="Linear Function",side=1,line=2,font=1,cex=1.2)
dev.off()


X <- expand.grid(Forest_Species = levels(db0$Forest_Species))
X
Xmat <- model.matrix(~Forest_Species,data=X)
beta = coef(m2)

mu <- Xmat%*%beta
X$lambda <- exp(mu)
sd <- sqrt(diag(Xmat%*%vcov(m2)%*%t(Xmat)))
X$hi <- exp(mu+1.96*sd)
X$lo <- exp(mu-1.96*sd)
X


#tiff("Histogram_v3.tiff", width = 7, height = 10, units = 'in', res = 300,compression = 'lzw')
pdf("Figure3.pdf", width = 5, height = 6)
par(mfrow=c(2,1))
counts <- table(subset(db0,Species=="Colobus")$Forest,subset(db0,Species=="Colobus")$Richness)
counts <-cbind(as.matrix(counts),matrix(0,ncol=2,nrow=2))
colnames(counts)[5:6] <- c("4","5")
par(mar=c(1,4,0,1))
df.bar <- barplot(as.matrix(counts), ylab="",
                  xlab="", col=c("dodgerblue","orange"),
                  beside=TRUE,ylim=c(0,29))
mtext("Number of samples",side=2,line=2.5,cex=1.,font=2)
legend("right", rownames(counts),fill = c("dodgerblue","orange"), bty = "n")
df.bar
text(1,27,"A",cex=1)
text(15,27,substitute(italic("Procolobus gordonorum")),cex=1)
points(1.5+3*X$lambda[1:2],c(22,23),pch=19,col=c("dodgerblue","orange"))
segments(x0 = 1.5+3*X$lo[1], y0=22, x1 = 1.5+3*X$hi[1], y1 = 22,col="dodgerblue",lwd=2)
#segments(x0 = 3+X$lambda[1], y0=0, x1 = 3+X$lambda[1], y1 = 23,col="dodgerblue",lty="dashed")
segments(x0 = 1.5+3*X$lo[2], y0=23, x1 = 1.5+3*X$hi[2], y1 = 23,col="orange",lwd=2)
#segments(x0 = 3+X$lambda[2], y0=0, x1 = 3+X$lambda[2], y1 = 23,col="orange",lty="dashed")

counts <- table(subset(db0,Species=="Baboon")$Forest,subset(db0,Species=="Baboon")$Richness)
par(mar=c(4,4,0,1))
df.bar <- barplot(as.matrix(counts), ylab="",
                  xlab="", col=c("dodgerblue","orange"),
                  beside=TRUE,ylim=c(0,29))
df.bar
mtext("Host Parasite Richness",side=1,line=2.5,cex=1.,font=2)
mtext("Number of samples",side=2,line=2.5,cex=1.,font=2)
text(1,25,"B",cex=1)
text(15,25,substitute(italic("Papio cynocephalus")),cex=1)
points(1.5+3*X$lambda[3:4],c(22,23),pch=19,col=c("dodgerblue","orange"))
segments(x0 = 1.5+3*X$lo[3], y0=22, x1 = 1.5+3*X$hi[3], y1 = 22,col="dodgerblue",lwd=2)
segments(x0 = 1.5+3*X$lo[4], y0=23, x1 = 1.5+3*X$hi[4], y1 = 23,col="orange",lwd=2)
dev.off()


