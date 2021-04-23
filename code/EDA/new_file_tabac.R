# Importation des packages et définition/source de fonctions

library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(prettyR)
library(binom)
library(Epi)
library(foreign)
library(naniar)
library(corrplot)
library(RColorBrewer)
library(Hmisc)
library(xtable)
library(psy)
library(tidyr)

source("http://www.sthda.com/upload/rquery_cormat.r")

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 



# Importation des données


brfss <- read.csv("~/CNAM/Stats/Mini-memoire/data/data_2013_2019.csv")


brfss$MENTHLTH[brfss$MENTHLTH == 88] <- 0
brfss <- brfss %>% replace_with_na(replace = list(MENTHLTH = c(77,99)))

brfss$POORHLTH[brfss$POORHLTH  == 88] <- 0
brfss <- brfss %>% replace_with_na(replace = list(POORHLTH  = c(77,99)))


brfss$QLMENTL2[brfss$QLMENTL2  == 88] <- 0
brfss <- brfss %>% replace_with_na(replace = list(QLMENTL2  = c(77,99)))

brfss$QLSTRES2[brfss$QLSTRES2  == 88] <- 0
brfss <- brfss %>% replace_with_na(replace = list(QLSTRES2  = c(77,99)))

brfss <- brfss %>% replace_with_na(replace = list(ADDEPEV2  = c(7,9)))

brfss <- brfss %>% replace_with_na(replace = list(EMTSUPRT  = c(7,9)))

brfss <- brfss %>% replace_with_na(replace = list(LSATISFY  = c(7,9)))

brfss <- brfss %>% replace_with_na(replace = list(ADDEPEV2  = c(7,9)))
brfss$ADDEPEV2[brfss$ADDEPEV2  == 2] <- 0


brfss <- brfss %>% replace_with_na(replace = list(X_RFSMOK3  = c(9)))
brfss$X_RFSMOK3 <- brfss$X_RFSMOK3 - 1

brfss <- brfss %>% replace_with_na(replace = list(X_SMOKER3  = c(9)))

brfss <- brfss %>% replace_with_na(replace = list(USENOW3  = c(9)))

brfss <- brfss %>% replace_with_na(replace = list(LASTSMK2  = c(77,99)))

brfss <- brfss %>% replace_with_na(replace = list(STOPSMK2  = c(7,9)))

brfss <- brfss %>% replace_with_na(replace = list(SMOKDAY2  = c(7,9)))

brfss <- brfss %>% replace_with_na(replace = list(SMOKE100  = c(7,9)))
brfss$SMOKE100[brfss$SMOKE100 == 2] <- 0

brfss <- brfss %>% replace_with_na(replace = list(SEX = c(7, 9)))
brfss$SEX = cut(brfss$SEX, 2, labels=c("Homme", 'Femme'))


brfss <- brfss %>% replace_with_na(replace = list(PREGNANT = c(7, 9)))

brfss <- brfss %>% replace_with_na(replace = list(MARITAL = c(9)))
brfss$MARITAL = cut(brfss$MARITAL, 6, labels=c("Marié", 'Divorcé',"Veuf","Séparé","Jamais marié","Membre d'un couple non marié"))

brfss$X_CHLDCNT[brfss$X_CHLDCNT == 88] <- 0
brfss <- brfss %>% replace_with_na(replace = list(CHILDREN = c(99)))
brfss$X_CHLDCNT[brfss$X_CHLDCNT  == 1] <- 0
brfss$X_CHLDCNT[brfss$X_CHLDCNT  == 2] <- 1
brfss$X_CHLDCNT[brfss$X_CHLDCNT  == 3] <- 2
brfss$X_CHLDCNT[brfss$X_CHLDCNT  == 4] <- 3
brfss$X_CHLDCNT[brfss$X_CHLDCNT  == 5] <- 4
brfss$X_CHLDCNT[brfss$X_CHLDCNT  == 6] <- 5
brfss <- brfss %>% replace_with_na(replace = list(X_CHLDCNT = c(9)))

brfss <- brfss %>% replace_with_na(replace = list(X_AGE65YR = c(3)))

brfss$X_AGE_G_FACTOR = cut(brfss$X_AGE_G, 6, labels=c("18-24 ans", '25-34 ans','35-44 ans',"45-54 ans","55-64 ans","65ans et plus"))

brfss <- brfss %>% replace_with_na(replace = list(EDUCA = c(9)))
brfss <- brfss %>% replace_with_na(replace = list(X_EDUCAG = c(9)))
brfss$X_EDUCAG_FACTOR = cut(brfss$X_EDUCAG, 4, labels=c("Sans diplôme", 'Diplôme du lycée',"A étudié à l'univ.","Diplôme de l'université"))

brfss <- brfss %>% replace_with_na(replace = list(EMPLOY1 = c(9)))

brfss <- brfss %>% replace_with_na(replace = list(X_INCOMG = c(9)))
brfss$X_INCOMG_FACTOR = cut(brfss$X_INCOMG, 5, labels=c("< 15 000", '15 - 25 000',"25 - 35 000","35 - 50 000","< 50 000"))


brfss <- brfss %>% replace_with_na(replace = list(RENTHOM1 = c(7, 9)))

### Socio-démographie
## Age 
#X_ageg5yr : reported age in 5-year age categories #  14 tranches de 5 ans
#X_age65yr : reported age in 2 age categories # 2 tranches avant/apres 65 ans
#X_age_g : imputed in 6 groups
#respondents sex	sex	SEX
#marital status	marital	MARITAL
#pregnancy status	pregnant	PREGNANT
#number of adults in houselholds	numadult	NUMADULT
#number of children in household	children	CHILDREN
#computed number of children in household	X_chldcnt	X_CHLDCNT
#education level	educa	EDUCA
#computed level of education completed categories	X_educag	X_EDUCAG
#employment stattus	employ1	EMPLOY1
#income level	incom2	INCOME2
#computed income categories	X_incomg	X_INCOMG
#own or rent a home	renthom1	RENTHOM1
#veteran	veteran3	VETERAN3


### Smoking 
#smoke100 : smoked at least 100 cigarettes
#smokday2 : frequency of days now smoking
#stopsmk2 : stopped smoking in last 12 months
#lastsmk2 : interval since last smoked
#usenow3 : use of smokeless tobacco products
#X_smoker3 : computed smoking status
#X_rfsmok3 : current smoking calculated variable


### Santé mentale
#poor physical health or mental health	poorhlth	POORHLTH
#mental health not good	menthlth	MENTHLTH
#how many days depressed past 30 days	qlmentl2	QLMENTL2
#how many days anxious past 30 days	qlstres2	QLSTRES2
#ever told a depressive disorder	addepev2	ADDEPEV2
#how often get emotional support needed	emtsuprt	EMTSUPRT
#satisfaction witth life	lsatisfy	LSATISFY

head(brfss)

str(brfss)

summary(brfss)

describe(brfss,num.desc=c("mean","sd","median","min","max","valid.n"))


########## Données sur le tabac

##################### Moins ou plus de 100 cigarettes dans leur vie ?

brfss$SMOKE100_FACTOR = cut(brfss$SMOKE100, 2, labels=c('Moins de 100 cigarettes', '100 cigarettes et plus'))
table(brfss$SMOKE100_FACTOR)

# Diagramme en barre
ggplot(subset(brfss,!is.na(SMOKE100_FACTOR)), aes(x = as.factor(SMOKE100_FACTOR),fill=SMOKE100_FACTOR)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + theme_economist()+ labs(title = "Répartition des répondants par rapport à leur consommation de tabac", x = "Nombre de cigarettes consommées dans leur vie", y = "Proportion de répondants") + scale_fill_discrete(name="Fréquence") + scale_fill_brewer(palette = "Reds") + theme(axis.text.x=element_text(size=10,vjust=.8, hjust=0.8),legend.position="none")


##################### Fréquence du tabagisme parmi les répondants 


#Création de la colonne "SMOKDAY2_FACTOR" basée sur la colonne "SMOKDAY2"
brfss$SMOKDAY2_FACTOR = cut(brfss$SMOKDAY2, 3, labels=c('Tous les jours', 'Quelques jours',"Jamais"))
table(brfss$SMOKDAY2_FACTOR)

# Diagramme en barre
ggplot(subset(brfss,!is.na(SMOKDAY2_FACTOR)), aes(x = as.factor(SMOKDAY2_FACTOR),fill=SMOKDAY2_FACTOR)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + theme_economist()+ labs(title = "Fréquence du tabagisme chez les répondants", x = "Fréquence", y = "Proportion de répondants") + scale_fill_discrete(name="Fréquence") + scale_fill_brewer(palette = "RdBu") + theme(axis.text.x=element_text(size=10,vjust=.8, hjust=0.8),legend.position="none")


##################### Arrêt du tabagisme dans les 12 derniers mois

#Création de la colonne "STOPSMK2_FACTOR" basée sur la colonne "STOPSMK2"
brfss$STOPSMK2_FACTOR = cut(brfss$SMOKE100, 2, labels=c("Non", 'Oui'))
table(brfss$STOPSMK2_FACTOR)

# Diagramme en barre
ggplot(subset(brfss,!is.na(STOPSMK2_FACTOR)), aes(x = as.factor(STOPSMK2_FACTOR),fill=STOPSMK2_FACTOR)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + theme_economist()+ labs(title = "Arrêt du tabagisme dans les douze derniers mois", x = "Arrêt du tabagisme", y = "Proportion de répondants") + scale_fill_discrete(name="Fréquence") + scale_fill_brewer(palette = "Greens") + theme(axis.text.x=element_text(size=10,vjust=.8, hjust=0.8),legend.position="none")


##################### Arrêt du tabagisme dans les 12 derniers mois

#Création de la colonne "LASTSMK2_FACTOR" basée sur la colonne "LASTSMK2"
brfss$LASTSMK2_FACTOR = as.factor(brfss$LASTSMK2)


levels(brfss$LASTSMK2_FACTOR)[levels(brfss$LASTSMK2_FACTOR)=="1"] <- "< 1 mois"
levels(brfss$LASTSMK2_FACTOR)[levels(brfss$LASTSMK2_FACTOR)=="2"] <- "1-3 mois"
levels(brfss$LASTSMK2_FACTOR)[levels(brfss$LASTSMK2_FACTOR)=="3"] <- "3-6 mois"
levels(brfss$LASTSMK2_FACTOR)[levels(brfss$LASTSMK2_FACTOR)=="4"] <- "6-12 mois"
levels(brfss$LASTSMK2_FACTOR)[levels(brfss$LASTSMK2_FACTOR)=="5"] <- "1-5 ans"
levels(brfss$LASTSMK2_FACTOR)[levels(brfss$LASTSMK2_FACTOR)=="6"] <- "5-10 ans"
levels(brfss$LASTSMK2_FACTOR)[levels(brfss$LASTSMK2_FACTOR)=="7"] <- "10 ans ou plus"
levels(brfss$LASTSMK2_FACTOR)[levels(brfss$LASTSMK2_FACTOR)=="8"] <- "Jamais fumé régulièrement"


table(brfss$LASTSMK2_FACTOR)

# Diagramme en barre
ggplot(subset(brfss,!is.na(LASTSMK2_FACTOR)), aes(x = as.factor(LASTSMK2_FACTOR),fill=LASTSMK2_FACTOR)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + theme_economist()+ labs(title = "Intervalle depuis la dernière fumée", x = "Intervalle", y = "Proportion de répondants") + scale_fill_discrete(name="Fréquence") + scale_fill_brewer(palette = "Pastel1") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8),legend.position="none")


##################### Arrêt du tabagisme dans les 12 derniers mois

#Création de la colonne "X_SMOKER3_FACTOR" basée sur la colonne "X_SMOKER3"
brfss$X_SMOKER3_FACTOR = as.factor(brfss$X_SMOKER3)

levels(brfss$X_SMOKER3_FACTOR)[levels(brfss$X_SMOKER3_FACTOR)=="1"] <- "Fumeur journalier"
levels(brfss$X_SMOKER3_FACTOR)[levels(brfss$X_SMOKER3_FACTOR)=="2"] <- "Fumeur quelques jours"
levels(brfss$X_SMOKER3_FACTOR)[levels(brfss$X_SMOKER3_FACTOR)=="3"] <- "Ancien fumeur"
levels(brfss$X_SMOKER3_FACTOR)[levels(brfss$X_SMOKER3_FACTOR)=="4"] <- "Non fumeur"


table(brfss$X_SMOKER3_FACTOR)

# Diagramme en barre
ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR)), aes(x = as.factor(X_SMOKER3_FACTOR),fill=X_SMOKER3_FACTOR)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + theme_economist()+ labs(title = "Statut des répondants par rapport au tabagisme", x = "Statut", y = "Proportion de répondants") + scale_fill_discrete(name="Fréquence") + scale_fill_brewer(palette = "RdBu") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8),legend.position="none")

##################### Fumeurs actuels

#Création de la colonne "X_RFSMOK3_FACTOR" basée sur la colonne "X_RFSMOK3"
brfss$X_RFSMOK3_FACTOR = cut(brfss$X_RFSMOK3, 2, labels=c("Non", 'Oui'))
table(brfss$X_RFSMOK3_FACTOR)

# Diagramme en barre
ggplot(subset(brfss,!is.na(X_RFSMOK3_FACTOR)), aes(x = as.factor(X_RFSMOK3_FACTOR),fill=X_RFSMOK3_FACTOR)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + theme_economist()+ labs(title = "Répartition des fumeurs parmi les répondants", x = "Fumeur", y = "Proportion de répondants") + scale_fill_discrete(name="Fréquence") + scale_fill_brewer(palette = "Reds") + theme(axis.text.x=element_text(size=10,vjust=.8, hjust=0.8),legend.position="none")







########## Données sur la santé mentale

##################### Mauvaise santé physique ou santé mentale durant les 30 derniers jours



#poor physical health or mental health	poorhlth	POORHLTH
ggplot(subset(brfss, !is.na(POORHLTH)), aes(x=POORHLTH,fill=POORHLTH)) + 
  geom_boxplot(fill="plum2",notch=TRUE,outlier.colour="black", outlier.shape=8,
               outlier.size=1) + theme_economist() +
  ggtitle("Nombre de jours impactés par la santé mentale / physique") + labs(x = "Nombre de jours") + xlim(0,30)  
  
  
#mental health not good	menthlth	MENTHLTH
ggplot(subset(brfss, !is.na(MENTHLTH)), aes(x=MENTHLTH,fill=MENTHLTH)) + 
  geom_boxplot(fill="plum2",notch=TRUE,outlier.colour="black", outlier.shape=8,
               outlier.size=1) + theme_economist() +
  ggtitle("Nombre de jours impactés par la santé mentale / état émo.") + labs(x = "Nombre de jours") + xlim(0,30)

#how many days depressed past 30 days	qlmentl2	QLMENTL2
ggplot(subset(brfss, !is.na(QLMENTL2)), aes(x=QLMENTL2,fill=QLMENTL2)) + 
  geom_boxplot(fill="mistyrose",notch=TRUE,outlier.colour="black", outlier.shape=8,
               outlier.size=1) + theme_economist() +
  ggtitle("Nombre de jours dépressifs au cours du dernier mois") + labs(x = "Nombre de jours")

#how many days anxious past 30 days	QLSTRES2
ggplot(subset(brfss, !is.na(QLSTRES2)), aes(x=QLSTRES2,fill=QLSTRES2)) + 
  geom_boxplot(fill="linen",notch=TRUE,outlier.colour="black", outlier.shape=8,
               outlier.size=1) + theme_economist() +
  ggtitle("Nombre de jours anxieux au cours du dernier mois") + labs(x = "Nombre de jours") + xlim(0,30)


#ever told a depressive disorder	addepev2	ADDEPEV2
brfss$ADDEPEV2_FACTOR = cut(brfss$ADDEPEV2, 2, labels=c("Non", 'Oui'))
table(brfss$ADDEPEV2_FACTOR)

ggplot(subset(brfss,!is.na(ADDEPEV2_FACTOR)), aes(x = as.factor(ADDEPEV2_FACTOR),fill=ADDEPEV2_FACTOR)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + theme_economist()+ labs(title = "Répartition de la dépression parmi les répondants", x = "Dépression", y = "Proportion de répondants") + scale_fill_discrete(name="Fréquence") + scale_fill_brewer(palette = "Reds") + theme(axis.text.x=element_text(size=10,vjust=.8, hjust=0.8),legend.position="none")


#how often get emotional support needed	emtsuprt	EMTSUPRT
#Création de la colonne "EMTSUPRT_FACTOR" basée sur la colonne "EMTSUPRT"
brfss$EMTSUPRT_FACTOR = as.factor(brfss$EMTSUPRT)

levels(brfss$EMTSUPRT_FACTOR)[levels(brfss$EMTSUPRT_FACTOR)=="1"] <- "Toujours"
levels(brfss$EMTSUPRT_FACTOR)[levels(brfss$EMTSUPRT_FACTOR)=="2"] <- "Souvent"
levels(brfss$EMTSUPRT_FACTOR)[levels(brfss$EMTSUPRT_FACTOR)=="3"] <- "Parfois"
levels(brfss$EMTSUPRT_FACTOR)[levels(brfss$EMTSUPRT_FACTOR)=="4"] <- "Rarement"
levels(brfss$EMTSUPRT_FACTOR)[levels(brfss$EMTSUPRT_FACTOR)=="5"] <- "Jamais"

ggplot(subset(brfss,!is.na(EMTSUPRT_FACTOR)), aes(x = as.factor(EMTSUPRT_FACTOR),fill=EMTSUPRT_FACTOR)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + theme_economist()+ labs(title = "Fréquence de Support émotionnel reçu", x = "Fréquence", y = "Proportion de répondants") + scale_fill_discrete(name="Fréquence") + scale_fill_brewer(palette = "PuBu") + theme(axis.text.x=element_text(size=10,vjust=.8, hjust=0.8),legend.position="none")



#satisfaction with life	lsatisfy	LSATISFY
brfss$LSATISFY_FACTOR = as.factor(brfss$LSATISFY)

levels(brfss$LSATISFY_FACTOR)[levels(brfss$LSATISFY_FACTOR)=="1"] <- "Très satisfait"
levels(brfss$LSATISFY_FACTOR)[levels(brfss$LSATISFY_FACTOR)=="2"] <- "Satisfait"
levels(brfss$LSATISFY_FACTOR)[levels(brfss$LSATISFY_FACTOR)=="3"] <- "Insatisfait"
levels(brfss$LSATISFY_FACTOR)[levels(brfss$LSATISFY_FACTOR)=="4"] <- "Très insatisfait"

ggplot(subset(brfss,!is.na(LSATISFY_FACTOR)), aes(x = as.factor(LSATISFY_FACTOR),fill=LSATISFY_FACTOR)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) + theme_economist()+ labs(title = "Niveau global de satisfaction des répondants", x = "Satisfaction", y = "Proportion de répondants") + scale_fill_discrete(name="Fréquence") + scale_fill_brewer(palette = "PuBu") + theme(axis.text.x=element_text(size=10,vjust=.8, hjust=0.8),legend.position="none")


############################## Santé mentale/phy, tabagisme et données socio-démographiques

######## Santé mentale/phy, tabagisme et sexe


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(POORHLTH) & !is.na(SEX)), aes(x=X_SMOKER3_FACTOR, y=POORHLTH, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~SEX) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Nombre de jours impactés par la santé mentale/physique, tabagisme et sexe", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))


######## Santé mentale/phy, tabagisme et age

ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(POORHLTH) & !is.na(X_AGE_G_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=POORHLTH, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_AGE_G_FACTOR) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Nombre de jours impactés par la santé mentale/physique, tabagisme et santé mentale/physique et âge", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Santé mentale/phy, tabagisme et statut matrimonial

ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(POORHLTH) & !is.na(MARITAL)), aes(x=X_SMOKER3_FACTOR, y=POORHLTH, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~MARITAL) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Nombre de jours impactés par la santé mentale/physique, tabagisme et statut matrimonial", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Santé mentale/phy, tabagisme et niveau d'études


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(POORHLTH) & !is.na(X_EDUCAG_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=POORHLTH, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_EDUCAG_FACTOR) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Nombre de jours impactés par la santé mentale/physique, tabagisme et niveau d'études", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))


######## Santé mentale/phy, tabagisme et niveau de revenus


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(POORHLTH) & !is.na(X_INCOMG_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=POORHLTH, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_INCOMG_FACTOR,ncol=5) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Nombre de jours impactés par la santé mentale/physique, tabagisme et revenus", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))




################################## Santé mentale, tabagisme & données socio-démographiques


######## Santé mentale, tabagisme et sexe


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(MENTHLTH) & !is.na(SEX)), aes(x=X_SMOKER3_FACTOR, y=MENTHLTH, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~SEX) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme, santé mentale et sexe", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))


######## Santé mentale, tabagisme et age

ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(MENTHLTH) & !is.na(X_AGE_G_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=MENTHLTH, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_AGE_G_FACTOR) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme et santé mentale et âge", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Santé mentale, tabagisme et statut matrimonial

ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(MENTHLTH) & !is.na(MARITAL)), aes(x=X_SMOKER3_FACTOR, y=MENTHLTH, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~MARITAL) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme et santé mentale et statut matrimonial", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Santé mentale, tabagisme et niveau d'études


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(MENTHLTH) & !is.na(X_EDUCAG_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=MENTHLTH, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_EDUCAG_FACTOR) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme et santé mentale et niveau d'études", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))


######## Santé mentale, tabagisme et niveau de revenus


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(MENTHLTH) & !is.na(X_INCOMG_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=MENTHLTH, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_INCOMG_FACTOR,ncol=5) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme et santé mentale et revenus", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))






################################## Humeur dépressive au cours des 30 derniers jours, tabagisme & données socio-démographiques


######## Humeur dépressive, tabagisme et sexe


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(QLMENTL2) & !is.na(SEX)), aes(x=X_SMOKER3_FACTOR, y=QLMENTL2, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~SEX) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme, humeur dépressive et sexe", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))


######## Humeur dépressive, tabagisme et age

ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(QLMENTL2) & !is.na(X_AGE_G_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=QLMENTL2, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_AGE_G_FACTOR) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme, humeur dépressive et âge", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Humeur dépressive, tabagisme et statut matrimonial

ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(QLMENTL2) & !is.na(MARITAL)), aes(x=X_SMOKER3_FACTOR, y=QLMENTL2, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~MARITAL) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme, humeur dépressive et statut matrimonial", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Humeur dépressive, tabagisme et niveau d'études


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(QLMENTL2) & !is.na(X_EDUCAG_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=QLMENTL2, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_EDUCAG_FACTOR) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme, humeur dépressive et niveau d'études", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))


######## Humeur dépressive, tabagisme et niveau de revenus


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(QLMENTL2) & !is.na(X_INCOMG_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=QLMENTL2, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_INCOMG_FACTOR,ncol=5) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme, humeur dépressive et revenus", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))





################################## Humeur anxieuse au cours des 30 derniers jours, tabagisme & données socio-démographiques


######## Humeur anxieuse, tabagisme et sexe


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(QLSTRES2) & !is.na(SEX)), aes(x=X_SMOKER3_FACTOR, y=QLSTRES2, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~SEX) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme, humeur anxieuse et sexe", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))


######## Humeur anxieuse, tabagisme et age

ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(QLSTRES2) & !is.na(X_AGE_G_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=QLSTRES2, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_AGE_G_FACTOR) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme, humeur anxieuse et âge", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Humeur anxieuse, tabagisme et statut matrimonial

ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(QLSTRES2) & !is.na(MARITAL)), aes(x=X_SMOKER3_FACTOR, y=QLSTRES2, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~MARITAL) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme, humeur anxieuse et statut matrimonial", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Humeur anxieuse, tabagisme et niveau d'études


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(QLSTRES2) & !is.na(X_EDUCAG_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=QLSTRES2, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_EDUCAG_FACTOR) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme, humeur anxieuse et niveau d'études", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))


######## Humeur anxieuse, tabagisme et niveau de revenus


ggplot(subset(brfss,!is.na(X_SMOKER3_FACTOR) & !is.na(QLSTRES2) & !is.na(X_INCOMG_FACTOR)), aes(x=X_SMOKER3_FACTOR, y=QLSTRES2, fill=X_SMOKER3_FACTOR)) + geom_boxplot() + 
  facet_wrap(~X_INCOMG_FACTOR,ncol=5) + scale_fill_brewer(palette = "Pastel2") + labs(title = "Tabagisme, humeur anxieuse et revenus", x = "Statut", y = "Nombre de jours impactés par la santé mentale / physique") + scale_fill_discrete(name="Statut") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))





################################## Dépression, tabagisme & données socio-démographiques


######## Dépression, tabagisme et sexe


ggplot(subset(brfss, !is.na(X_SMOKER3_FACTOR) & !is.na(ADDEPEV2_FACTOR) & !is.na(SEX)), aes(x = X_SMOKER3_FACTOR, fill = ADDEPEV2_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~SEX) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Dépression selon le tabagisme et le sexe", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Dépression, tabagisme et age

ggplot(subset(brfss, !is.na(X_SMOKER3_FACTOR) & !is.na(ADDEPEV2_FACTOR) & !is.na(X_AGE_G_FACTOR)), aes(x = X_SMOKER3_FACTOR, fill = ADDEPEV2_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~X_AGE_G_FACTOR) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Dépression selon le tabagisme et l'âge", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Dépression, tabagisme et statut matrimonial

ggplot(subset(brfss, !is.na(X_SMOKER3_FACTOR) & !is.na(ADDEPEV2_FACTOR) & !is.na(MARITAL)), aes(x = X_SMOKER3_FACTOR, fill = ADDEPEV2_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~MARITAL) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Dépression selon le tabagisme et le statut matrimonial", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Dépression, tabagisme et niveau d'études


ggplot(subset(brfss, !is.na(X_SMOKER3_FACTOR) & !is.na(ADDEPEV2_FACTOR) & !is.na(X_EDUCAG_FACTOR)), aes(x = X_SMOKER3_FACTOR, fill = ADDEPEV2_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~X_EDUCAG_FACTOR) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Dépression selon le tabagisme et le niveau d'études", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))


######## Dépression, tabagisme et niveau de revenus


ggplot(subset(brfss, !is.na(X_SMOKER3_FACTOR) & !is.na(ADDEPEV2_FACTOR) & !is.na(X_INCOMG_FACTOR)), aes(x = X_SMOKER3_FACTOR, fill = ADDEPEV2_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~X_INCOMG_FACTOR) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Dépression selon le tabagisme et le niveau de revenus", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))






################################## Support émotionnel, tabagisme & données socio-démographiques


######## Support émotionnel, tabagisme et sexe


ggplot(subset(brfss, !is.na(EMTSUPRT_FACTOR) & !is.na(X_SMOKER3_FACTOR) & !is.na(SEX)), aes(x = X_SMOKER3_FACTOR, fill = EMTSUPRT_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~SEX) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Support émotionnel selon le tabagisme et le sexe", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Support émotionnel, tabagisme et age

ggplot(subset(brfss, !is.na(EMTSUPRT_FACTOR) & !is.na(X_SMOKER3_FACTOR) & !is.na(X_AGE_G_FACTOR)), aes(x = X_SMOKER3_FACTOR, fill = EMTSUPRT_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~X_AGE_G_FACTOR) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Support émotionnel selon le tabagisme et l'âge", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Support émotionnel, tabagisme et statut matrimonial

ggplot(subset(brfss, !is.na(EMTSUPRT_FACTOR) & !is.na(X_SMOKER3_FACTOR) & !is.na(MARITAL)), aes(x = X_SMOKER3_FACTOR, fill = EMTSUPRT_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~MARITAL) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Support émotionnel selon le tabagisme et le statut matrimonial", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Support émotionnel, tabagisme et niveau d'études


ggplot(subset(brfss, !is.na(EMTSUPRT_FACTOR) & !is.na(X_SMOKER3_FACTOR) & !is.na(X_EDUCAG_FACTOR)), aes(x = X_SMOKER3_FACTOR, fill = EMTSUPRT_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~X_EDUCAG_FACTOR) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Support émotionnel selon le tabagisme et le niveau d'études", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))


######## Support émotionnel, tabagisme et niveau de revenus


ggplot(subset(brfss, !is.na(EMTSUPRT_FACTOR) & !is.na(X_SMOKER3_FACTOR) & !is.na(X_INCOMG_FACTOR)), aes(x = X_SMOKER3_FACTOR, fill = EMTSUPRT_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~X_INCOMG_FACTOR) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Support émotionnel selon le tabagisme et le niveau de revenus", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))





################################## Niveau de satisfaction, tabagisme & données socio-démographiques


######## Niveau de satisfaction, tabagisme et sexe


ggplot(subset(brfss, !is.na(LSATISFY_FACTOR) & !is.na(X_SMOKER3_FACTOR) & !is.na(SEX)), aes(x = X_SMOKER3_FACTOR, fill = LSATISFY_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~SEX) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Niveau de satisfaction selon le tabagisme et le sexe", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Niveau de satisfaction, tabagisme et age

ggplot(subset(brfss, !is.na(LSATISFY_FACTOR) & !is.na(X_SMOKER3_FACTOR) & !is.na(X_AGE_G_FACTOR)), aes(x = X_SMOKER3_FACTOR, fill = LSATISFY_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~X_AGE_G_FACTOR) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Niveau de satisfaction selon le tabagisme et l'âge", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Niveau de satisfaction, tabagisme et statut matrimonial

ggplot(subset(brfss, !is.na(LSATISFY_FACTOR) & !is.na(X_SMOKER3_FACTOR) & !is.na(MARITAL)), aes(x = X_SMOKER3_FACTOR, fill = LSATISFY_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~MARITAL) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Niveau de satisfaction selon le tabagisme et le statut matrimonial", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))

######## Niveau de satisfaction, tabagisme et niveau d'études


ggplot(subset(brfss, !is.na(LSATISFY_FACTOR) & !is.na(X_SMOKER3_FACTOR) & !is.na(X_EDUCAG_FACTOR)), aes(x = X_SMOKER3_FACTOR, fill = LSATISFY_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~X_EDUCAG_FACTOR) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Niveau de satisfaction selon le tabagisme et le niveau d'études", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))


######## Niveau de satisfaction, tabagisme et niveau de revenus


ggplot(subset(brfss, !is.na(LSATISFY_FACTOR) & !is.na(X_SMOKER3_FACTOR) & !is.na(X_INCOMG_FACTOR)), aes(x = X_SMOKER3_FACTOR, fill = LSATISFY_FACTOR)) +
  geom_bar(colour = "black",position = "fill") + facet_wrap(~X_INCOMG_FACTOR) +
  scale_fill_brewer(palette = "Pastel1") + theme_economist()+ labs(title = "Niveau de satisfaction selon le tabagisme et le niveau de revenus", x = "Tabagisme", y = "Pourcentage de répondants") + scale_fill_discrete(name="Dépression") + theme(axis.text.x=element_text(angle=30,size=10,vjust=.8, hjust=0.8))








################ MATRICE DE CORRELATION 

X <- brfss %>%
  select(NUMADULT,
         MENTHLTH,
         POORHLTH,
         CHILDREN,
         X_AGEG5YR,
         EDUCA,
         INCOME2,
         X_SMOKER3,
         EMTSUPRT,
         LSATISFY,
         QLSTRES2,
         QLMENTL2
  )


rquery.cormat(X)

corstars(X, result="text")


#ACP pour les variables
mdspca(X)
sphpca(X)
sphpca(X,v=130)

expliquer <- "X_SMOKER3"
explicatives <- c("NUMADULT",
                  "MENTHLTH",
                  "POORHLTH",
                  "CHILDREN",
                  "X_AGEG5YR",
                  "EDUCA",
                  "INCOME2",
                  "EMTSUPRT",
                  "LSATISFY",
                  "QLSTRES2",
                  "QLMENTL2")
fpca(data=X,y=expliquer,x=explicatives,partial="No")


#faire des cor.test





#####################################
# Tests statistiques
#####################################


# Sexe et tabagisme
brfss$sex <- brfss$SEX
levels(brfss$sex)[levels(brfss$sex)=="Homme"] <- 0
levels(brfss$sex)[levels(brfss$sex)=="Femme"] <- 1


  
tab <- table(brfss$X_RFSMOK3,brfss$sex,deparse.level = 2)
prop.table(tab,1)
prop.table(tab,2)


chisq.test(brfss$SEX,brfss$X_RFSMOK3,correct=FALSE)
#### on a donc une différence significative de prévalence de fumeur qui ne pourrait pas être expliquée par le hasard à lui tout seul (p très inférieur à 5%)

### faire aussi chi2 pr ADDEPEV2, EMTSUPRT, LSATISFY et tabagisme


#####faire des tests de wilcoxon entre les variables POORHLTH MENTHLTH QLMENTL2 QLSTRES2 et tabagisme