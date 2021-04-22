+++++++++++CODE QUI MARCHE+++++++++++++++++++++
library(tidyverse)
library(xts)

# Sélection d'uniquement 1 variable
d <- select(data, IYEAR, X_RFSMOK3)

# Creation d'une variable contenant une valeur unique

e <- d  %>%
  mutate(fumeur = ifelse(X_RFSMOK3 == "true", 1, 0)) %>%
  select(-X_RFSMOK3) %>%
  subset(!is.na(fumeur))

f <- e %>%
  aggregate(by=list(e$IYEAR), sum) %>% 
  select(-IYEAR)

f$Group.1 <- as.Date(ISOdate(agg$Group.1, 1, 1))

t <- xts(order.by=f$Group.1, f$fumeur) 
t <- t[-c(nrow(t)),] 
colnames(t) <-'Fumeur'

plot(t)


+++++++++++++++++++++++++++++++++++++++++++++++++++
  
d <- select(data, IYEAR, SEX, X_RFSMOK3)

d <- d  %>% 
  mutate(homme = ifelse(SEX == "Male", 1, 0)) %>%
  mutate(femme = ifelse(SEX == "Female", 1, 0)) %>%
  mutate(fumeur = ifelse(X_RFSMOK3 == "true", 1, 0)) %>%
  mutate(non_fumeur = ifelse(X_RFSMOK3 == "false", 1, 0)) %>%
  select(-SEX, -X_RFSMOK3)

s <- d  %>% 
  group_by(IYEAR) %>%
  summarise(sum = sum(homme, na.rm=T))

dagg <- aggregate(d, by=list(d$IYEAR), sum)


d$IYEAR <- as.Date(ISOdate(d$IYEAR, 1, 1))



dataset_as_time_series <- ts(data = d[-1],
                             start = 2013,
                             end = 2020,
                             frequency = 1)




xts.ts <- xts(order.by=d$IYEAR, d$X_RFSMOK3)
colnames(xts.ts)<-'Smoker'

plot(xts.ts)

aggregate(xts.ts, index(xts.ts),nfrequency = 1, FUN = count, ndeltat = 1, ts.eps = getOption("ts.eps"))

agg <- aggregate(xts.ts, index(xts.ts), sum)
aggregate(xts.ts,as.Date(index(xts.ts)),count)


agg <- aggregate(ts,                   # Time series object
                 nfrequency = 1,      # Observations per unit of time (submultiple of x)
                 FUN = sum,           # Function to be applied for summary statistics
                 ndeltat = 1,         # Fraction between successive observations
                 ts.eps = getOption("ts.eps")) # Tolerance to determine if 'nfrequency' is a submultiple of the frequency of )

