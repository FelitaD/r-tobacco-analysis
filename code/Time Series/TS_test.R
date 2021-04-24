METHODES XTS

coredata(data_4)
index(data_4)
tclass(data_4)
tzone(data_4)
tformat(data_4) <- "%Y-%m-%d"
periodicity(data_4)
data_5 <- to.yearly(data_4)
data_6 <- to.monthly(data_4)
data_7 <- to.quarterly(data_4)
data_8 <- to.period(data_4,period="week")
nmonths(data_4)
nquarters(data_4)
nyears(data_4)
data_9 <- make.index.unique(data_4, drop=TRUE)
.index(data_4)
.indexwday(data_4)
start(data_4)
end(data_4)
str(data_4)
time(data_4)
head(data_4) # comparer le debut et la fin pour chaque variables
tail(data_4)

data_xts <- as.xts(data_4)
tmp <- tempfile()
write.zoo(data_xts, sep=",", file=tmp)

print(PMS_ts)
colnames(PMS_ts)
str(PMS_ts)
end(PMS_ts)
frequency(PMS_ts)
deltat(PMS_ts)


__________________________________________________________________________
  
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

