METHODES XTS

coredata(tabac)
index(tabac)
tclass(tabac)
tzone(tabac)
tformat(tabac) <- "%Y-%m-%d"
periodicity(tabac)
data_5 <- to.yearly(tabac)
data_6 <- to.monthly(tabac)
data_7 <- to.quarterly(tabac)
data_8 <- to.period(tabac,period="week")
nmonths(tabac)
nquarters(tabac)
nyears(tabac)
data_9 <- make.index.unique(tabac, drop=TRUE)
.index(tabac)
.indexwday(tabac)
start(tabac)
end(tabac)
str(tabac)
time(tabac)
head(tabac) # comparer le debut et la fin pour chaque variables
tail(tabac)

data_xts <- as.xts(tabac)
tmp <- tempfile()
write.zoo(data_xts, sep=",", file=tmp)

print(PMS_ts)
colnames(PMS_ts)
str(PMS_ts)
end(PMS_ts)
frequency(PMS_ts)
deltat(PMS_ts)
