```{r}
tabl <-read.table(text="time, value
2012-11-30-10:28:00, 12.9
2012-11-30-10:29:00, 5.5
2012-11-30-10:30:00, 5.5
2012-11-30-10:31:00, 5.5
2012-11-30-10:32:00, 9
2012-11-30-10:35:00, 9
2012-11-30-10:36:00, 14.4
2012-11-30-10:38:00, 12.6", header = TRUE, sep=",", as.is=TRUE)
head(tabl)

times <-as.POSIXct(strptime(tabl[,1], '%Y-%m-%d-%H:%M:%S'))
head(times)

new_tabl <-zoo(tabl[,2],times)
head(new_tabl)

data3 <-merge(new_tabl, zoo(, seq(min(times), max(times), "min")))
head(data3)

data4 <-na.approx(data3)
head(data4)
```
