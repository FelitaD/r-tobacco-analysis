
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