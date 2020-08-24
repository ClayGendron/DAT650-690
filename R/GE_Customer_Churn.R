library(tidymodels)
library(tidyverse)
library(here)
library(corrplot)
library(randomForest)
library(rattle)
library(magrittr)

# data pull
churn_dataset <- read.csv(here::here('Data/ge_cell_data.csv'))

# cal and val tables

churn_calibration <- churn_dataset %>% filter(CALIBRAT == 1)
churn_validation <- churn_dataset %>% filter(CALIBRAT == 0)

# descriptive statistics 
churn_lm_filter <- lm(CHURNDEP ~ REVENUE + MOU + RECCHRGE + DIRECTAS + OVERAGE + ROAM + 
                        CHANGEM + CHANGER + DROPVCE + BLCKVCE + UNANSVCE + CUSTCARE + 
                        THREEWAY + MOUREC + OUTCALLS + INCALLS + PEAKVCE + OPEAKVCE + 
                        DROPBLK + CALLFWDV + CALLWAIT + MONTHS + UNIQSUBS + 
                        ACTVSUBS + PHONES + MODELS + EQPDAYS + CUSTOMER + AGE1 + 
                        AGE2 + CHILDREN + CREDITA + CREDITAA + CREDITB + CREDITC + 
                        CREDITDE + CREDITGY + CREDITZ + CREDIT_RATING + PRIZMRUR + PRIZMUB + 
                        PRIZMTWN + PRZM_NUM + REFURB + WEBCAP + TRUCK + RV + 
                        OCCPROF + OCCCLER + OCCCRFT + OCCSTUD + OCCHMKR + OCCRET + 
                        OCCSELF + OCC + OWNRENT + MARRYUN + MARRYYES + MARRYNO + 
                        MARRY + MAILORD + MAILRES + MAILFLAG + TRAVEL + PCOWN + 
                        CREDITCD + RETCALLS + RETACCPT + NEWCELLY + NEWCELLN + REFER + 
                        INCMISS + INCOME + MCYCLE + CREDITAD + SETPRCM + SETPRC + 
                        RETCALL, data = churn_calibration)
summary(churn_lm_filter)

# selected signifigant variables
# MOU + REFURB + OCCCLER + WEBCAP + MODELS + ROAM + OVERAGE + PRIZMUB + PRZM_NUM + CHANGER

MOU_hist <- hist(churn_calibration$MOU,
                  main = 'MOU Histogram',
                  xlab = 'MOU',
                  ylab = 'Frequency',
                  col = 'dodgerblue',
                  freq = TRUE)

REFURB_hist <- hist(churn_calibration$REFURB,
                 main = 'REFURB Histogram',
                 xlab = 'REFURB',
                 ylab = 'Frequency',
                 col = 'dodgerblue',
                 freq = TRUE)

OCCLER_hist <- hist(churn_calibration$OCCLER,
                    main = 'OCCLER Histogram',
                    xlab = 'OCCLER',
                    ylab = 'Frequency',
                    col = 'dodgerblue',
                    freq = TRUE)

WEBCAP_hist <- hist(churn_calibration$WEBCAP,
                    main = 'WEBCAP Histogram',
                    xlab = 'WEBCAP',
                    ylab = 'Frequency',
                    col = 'dodgerblue',
                    freq = TRUE)

MODELS_hist <- hist(churn_calibration$MODELS,
                    main = 'MODELS Histogram',
                    xlab = 'MODELS',
                    ylab = 'Frequency',
                    col = 'dodgerblue',
                    freq = TRUE)

ROAM_hist <- hist(churn_calibration$ROAM,
                  main = 'ROAM Histogram',
                  xlab = 'ROAM',
                  ylab = 'Frequency',
                  col = 'dodgerblue',
                  freq = TRUE)

OVERAGE_hist <- hist(churn_calibration$OVERAGE,
                     main = 'OVERAGE Histogram',
                     xlab = 'OVERAGE',
                     ylab = 'Frequency',
                     col = 'dodgerblue',
                     freq = TRUE)

PRIZMUB_hist <- hist(churn_calibration$PRIZMUB,
                     main = 'PRIZMUB Histogram',
                     xlab = 'PRIZMUB',
                     ylab = 'Frequency',
                     col = 'dodgerblue',
                     freq = TRUE)

PRZM_NUM_hist <- hist(churn_calibration$PRZM_NUM,
                      main = 'PRZM_NUM Histogram',
                      xlab = 'PRZM_NUM',
                      ylab = 'Frequency',
                      col = 'dodgerblue',
                      freq = TRUE)

CHANGER_hist <- hist(churn_calibration$CHANGER,
                     main = 'CHANGER Histogram',
                     xlab = 'CHANGER',
                     ylab = 'Frequency',
                     col = 'dodgerblue',
                     freq = TRUE)

RECCHRGE_hist <- hist(churn_calibration$RECCHRGE,
                 main = 'RECCHRGE Histogram',
                 xlab = 'RECCHRGE',
                 ylab = 'Frequency',
                 col = 'dodgerblue',
                 freq = TRUE)

CHANGEM_hist <- hist(churn_calibration$CHANGEM,
                      main = 'CHANGEM Histogram',
                      xlab = 'CHANGEM',
                      ylab = 'Frequency',
                      col = 'dodgerblue',
                      freq = TRUE)


# building predictive model

building <- TRUE
scoring  <- ! building

# random forest model 


churn_rf <- randomForest::randomForest(as.factor(CHURNDEP) ~ . -CHURN -CSA,
                                     data=churn_calibration, 
                                     ntree=1000,
                                     mtry=10,
                                     importance=TRUE,
                                     na.action=randomForest::na.roughfix,
                                     replace=FALSE)

# Generate textual output of the 'Random Forest' model.

churn_rf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(churn_rf$y, as.numeric(churn_rf$predicted))


# List the importance of the variables.

rn <- round(randomForest::importance(churn_rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# K-Means Clustering Model

k_table <- churn_calibration %>% dplyr::select(CHANGEM, RECCHRGE, EQPDAYS, OUTCALLS, OPEAKVCE, INCALLS, WEBCAP, MOUREC, MAILRES, MOU, CHURNDEP)

k_mod <- kmeans(k_table,centers = 4, nstart = 100)
k_mod

# build table

pva_clusters <- cbind(pva, k_mod$cluster)
















