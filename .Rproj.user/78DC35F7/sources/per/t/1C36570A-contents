# 0. preparations
# 1. Check BHI measurement invariance and compute scale scores
# 2. Preprocess potential control variables
# 3. Save data
# 4. Explore ToP effects


#----------------------------------------------------------------
# 0. preparations
#----------------------------------------------------------------
rm(list=ls())



dat <- read.csv("Data/Topaq.csv", header = T)[,-1]



library(psych)
library(lavaan)
library(dplyr)

sessionInfo()

dat$version <- factor(dat$survey, levels = c(1,2), labels = c("german", "english"))
names(dat)

#----------------------------------------------------------------
# 1. Check BHI measurement invariance and compute scale scores
#----------------------------------------------------------------

#----------------------------------------------------------------
# HH


# measuremnt invariance

    configural <- 'f =~ BHI_HH_1 + BHI_HH_2_r + BHI_HH_3_r + BHI_HH_4_r
      BHI_HH_1 ~ 1
      BHI_HH_2_r ~ 1
      BHI_HH_3_r ~ 1
      BHI_HH_4_r ~ 1'

    fit.configural <- cfa(configural, dat, group = "version")
    

    metric <- 'f =~ c(l1, l1)*BHI_HH_1 + c(l2, l2)*BHI_HH_2_r + c(l3, l3)*BHI_HH_3_r + c(l4, l4)*BHI_HH_4_r
      BHI_HH_1 ~ 1
      BHI_HH_2_r ~ 1
      BHI_HH_3_r ~ 1
      BHI_HH_4_r ~ 1'
    
    fit.metric <- cfa(metric, dat, group = "version")
    
    
    scalar <- 'f =~ c(l1, l1)*BHI_HH_1 + c(l2, l2)*BHI_HH_2_r + c(l3, l3)*BHI_HH_3_r + c(l4, l4)*BHI_HH_4_r
      BHI_HH_1 ~ c(m1, m1)*1
      BHI_HH_2_r ~ c(m2, m2)*1
      BHI_HH_3_r ~ c(m3, m3)*1
      BHI_HH_4_r ~ c(m4, m4)*1'

    fit.scalar <- cfa(scalar, dat, group = "version")

anova(fit.configural, fit.metric) # metric invariance is given
anova(fit.metric, fit.scalar) # scalar invariance is not given


# reverse code
dat$BHI_HH_2 <- 6 - dat$BHI_HH_2_r
dat$BHI_HH_3 <- 6 - dat$BHI_HH_3_r
dat$BHI_HH_4 <- 6 - dat$BHI_HH_4_r

# scale scores
dat$HH <- rowMeans(dat[,c("BHI_HH_1", "BHI_HH_2", "BHI_HH_3", "BHI_HH_4")])

# alpha 
psych::alpha(dat[,c("BHI_HH_1", "BHI_HH_2", "BHI_HH_3", "BHI_HH_4")])$total[1]



#----------------------------------------------------------------
# E


# measurement invariance

    configural <- 'f =~ BHI_E_1 + BHI_E_2_r + BHI_E_3_r + BHI_E_4
        BHI_E_1 ~ 1
        BHI_E_2_r ~ 1
        BHI_E_3_r ~ 1
        BHI_E_4 ~ 1'

    fit.configural <- cfa(configural, dat, group = "version")
    

    metric <- 'f =~ c(l1, l1)*BHI_E_1 + c(l2, l2)*BHI_E_2_r + c(l3, l3)*BHI_E_3_r + c(l4, l4)*BHI_E_4
        BHI_E_1 ~ 1
        BHI_E_2_r ~ 1
        BHI_E_3_r ~ 1
        BHI_E_4 ~ 1'
    
    fit.metric <- cfa(metric, dat, group = "version")
    
    
    scalar <- 'f =~ c(l1, l1)*BHI_E_1 + c(l2, l2)*BHI_E_2_r + c(l3, l3)*BHI_E_3_r + c(l4, l4)*BHI_E_4
        BHI_E_1 ~ c(m1, m1)*1
        BHI_E_2_r ~ c(m2, m21)*1
        BHI_E_3_r ~ c(m3, m3)*1
        BHI_E_4 ~ c(m4, m4)*1'
    
    fit.scalar <- cfa(scalar, dat, group = "version")

anova(fit.configural, fit.metric) # metric invariance is given
anova(fit.metric, fit.scalar) # scalar invariance is not given

# reverse code
dat$BHI_E_2 <- 6 - dat$BHI_E_2_r
dat$BHI_E_3 <- 6 - dat$BHI_E_3_r

# scale scores
dat$E <- rowMeans(dat[,c("BHI_E_1", "BHI_E_2", "BHI_E_3", "BHI_E_4")])

# alpha 
psych::alpha(dat[,c("BHI_E_1", "BHI_E_2", "BHI_E_3", "BHI_E_4")])$total[1] 




#----------------------------------------------------------------
# X

# measurement invariance

    configural <- 'f =~ BHI_X_1_r + BHI_X_2 + BHI_X_3 + BHI_X_4_r
        BHI_X_1_r ~ 1
        BHI_X_2 ~ 1
        BHI_X_3 ~ 1
        BHI_X_4_r ~ 1'

    fit.configural <- cfa(configural, dat, group = "version")


    metric <- 'f =~ c(l1, l1)*BHI_X_1_r + c(l2, l2)*BHI_X_2 + c(l3, l3)*BHI_X_3 + c(l4, l4)*BHI_X_4_r
        BHI_X_1_r ~ 1
        BHI_X_2 ~ 1
        BHI_X_3 ~ 1
        BHI_X_4_r ~ 1'
    
    fit.metric <- cfa(metric, dat, group = "version")   
    
    
    scalar <- 'f =~ c(l1, l1)*BHI_X_1_r + c(l2, l2)*BHI_X_2 + c(l3, l3)*BHI_X_3 + c(l4, l4)*BHI_X_4_r
        BHI_X_1_r ~ c(m1, m1)*1
        BHI_X_2 ~ c(m2, m21)*1
        BHI_X_3 ~ c(m3, m3)*1
        BHI_X_4_r ~ c(m4, m4)*1'

    fit.scalar <- cfa(scalar, dat, group = "version")

    
anova(fit.configural, fit.metric) # metric invariance is given
anova(fit.metric, fit.scalar) # scalar invariance is borderline (see BIC vs AIC)


# reverse code
dat$BHI_X_1 <- 6 - dat$BHI_X_1_r
dat$BHI_X_4 <- 6 - dat$BHI_X_4_r

# scale scores
dat$X <- rowMeans(dat[,c("BHI_X_1", "BHI_X_2", "BHI_X_3", "BHI_X_4")])

# alpha 
psych::alpha(dat[,c("BHI_X_1", "BHI_X_2", "BHI_X_3", "BHI_X_4")])$total[1] 



#----------------------------------------------------------------
# A

# measurementinvariance

    configural <- 'f =~ BHI_A_1_r + BHI_A_2_r + BHI_A_3 + BHI_A_4
        BHI_A_1_r ~ 1
        BHI_A_2_r ~ 1
        BHI_A_3 ~ 1
        BHI_A_4 ~ 1'
    
    fit.configural <- cfa(configural, dat, group = "version")
    
    
    metric <- 'f =~ c(l1, l1)*BHI_A_1_r + c(l2, l2)*BHI_A_2_r + c(l3, l3)*BHI_A_3 + c(l4, l4)*BHI_A_4
        BHI_A_1_r ~ 1
        BHI_A_2_r ~ 1
        BHI_A_3 ~ 1
        BHI_A_4 ~ 1'
    
    fit.metric <- cfa(metric, dat, group = "version")
    
    
    scalar <- 'f =~ c(l1, l1)*BHI_A_1_r + c(l2, l2)*BHI_A_2_r + c(l3, l3)*BHI_A_3 + c(l4, l4)*BHI_A_4
        BHI_A_1_r ~ c(m1, m1)*1
        BHI_A_2_r ~ c(m2, m21)*1
        BHI_A_3 ~ c(m3, m3)*1
        BHI_A_4 ~ c(m4, m4)*1'
    
    fit.scalar <- cfa(scalar, dat, group = "version")
    

anova(fit.configural, fit.metric) # metric invariance is given
anova(fit.metric, fit.scalar) # scalar invariance is not given


# reverse code
dat$BHI_A_1 <- 6 - dat$BHI_A_1_r
dat$BHI_A_2 <- 6 - dat$BHI_A_2_r

# scale scores
dat$A <- rowMeans(dat[,c("BHI_A_1", "BHI_A_2", "BHI_A_3", "BHI_A_4")])

# alpha 
psych::alpha(dat[,c("BHI_A_1", "BHI_A_2", "BHI_A_3", "BHI_A_4")])$total[1] 



#----------------------------------------------------------------
# C

# measurementinvariance

configural <- 'f =~ BHI_C_1 + BHI_C_2_r + BHI_C_3 + BHI_C_4_r
        BHI_C_1 ~ 1
        BHI_C_2_r ~ 1
        BHI_C_3 ~ 1
        BHI_C_4_r ~ 1'

fit.configural <- cfa(configural, dat, group = "version")


metric <- 'f =~ c(l1, l1)*BHI_C_1 + c(l2, l2)*BHI_C_2_r + c(l3, l3)*BHI_C_3 + c(l4, l4)*BHI_C_4_r
        BHI_C_1 ~ 1
        BHI_C_2_r ~ 1
        BHI_C_3 ~ 1
        BHI_C_4_r ~ 1'

fit.metric <- cfa(metric, dat, group = "version")


scalar <- 'f =~ c(l1, l1)*BHI_C_1 + c(l2, l2)*BHI_C_2_r + c(l3, l3)*BHI_C_3 + c(l4, l4)*BHI_C_4_r
        BHI_C_1 ~ c(m1, m1)*1
        BHI_C_2_r ~ c(m2, m21)*1
        BHI_C_3 ~ c(m3, m3)*1
        BHI_C_4_r ~ c(m4, m4)*1'

fit.scalar <- cfa(scalar, dat, group = "version")


anova(fit.configural, fit.metric) # metric invariance is given
anova(fit.metric, fit.scalar) # scalar invariance is not given


# reverse code
dat$BHI_C_2 <- 6 - dat$BHI_C_2_r
dat$BHI_C_4 <- 6 - dat$BHI_C_4_r

# scale scores
dat$C <- rowMeans(dat[,c("BHI_C_1", "BHI_C_2", "BHI_C_3", "BHI_C_4")])

# alpha 
psych::alpha(dat[,c("BHI_C_1", "BHI_C_2", "BHI_C_3", "BHI_C_4")])$total[1] 



#----------------------------------------------------------------
# O

# measurementinvariance

configural <- 'f =~ BHI_O_1 + BHI_O_2_r + BHI_O_3 + BHI_O_4
        BHI_O_1 ~ 1
        BHI_O_2_r ~ 1
        BHI_O_3 ~ 1
        BHI_O_4 ~ 1'

fit.configural <- cfa(configural, dat, group = "version")


metric <- 'f =~ c(l1, l1)*BHI_O_1 + c(l2, l2)*BHI_O_2_r + c(l3, l3)*BHI_O_3 + c(l4, l4)*BHI_O_4
        BHI_O_1 ~ 1
        BHI_O_2_r ~ 1
        BHI_O_3 ~ 1
        BHI_O_4 ~ 1'

fit.metric <- cfa(metric, dat, group = "version")


scalar <- 'f =~ c(l1, l1)*BHI_O_1 + c(l2, l2)*BHI_O_2_r + c(l3, l3)*BHI_O_3 + c(l4, l4)*BHI_O_4
        BHI_O_1 ~ c(m1, m1)*1
        BHI_O_2_r ~ c(m2, m21)*1
        BHI_O_3 ~ c(m3, m3)*1
        BHI_O_4 ~ c(m4, m4)*1'

fit.scalar <- cfa(scalar, dat, group = "version")


anova(fit.configural, fit.metric) # metric invariance is given
anova(fit.metric, fit.scalar) # scalar invariance is not given


# reverse code
dat$BHI_O_2 <- 6 - dat$BHI_O_2_r

# scale scores
dat$O <- rowMeans(dat[,c("BHI_O_1", "BHI_O_2", "BHI_O_3", "BHI_O_4")])

# alpha 
psych::alpha(dat[,c("BHI_O_1", "BHI_O_2", "BHI_O_3", "BHI_O_4")])$total[1] 


#----------------------------------------------------------------
# 2. Preprocess potential control variables
#----------------------------------------------------------------

#----------------------------------------------------------------
# Corona
psych::describe(dat$Corona) 
describe(dat$Corona)

#----------------------------------------------------------------
# Curfew 

table(dat$Curfew_1) 
table(dat$Curfew_1_1) 
table(dat$Curfew_1_2) 

table(dat$Curfew_2) 
table(dat$Curfew_2_1) 
table(dat$Curfew_2_2) 
table(dat$Curfew_2_3) 

table(dat$Curfew_1, dat$Curfew_2_3) 

  # Adopt Cufew_1 and Curfew_2_3 
  dat$Personal_Mobility_Restriction <- ifelse(dat$Curfew_1 == 2, 0, dat$Curfew_1)
  dat$Curfew_2_3 <- ifelse(is.na(dat$Curfew_2_3), 2, dat$Curfew_2_3) # change NAs to "2" 
  dat$Public_Transport_Restriction <- ifelse(dat$Curfew_2_3 == 2, 0, dat$Curfew_2_3) # recode as restriction yes = 1, no = 0 and rename
  
  
#----------------------------------------------------------------
# Risk & Quarantine 
  
table(dat$Risk_Quarantine) 
table(dat$Risk_Duration) 
  dat$Quarantine_Duration <- ifelse(is.na(dat$Risk_Duration), 0, dat$Risk_Duration) # change NAs to "0" and rename
  table(dat$Quarantine_Duration)
table(dat$Risk_Freq) 

#----------------------------------------------------------------
# Place of residence and Nationality

table(dat$Residence) 
tab <- table(dat$Residence) 
tab <- as.data.frame(tab)

### Look at country cases more in-depth:
library(countrycode)
tab$country <- countrycode(tab$Var1, "iso2c", "country.name", warn = TRUE, nomatch = NA, custom_dict = NULL, custom_match = NULL, origin_regex = FALSE)

## create continent variable
dat$continent <- countrycode(dat$Residence, "iso2c", "continent",warn = TRUE, nomatch = NA, custom_dict = NULL, custom_match = NULL, origin_regex = FALSE)
table(dat$continent)

dat$Germany <- ifelse(dat$Residence == "DE", 1, 0)
dat$USA <- ifelse(dat$Residence == "US", 1, 0)

dat$German <- ifelse(dat$Nationality == "DE", 1, 0)
dat$American <- ifelse(dat$Nationality == "US", 1, 0)

## Dummies for Europe/US,Canada/Rest
dat$Europe <- ifelse(dat$continent== "Europe",1,0)
dat$Americas <- ifelse(dat$Residence == "US" | dat$Residence == "CA",1,0)
dat$Rest <- ifelse(dat$Europe  == 1 | dat$Americas ==1,0,1)

## Exclude participants from countries other than Europe/US, Canada
dat <- subset(dat, !dat$Rest == 1)

## check how many different countries in sample
count <- dat %>% group_by(Residence) %>% summarise(sum = length(Residence))


################### Calculate number of days between the first Covid-19 case and date of participation #####################
### Add new variable with the number of days between first infection in repondent's country of residence and day of participating in the survey. 

## load data about dates of first infection per country
dates <- read.csv("infection_dates.csv", header = T, sep = ";")
dates$cc <- countrycode(dates$Country, "country.name", "iso2c")
dat <- merge(dat, dates,by.x = "Residence", by.y = "cc")

dat$Date.of.First.Case <- as.Date(dat$Date.of.First.Case,format = "%d.%m.%y")
dat$date <- as.Date(dat$date, format = "%Y-%m-%d %H:%M:%S")
dat$date_diff <- dat$date - dat$Date.of.First.Case
range(dat$date_diff)


################################################################

describeBy(dat[,c("ToP_Amount", "ToP_Freqency", "ToP_Inhouse", "ToP_Ratio")], dat$Europe) # In Germany poeple have less ToP in house
describeBy(dat[,c("ToP_Amount", "ToP_Freqency", "ToP_Inhouse", "ToP_Ratio")], dat$Americas) # In the USA poeple have more ToP in house

describeBy(dat[,c("ToP_Amount", "ToP_Freqency", "ToP_Inhouse", "ToP_Ratio")], dat$Europe) # Germans have less ToP in house
describeBy(dat[,c("ToP_Amount", "ToP_Freqency", "ToP_Inhouse", "ToP_Ratio")], dat$Americas) # Americans have more ToP in house

dat$Residence <- factor(dat$Residence, levels = c("DE", levels(dat$Residence)[-10]))


#----------------------------------------------------------------
# 3. Save processed data
#----------------------------------------------------------------

rm(list=setdiff(ls(),"dat"))
save.image("topaq_processed.rdata")


#----------------------------------------------------------------
# 4. Explore ToP effects
#----------------------------------------------------------------

#-----------------------------------------------
# control for covariates via multiple regression

# rescale ToP_InHouse to the unit of loo rolls
dat$ToP_Inhouse <- ifelse(dat$ToP_Inhouse ==1, 0, dat$ToP_Inhouse)
dat$ToP_Inhouse <- ifelse(dat$ToP_Inhouse ==2, 2.5, dat$ToP_Inhouse)
dat$ToP_Inhouse <- ifelse(dat$ToP_Inhouse ==3, 6.5, dat$ToP_Inhouse)
dat$ToP_Inhouse <- ifelse(dat$ToP_Inhouse ==4, 10.5, dat$ToP_Inhouse)
dat$ToP_Inhouse <- ifelse(dat$ToP_Inhouse ==5, 14.5, dat$ToP_Inhouse)
dat$ToP_Inhouse <- ifelse(dat$ToP_Inhouse ==6, 18.5, dat$ToP_Inhouse)
dat$ToP_Inhouse <- ifelse(dat$ToP_Inhouse ==7, 22.5, dat$ToP_Inhouse)

# rescale predictors and continuous covariates to the unit of SDs
dat$HH <- scale(dat$HH)
dat$E <- scale(dat$E)
dat$X <- scale(dat$X)
dat$A <- scale(dat$A)
dat$C <- scale(dat$C)
dat$O <- scale(dat$O)
dat$z.Left_Right_Politics <- scale(dat$Left_Right_Politics)
dat$z.Corona <- scale(dat$Corona)
dat$z.Age <- scale(dat$Age)
dat$z.ToP_Amount <- scale(dat$ToP_Amount)
dat$z.ToP_Freqency <- scale(dat$ToP_Freqency)
dat$z.ToP_Inhouse <- scale(dat$ToP_Inhouse)
dat$z.Quarantine_Duration <- scale(dat$Quarantine_Duration)
dat$z.Household_Size <- scale(dat$Household_Size)
dat$z.date_diff <- scale(dat$date_diff)

## Recode Gender because of category diverse
dat$Gender <- as.factor(dat$Gender)
head(dat$Gender)

## Recode Variable Europe/USA for graphs
dat$Cat.Country <- ifelse(dat$Europe == 1, "Europe",
                          ifelse(dat$Americas == 1,"Americas",NA))

################################################################
############# PERCEIVED THREAT OF CORONA ########################

# baseline model 
m_corona <- lm(z.Corona ~ z.Left_Right_Politics +  Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(m_corona)


# linear model HH
m_corona_HH <- lm(z.Corona ~ HH + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(m_corona_HH)

# linear model E
m_corona_E <-lm(z.Corona ~ E + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(m_corona_E)

# interaction model E
m_corona_E2 <-lm(z.Corona ~ E*Europe + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + z.date_diff, data = dat)
summary(m_corona_E2)

# linear model X
m_corona_X <-lm(z.Corona ~ X + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(m_corona_X)

# linear model A
m_corona_A <-lm(z.Corona ~ A + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(m_corona_A)

# linear model C
m_corona_C <-lm(z.Corona ~ C + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(m_corona_C)

# linear model O
m_corona_O <-lm(z.Corona ~ O + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(m_corona_O)

################################################################################## SHOPPING FREQUENCY ############################

# baseline model 
fit_topfrequency_baseline <- lm(z.ToP_Freqency ~ z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topfrequency_baseline)

# Corona Model 
fit_topfrequency_Corona <- lm(z.ToP_Freqency ~ z.Corona + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topfrequency_Corona)

# Interaction model 
fit_topfrequency_Corona_IA <- lm(z.ToP_Freqency ~ z.Corona*Europe + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + z.date_diff, data = dat)
summary(fit_topfrequency_Corona_IA)

# linear model HH
fit_topfrequency_HH <- lm(z.ToP_Freqency ~ HH + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topfrequency_HH)

# linear model E
fit_topfrequency_E <- lm(z.ToP_Freqency ~ E + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topfrequency_E)

# linear model X
fit_topfrequency_X <- lm(z.ToP_Freqency ~ X + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topfrequency_X)

# linear model A
fit_topfrequency_A <- lm(z.ToP_Freqency ~ A + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topfrequency_A)

# linear model C
fit_topfrequency_C <- lm(z.ToP_Freqency ~ C + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topfrequency_C)

# interaction model C
fit_topfrequency_C2 <- lm(z.ToP_Freqency ~ C*Europe + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + z.date_diff, data = dat)
summary(fit_topfrequency_C2)

# linear model O
fit_topfrequency_O <- lm(z.ToP_Freqency ~ O + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topfrequency_O)

##### SEM Model for indirect effect of E on Top_frequency
dat$Gender_SEM <- ifelse(dat$Gender == 3,NA,dat$Gender)
SEM_topfrequency <- 'z.Corona ~ a*E 
z.ToP_Freqency ~ E + b*z.Corona + z.Left_Right_Politics + z.Age + Gender_SEM + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff
ind:=a*b'
fit1 <- sem(SEM_topfrequency,data=dat,meanstructure=T,se="bootstrap")
summary(fit1,fit.measure=T, ci=T)

#################################################################
##################### Shopping intensity  #######################

# baseline model
fit_amount_baseline <- lm(z.ToP_Amount ~ z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_amount_baseline)

# Corona Model 
fit_amount_Corona <- lm(z.ToP_Amount ~ z.Corona + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_amount_Corona)

# Interaction model 
fit_amount_Corona_IA <- lm(z.ToP_Amount ~ z.Corona*Europe + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + z.date_diff, data = dat)
summary(fit_amount_Corona_IA)

# linear model HH
fit_amount_HH <- lm(z.ToP_Amount ~ HH + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_amount_HH)

# linear model E
fit_amount_E <- lm(z.ToP_Amount ~ E + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_amount_E)

# linear model X
fit_amount_X <- lm(z.ToP_Amount ~ X + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_amount_X)

#linear model A
fit_amount_A <- lm(z.ToP_Amount ~ A + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_amount_A)

# linear model C
fit_amount_C <- lm(z.ToP_Amount ~ C + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_amount_C) 

# interaction model C
fit_amount_C_IA <- lm(z.ToP_Amount ~ C*Europe + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + z.date_diff, data = dat)
summary(fit_amount_C_IA) 

# linear model O
fit_amount_O <- lm(z.ToP_Amount ~ O + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_amount_O)


##### SEM Model for indirect effect of E on Top_amount
SEM_topamount <- 'z.Corona ~ a*E 
z.ToP_Amount ~ E + b*z.Corona + z.Left_Right_Politics + z.Age + Gender_SEM + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff
ind:=a*b'
fit2 <- sem(SEM_topamount,data=dat,meanstructure=T,se="bootstrap")
summary(fit2,fit.measure=T,ci=T)

#################################################################
################### Stocked toilet rolls ########################

# baseline model
fit_topinhouse_baseline <- lm(z.ToP_Inhouse ~ z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topinhouse_baseline)

# Corona model 
fit_topinhouse_Corona <- lm(z.ToP_Inhouse ~ z.Corona + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topinhouse_Corona)

# Interaction model 
fit_topinhouse_Corona_IA <- lm(z.ToP_Inhouse ~ z.Corona*Europe + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topinhouse_Corona_IA)

# linear model HH
fit_topinhouse_HH <- lm(z.ToP_Inhouse ~ HH + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topinhouse_HH)

# linear model E
fit_topinhouse_E <- lm(z.ToP_Inhouse ~ E + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topinhouse_E)

# linear model X
fit_topinhouse_X <- lm(z.ToP_Inhouse ~ X + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff, data = dat)
summary(fit_topinhouse_X)

# linear model A
fit_topinhouse_A <- lm(z.ToP_Inhouse ~ A + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe+ z.date_diff, data = dat)
summary(fit_topinhouse_A)

# linear model C
fit_topinhouse_C <- lm(z.ToP_Inhouse ~ C + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe+ z.date_diff, data = dat) 
summary(fit_topinhouse_C)

# interaction model C
fit_topinhouse_C2 <- lm(z.ToP_Inhouse ~ C*Europe + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + z.date_diff, data = dat) 
summary(fit_topinhouse_C2)

# linear model O
fit_topinhouse_O <- lm(z.ToP_Inhouse ~ O + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe+ z.date_diff, data = dat)
summary(fit_topinhouse_O) 

# interaction model O
fit_topinhouse_O2 <- lm(z.ToP_Inhouse ~ O*Europe + z.Left_Right_Politics + Gender + z.Age + z.Household_Size + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + z.date_diff, data = dat)
summary(fit_topinhouse_O2) 

##### Exclude diverse from Gender and include as CV
SEM_topinhouse <- 'z.Corona ~ a*E 
z.ToP_Inhouse ~ E + b*z.Corona +  z.Left_Right_Politics + z.Age + z.Household_Size + Gender_SEM + z.Quarantine_Duration + Personal_Mobility_Restriction + Public_Transport_Restriction + Europe + z.date_diff
ind:=a*b'
fit3 <- sem(SEM_topinhouse,data=dat,meanstructure=T,se="bootstrap")
summary(fit3,fit.measure=T, ci=T)

