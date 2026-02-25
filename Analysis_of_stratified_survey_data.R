library(Distance)
library(kableExtra)

data(minke)
head(minke)

# Specify truncation distance
minke.trunc <- 1.5

# Create dataset for South 
minke.S <- minke[minke$Region.Label=="South", ]

# Fit df to south
minke.df.S.strat <- ds(minke.S,key="hr",adjustment=NULL,truncation=minke.trunc)
summary(minke.df.S.strat)
#AIC = 8.617
#Average p/detectability = 0.49
#Abundance estimate = 4587
#Density estimate = 0.05

# Create dataset for North 
minke.N <- minke[minke$Region.Label=="North", ]

# Fit df to north
minke.df.N.strat <- ds(minke.N,key="hr",adjustment=NULL,truncation=minke.trunc)
summary(minke.df.N.strat)
#AIC = 37.278
#Average p/detectability = 0.75
#Abundance estimate = 9986
#Density estimate = 0.01
#We expected abundance to be higher in the south which it was not, but that's because the covered
#area in the north was much larger, and therefore the density is higher in the south

aic.S <- summary(minke.df.S.strat$ddf)$aic #AIC = 8.617
aic.N <- summary(minke.df.N.strat$ddf)$aic #AIC = 37.278
aic.SN <- aic.S+aic.N #Total stratified AIC = 45.895

minke.df.all <- ds(minke,truncation=minke.trunc,key="hr",adjustment=NULL)
summary(minke.df.all)

aic.all <- summary(minke.df.all$ddf)$aic 
#Total unstratified AIC = 48.637, which is higher than North+South so separate detection functions should be fit to each strata
#Total abundance estimate = 14,573

#dht2 uses an existing detection function to estimate abundance over specified regions
dht2(ddf=minke.df.all,flatfile=minke,strat_formula=~Region.Label,stratification="geographical") #can add convert_units if they weren't all the same
#Total AIC = 45.895
#Total abundance estimate = 15,835
#the uncertainty in the total population size is computed, rather than needing to be calculated manually using the delta method
