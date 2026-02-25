library(Distance)

#point transects of the Hawaiian amakihi bird----
data(amakihi)
head(amakihi,n=3)
#Covariates:
#OBs = observer initials
#MAS = minutes after sunrise
#HAS = hours after sunrise

#Data exploration includes assessing the shape, considering truncation, and exploring patterns in covariates
#assessing the shape/distribution of distances to decide on a trunction distance
hist(amakihi$distance)
#drops off after 100

# Boxplots by obs (discrete/factor)
boxplot(amakihi$distance~amakihi$OBs,xlab="Observer",ylab="Distance (m)")
#mean and variance are higher for TJS than SGF or TKP

# Boxplots by hour after sunrise (discrete/factor)
boxplot(amakihi$distance~amakihi$HAS,xlab="Hour",ylab="Distance (m)")
#variance is higher earlier in the day

# Scatterplot of minutes after sunrise vs distance (continuous)
plot(x=amakihi$MAS,y=amakihi$distance,xlab="Minutes after sunrise",ylab="Distance (m)",pch=20)
#variance is higher earlier in the day

#MAS and HAS are correlated, so you only want to use one 
plot(x=amakihi$MAS,y=amakihi$HAS,xlab="Minutes after sunrise",ylab="Hours after sunrise",pch=20)
#I would lean toward the continuous data because discrete data are harder to fit models to
#but this exercise used HAS, maybe because there is already another discrete covariate

# Convert HAS to a factor to allow the relationship with detectability to be non-linear
amakihi$HAS <- factor(amakihi$HAS)

# Set the reference level 
amakihi$OBs <- relevel(amakihi$OBs,ref="TKP")
amakihi$HAS <- relevel(amakihi$HAS,ref="5")

# With three potential covariates, there are 8 possible models for the detection function:
# No covariates
# OBs
# HAS
# MAS
# OBs + HAS
# OBs + MAS
# HAS + MAS
# OBs + HAS + MAS

#Note that covariates are not allowed if a uniform key function is chosen 
#and if covariate terms are included, adjustment terms are not allowed

conversion.factor <- convert_units("meter",NULL,"hectare")

#no covariates, truncating at 10% gives a max of 90 m
amak.hr <- ds(amakihi,transect="point",key="hr",truncation="10%",adjustment=NULL,convert_units = conversion.factor)

#OBs covariate
amak.hr.obs <- ds(amakihi,transect="point",key="hr",formula=~OBs,truncation="10%",convert_units = conversion.factor)

#HAS covariate
amak.hr.has <- ds(amakihi,transect="point",key="hr",formula=~HAS,truncation="10%",convert_units = conversion.factor)

#OBs+HAS covariates
amak.hr.obs.has <- ds(amakihi,transect="point",key="hr",formula=~OBs+HAS,truncation="10%",convert_units = conversion.factor)

AIC(amak.hr,amak.hr.obs,amak.hr.has,amak.hr.obs.has)
summarize_ds_models(amak.hr,amak.hr.obs,amak.hr.has,amak.hr.obs.has)
plot(amak.hr.obs.has,showpoints=FALSE)

#visualize the different detection functions for each combination of covariates - not sure why they don't surround the overall detection function
TJS_0 <- data.frame(OBs="TJS",HAS=0)
TJS_5 <- data.frame(OBs="TJS",HAS=5)
SGF_0 <- data.frame(OBs="SGF",HAS=0)
SGF_5 <- data.frame(OBs="SGF",HAS=5)
TKP_0 <- data.frame(OBs="TKP",HAS=0)
TKP_5 <- data.frame(OBs="TKP",HAS=5)
add_df_covar_line(amak.hr.obs.has,data=TJS_0,lty=1,lwd=2,col="blue",pdf=TRUE)
add_df_covar_line(amak.hr.obs.has,data=TJS_5,lty=2,lwd=2,col="blue",pdf=TRUE)
add_df_covar_line(amak.hr.obs.has,data=SGF_0,lty=1,lwd=2,col="red",pdf=TRUE)
add_df_covar_line(amak.hr.obs.has,data=SGF_5,lty=2,lwd=2,col="red",pdf=TRUE)
add_df_covar_line(amak.hr.obs.has,data=TKP_0,lty=1,lwd=2,col="green",pdf=TRUE)
add_df_covar_line(amak.hr.obs.has,data=TKP_5,lty=2,lwd=2,col="green",pdf=TRUE)

#check that there are no counts with p<=0.1, and <5% with p<=0.2
p_dist_table(amak.hr.obs.has,proportion=TRUE)
#20% has p<=0.2, so could change truncation=70 rather than 10% = 90 m

#line transects of the Eastern Tropical Pacific spotted dolphin----
#surveyed on tuna vessels because the dolphins often associate with yellowfin tuna

data(ETP_Dolphin) #previously truncated at 5 nm
head(ETP_Dolphin,n=3)
#Covariates:
#Month 6 = June, 7 = July, 8 = August
#Beauf.class 1 = Beaufort 0-2, 2 = Beaufort 3-5
#Cue.type 1 = birds flying above the school, 2 = splashes on the water, 3 = unspecified, 4 = floating objects such as logs
#Search.method 0 = 20x binoculars from the crow's nest, 2 = 20x binoculars from another location on the vessel, 3 = helicopter, 5 = bird radar

ETP_Dolphin_units[,1:2]

hist(ETP_Dolphin$distance,n=100)
#spiked and heaped

boxplot(ETP_Dolphin$distance~ETP_Dolphin$Month,xlab="Month",ylab="Distance (nm)")
#mean lower in July but variance equal between months

boxplot(ETP_Dolphin$distance~ETP_Dolphin$Beauf.class,xlab="Beaufort class",ylab="Distance (nm)")
#generally equal

boxplot(ETP_Dolphin$distance~ETP_Dolphin$Cue.type,xlab="Cue type",ylab="Distance (nm)")
#means higher for 2 and 4, variance higher for 1 and 3

boxplot(ETP_Dolphin$distance~ETP_Dolphin$Search.method,xlab="Search method",ylab="Distance (nm)")
#mean higher for 3, variance lower for 5

plot(x=ETP_Dolphin$Cue.type,y=ETP_Dolphin$Search.method,xlab="Cue type",ylab="Search method",pch=20)
#there appears to be a correlation between cue type and search method, but I'm not sure this is the best way to look at it

ETP_Dolphin$Month <- factor(ETP_Dolphin$Month)
ETP_Dolphin$Beauf.class <- factor(ETP_Dolphin$Beauf.class)
ETP_Dolphin$Cue.type <- factor(ETP_Dolphin$Cue.type)
ETP_Dolphin$Search.method <- factor(ETP_Dolphin$Search.method)

#compare half normal and hazard rate key functions
ETP.hn <- ds(ETP_Dolphin,key="hn",adjustment=NULL)
ETP.hr <- ds(ETP_Dolphin,key="hr",adjustment=NULL)
knitr::kable(as.data.frame(AIC(ETP.hn,ETP.hr))) %>%
  kable_styling(bootstrap_options="condensed",full_width=F)  #returns df and AIC as table-plot

#no covariates
ETP.hr <- ds(ETP_Dolphin,transect="line",key="hr")
#warning that it is spiked

#cue type covariate
ETP.hr.cue <- ds(ETP_Dolphin,transect="line",key="hr",formula=~Cue.type)

#search method covariate
ETP.hr.search <- ds(ETP_Dolphin,transect="line",key="hr",formula=~Search.method)

#cue type and search method covariates
ETP.hr.cue.search <- ds(ETP_Dolphin,transect="line",key="hr",formula=~Cue.type+Search.method)

AIC(ETP.hr,ETP.hr.cue,ETP.hr.search,ETP.hr.cue.search)
#cue+search returned the lowest AIC
#but when using the script in the solution, it shows that search alone has the lowest AIC
knitr::kable(summarize_ds_models(ETP.hr,ETP.hr.cue,ETP.hr.search,output="plain")[,2:7],row.names=FALSE,caption="ETP dolphin model selection.",digits=3) %>%
  kable_styling(bootstrap_options="condensed",full_width=F)  

plot(ETP.hr.cue.search,showpoints=FALSE)
cue1_search0 <- data.frame(Cue.type=1,Search.method=0)
cue2_search2 <- data.frame(Cue.type=2,Search.method=2)
cue3_search3 <- data.frame(Cue.type=3,Search.method=3)
cue4_search5 <- data.frame(Cue.type=4,Search.method=5)
add_df_covar_line(ETP.hr.cue.search,data=cue1_search0,lty=1,lwd=2,col="red",pdf=TRUE)
add_df_covar_line(ETP.hr.cue.search,data=cue2_search2,lty=1,lwd=2,col="blue",pdf=TRUE)
add_df_covar_line(ETP.hr.cue.search,data=cue3_search3,lty=1,lwd=2,col="green",pdf=TRUE)
add_df_covar_line(ETP.hr.cue.search,data=cue4_search5,lty=1,lwd=2,col="purple",pdf=TRUE)

#look at variation (se) for each covariate
print(ETP.hr.cue.search$ddf)
#se is very high for search method 3 = helicopter, so it would be good to remove this method because the shapes should be the same

gof_ds(ETP.hr.cue.search)
#fits well except for a slight negative deviation at the beginning

plot(ETP.hr.cue,main="ETP dolphin survey",showpoints=FALSE)
add_df_covar_line(ETP.hr.cue,data=data.frame(Cue.type=1),col='red',lwd=2,lty=1)
add_df_covar_line(ETP.hr.cue,data=data.frame(Cue.type=2), col='blue',lwd=2,lty=1)
add_df_covar_line(ETP.hr.cue,data=data.frame(Cue.type=3),col='green',lwd=2,lty=1)
add_df_covar_line(ETP.hr.cue,data=data.frame(Cue.type=4),col='purple',lwd=2,lty=1)
legend("topright",legend=c("Birds","Splashes","Unspecified","Floating objects"),
       col=c("red","blue","green","purple"),lwd=2,title="Cue type")
