library(knitr)
library(mrds)
library(Distance)

#Access the golf tee data
data(book.tee.data)

#Data are in a hierarchical structure rather than in a ‘flat file’ format
#Extract the list elements from the dataset into easy-to-access objects
detections <- book.tee.data$book.tee.dataframe # detection information
region <- book.tee.data$book.tee.region # region info
samples <- book.tee.data$book.tee.samples # transect info
obs <- book.tee.data$book.tee.obs # links detections to transects and regions

#In the detections dataframe, define sex and exposure as factor variables 
detections$sex <- as.factor(detections$sex)
detections$exposure <- as.factor(detections$exposure)

#Fit trial configuration with full independence model
fi.mr.dist <- ddf(method='trial.fi',mrmodel=~glm(link='logit',formula=~distance),data=detections,meta.data=list(width=4))

#Create a set of tables summarizing the double observer data 
detection.tables <- det.tables(fi.mr.dist)
#Print these detection tables
detection.tables

#observer 2 is the person setting up the trial, aka the tracker
#observer 1 is the person who will 'recapture' the animals by detecting them again, aka the primary
#duplicates are when both observers detected the animal (aka resighting or success)

#Plot detection information, change number to see other plots
#1-Histograms of distances for detections by either, or both, observers. The shaded regions show the number for observer 1.
#2-Histograms of distances for detections by either, or both, observers. The shaded regions show the number for observer 2.
#3-Histograms of distances for duplicates (detected by both observers).
#4-Histogram of distances for detections by either, or both, observers. Not shown for trial configuration.
#5-Histograms of distances for observer 2. The shaded regions indicate the number of duplicates - for example, the shaded region is the number of clusters in each distance bin that were detected by Observer 1 given that they were also detected by Observer 2 (the “|” symbol in the plot legend means “given that”).
#6-Histograms of distances for observer 1. The shaded regions indicate the number of duplicates as for plot 5. Not shown for trial configuration.
plot(detection.tables,which=1)

#Produce a summary of the fitted detection function object
summary(fi.mr.dist)

#Produce goodness of fit statistics and a qq plot
gof.result <- ddf.gof(fi.mr.dist,main="Full independence, trial configuration\ngoodness of fit Golf tee data")
#points are consistently below the line
gof.result
#Distance sampling component Total Chisquare = 11.508
#Mark-recapture component Total Chisquare = 3.4?

#Extract chi-square statistics for reporting in the text below (see Markdown file for how this is done).
chi.distance <- gof.result$chisquare$chi1$chisq
chi.markrecap <- gof.result$chisquare$chi2$chisq
chi.total <- gof.result$chisquare$pooled.chi

#Divide the plot region into 2 columns
par(mfrow=c(1,2))
#Plot detection functions
plot(fi.mr.dist)
#Observer = 1 detections shows the detections made by observer 1, with the detection function from the right plot of resightings overlaid starting at the intercept from the right plot
#if there is dependency between observer detections, we would see the detection function on the left plot decrease slower than the histograms
#what we see here is that the histograms fall faster than the detection function, and so there is unmodelled heterogeneity
#average detection probability (the detection function) doesn't have a wide shoulder, drops off rapidly
#detection probability at 0 distance is fairly close to 1

#Calculate density estimates using the dht function
tee.abund <- dht(model=fi.mr.dist,region.table=region,sample.table=samples,obs.table=obs)

#Print out results in a nice format
knitr::kable(tee.abund$individuals$summary,digits=2,caption="Survey summary statistics for golftees")
#381 estimated individuals, compared to the real value of 760

knitr::kable(tee.abund$individuals$N,digits=2,caption="Abundance estimates for golftee population with two strata")
#593 estimated individuals when two strata are used, compared to the real value of 760

#Example of adding covariates to MR detection function
fi.mr.dist.size.sex.exposure <- ddf(method='trial.fi',mrmodel=~glm(link='logit',formula=~distance+size+sex+exposure),data=detections,meta.data=list(width=4))
summary(fi.mr.dist.size.sex.exposure)
#estimates of exposure>sex>distance>size

par(mfrow=c(1,1))
gof.result.covariates <- ddf.gof(fi.mr.dist.size.sex.exposure,main="Full independence, trial configuration\ngoodness of fit Golf tee data with covariates")
#points fall on the line better, though still mostly below it

chi.distance.covariates <- gof.result.covariates$chisquare$chi1$chisq
chi.markrecap.covariates <- gof.result.covariates$chisquare$chi2$chisq
chi.total.covariates <- gof.result.covariates$chisquare$pooled.chi
par(mfrow=c(1,2))
plot(fi.mr.dist.size.sex.exposure,showpoints=FALSE)
AIC(fi.mr.dist,fi.mr.dist.size.sex.exposure)
#including the covariates is better

#remove the size covariate because it is the least significant based on the estimate above
fi.mr.dist.sex.exposure <- ddf(method='trial.fi',mrmodel=~glm(link='logit',formula=~distance+sex+exposure),data=detections,meta.data=list(width=4))
AIC(fi.mr.dist,fi.mr.dist.size.sex.exposure,fi.mr.dist.sex.exposure)
#removing size is better

#check for interactions between covariates
fi.mr.dist.sex.exposure.interactions <- ddf(method='trial.fi',mrmodel=~glm(link='logit',formula=~distance*sex*exposure),data=detections,meta.data=list(width=4))
AIC(fi.mr.dist,fi.mr.dist.size.sex.exposure,fi.mr.dist.sex.exposure,fi.mr.dist.sex.exposure.interactions)
#including interactions is better

#Fit trial configuration with point independence model
pi.mr.dist <- ddf(method='trial',mrmodel=~glm(link='logit',formula=~distance),dsmodel=~cds(key='hn'),data=detections,meta.data=list(width=4))

#Summary pf the model 
summary(pi.mr.dist)
#223 estimated individuals, compared to the real value of 760

#Produce goodness of fit statistics and a qq plot
par(mfrow=c(1,1))
gof.results <- ddf.gof(pi.mr.dist,main="Point independence, trial configuration\n goodness of fit Golftee data")
#the points now fall evenly above and below the line

AIC(fi.mr.dist.sex.exposure.interactions,pi.mr.dist)
#the full independence model is better

#Fit the PI-trial model - DS sex and MR distance 
pi.mr.dist.ds.sex <- ddf(method='trial',mrmodel=~glm(link='logit',formula=~distance),dsmodel=~mcds(key='hn',formula=~sex),data=detections,meta.data=list(width=4))
AIC(fi.mr.dist.sex.exposure.interactions,pi.mr.dist,pi.mr.dist.ds.sex)
#the full independence model is better

fi.mr.dist.ds.sex <- ddf(method='trial.fi',mrmodel=~glm(link='logit',formula=~distance+sex),data=detections,meta.data=list(width=4))
AIC(fi.mr.dist.sex.exposure.interactions,pi.mr.dist,pi.mr.dist.ds.sex,fi.mr.dist.ds.sex)
#not including the distance sampling is better

fi.mr.dist.ds.sex.exposure.interactions <- ddf(method='trial.fi',mrmodel=~glm(link='logit',formula=~distance*sex*exposure),data=detections,meta.data=list(width=4))
AIC(fi.mr.dist.sex.exposure.interactions,pi.mr.dist,pi.mr.dist.ds.sex,fi.mr.dist.ds.sex,fi.mr.dist.ds.sex.exposure.interactions)
#AIC is equivalent whether including distance sampling or not

pi.mr.dist.ds.sex.exposure.interactions <- ddf(method='trial',mrmodel=~glm(link='logit',formula=~distance*sex*exposure),dsmodel=~mcds(key='hn',formula=~sex),data=detections,meta.data=list(width=4))
AIC(fi.mr.dist.sex.exposure.interactions,pi.mr.dist,pi.mr.dist.ds.sex,fi.mr.dist.ds.sex,fi.mr.dist.ds.sex.exposure.interactions,pi.mr.dist.ds.sex.exposure.interactions)
#AIC is slightly better when including distance sampling and interactions between distance, sex, and exposure

summary(pi.mr.dist.ds.sex.exposure.interactions)
#Average p estimate = 0.56
#Average primary p(0) = 0.94, which is close enough to 1 to not have used MRDS

par(mfrow=c(1,1))
gof.result.covariates <- ddf.gof(fi.mr.dist.size.sex.exposure,main="Full independence, trial configuration\ngoodness of fit Golf tee data with covariates")
#points fall on the line better, though still mostly below it

chi.distance.covariates <- gof.result.covariates$chisquare$chi1$chisq
chi.markrecap.covariates <- gof.result.covariates$chisquare$chi2$chisq
chi.total.covariates <- gof.result.covariates$chisquare$pooled.chi
par(mfrow=c(1,2))
plot(fi.mr.dist.size.sex.exposure,showpoints=FALSE)

#Crabeater seal aerial survey----

#Check detections
head(detections)
#object = cluster in this case
#observer 1 = front of helicopter, one person on either side made up team 1
#observer 2 = back of helicopter, one person on either side made up team 2

crabseal <- read.csv("crabbieMRDS.csv") #csv not available
str(crabseal)

crabseal$side <- as.factor(crabseal$side)
crabseal$vis <- as.factor(crabseal$vis)
crabseal$glare <- as.factor(crabseal$glare)
crabseal$observer <- as.factor(crabseal$observer)
crabseal$obsname <- as.factor(crabseal$obsname)

#IO configuration - full independence
#MR model - distance only
#Truncation 700 m
seal.fi.mr.dist <- ddf(method="io.fi",mrmodel=~glm(link="logit",formula=~distance),data=crabseal,meta.data=list(width=700))
summary(seal.fi.mr.dist)
#Average p = 0.907 #all observers across all distances
#Average primary p(0) = 0.892 #both observers in front
#Average secondary p(0) = 0.901 #both observers in back
#Average combined p(0) = 0.988 #all observers

plot(seal.fi.mr.dist,showpoints=FALSE)
gof.seal <- ddf.gof(seal.fi.mr.dist,main="Full independence, independent observer configuration\ngoodness of fit Crabeater seal data")

#IO configuration - point independence
#MR model - distance only
#DS model - half normal detection function, no additional covars
#Truncation at 700m
seal.pi.mr.dist.ds.hn <- ddf(method="io",dsmodel=~cds(key="hn"),mrmodel=~glm(link="logit",formula=~distance),data=crabseal,meta.data=list(width=700))
summary(seal.pi.mr.dist.ds.hn)
#AIC is much lower when using point independence

#to look at a subset of the data when creating models is taking a long time because of the amount of data,
#can use 'fold' to pull out every 5th row, for example
crabseal15 <- crabseal[crabseal$fold %in% c(1, 5), ]

seal.pi.mr.dist.ds.hn.fold15 <- ddf(method="io",dsmodel=~cds(key="hn"),mrmodel=~glm(link="logit",formula=~distance),data=crabseal15, meta.data=list(width=700))
seal.pi.mr.dist.observer.ds.hn.fold15 <- ddf(method="io",dsmodel=~cds(key="hn"),mrmodel=~glm(link="logit",formula=~distance+observer),data=crabseal15,meta.data=list(width=700))
#both models returned errors that process.data(data,meta.data) objects do not have records for both observers

AIC(seal.pi.mr.dist.ds.hn.fold15,seal.pi.mr.dist.observer.ds.hn.fold15)
#including observer as a covariate is better

#Create tables for estimating abundance 
#Selecting observer==1 ensures that observations in the obs.table are unique 
tables <- Distance:::checkdata(crabseal[crabseal$observer==1, ])

#Estimate abundance in covered region, convert.units=0.001 adjusts the units of perpendicular distance (m) to the units of transect effort (km)
pi.abund <- dht(model=seal.pi.mr.dist.ds.hn,region=tables$region.table,sample=tables$sample.table,obs=tables$obs.table,se=TRUE,options=list(convert.units=0.001))

#Pretty tables of data summary
knitr::kable(pi.abund$individuals$summary,digits=3,caption="Summary information from crabeater seal aerial survey")

#Pretty tables of estimates of individual abundance
knitr::kable(pi.abund$individual$N,digits=3,caption="Crabeater seal abundance estimates for study area of arbitrary size")

#Read in data
crab.covar <- read.csv("crabbieMCDS.csv") #csv not available
#Check data imported OK
head(crab.covar, n=3)

#Define factor variables
crab.covar$side <- as.factor(crab.covar$side) #which side of the plane
crab.covar$vis <- as.factor(crab.covar$vis) #visibility (Poor, Good, Excellent)

#Fit HN key function with side of plane
ds.side <- ds(crab.covar,key="hn",formula=~side,truncation=700)

#Divide plot region
par(mfrow = c(1, 2))
#Create a title for the plot
plot.title <- "Two sets of points\none for each 'side' of plane"
#Plot model
plot(ds.side,pch=19,cex=0.5,main=plot.title)
#Plot qq plot
gof.result <- gof_ds(ds.side,lwd=2,lty=1,pch=".",cex=0.5)
#Extract gof statistics
message <- paste("CVM GOF p-value=",round(gof.result$dsgof$CvM$p,4))
# Add gof stats to plot
text(0.6,0.2,message,cex=0.5)

#Fit HN key function with no covars and no adjustments
ds.nocov <- ds(crab.covar,key="hn",adjustment=NULL,truncation=700)
#AIC shows that including side as a covariate is better

#Fit HN key function with visibility
ds.vis <- ds(crab.covar,key="hn",formula=~vis,truncation=700)
#Fit HN key function with side+visibility
ds.side.vis <- ds(crab.covar,key="hn",formula=~side+vis,truncation=700)

AIC(ds.side,ds.vis,ds.side.vis)
#including both covariates is better

#Fit HN key function with no covars and no adjustments
ds.nocov.hncos <- ds(crab.covar,key="hn",adjustment="cos",truncation=700)
#cosine(2,3) was selected

#p(0) was close to 1 so MRDS wasn't needed and MCDS was adequate, but a double platform survey was required to know this
