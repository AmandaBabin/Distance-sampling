#----
library(Distance)

data(LTExercise)
head(LTExercise)
summary(LTExercise$distance) 
#shows 1 row with NA
LTExercise[100:102, ] 
#row numbers provided to show which Sample.Label aka line transect identifier has the NA = Line 11
#transects with no detections need to be retained

conversion.factor_1 <- convert_units("meter","kilometer","square kilometer")

#Fit half normal, no adjustments
lt.hn <- ds(data=LTExercise,key="hn",adjustment=NULL,convert_units=conversion.factor_1)
summary(lt.hn)
#there are 105 objects in total
#the maximum observed perpendicular distance is 35.8 m
plot(lt.hn,nc=30)
#shows a large gap in the observed distances which requires truncation
#can exclude distances beyond a specified distance, or
#can exclude a specified percentage of the largest distances (aka right truncation)

#Truncate at 20 m
lt.hn.t20m <- ds(data=LTExercise,key="hn",adjustment=NULL,truncation=20,convert_units=conversion.factor_1)
summary(lt.hn.t20m)
plot(lt.hn.t20m,nc=30)
#there are now 103 objects in total

#Truncate largest 10% of distances
lt.hn.t10per <- ds(data=LTExercise,key="hn",adjustment=NULL,truncation="10%",convert_units=conversion.factor_1)
summary(lt.hn.t10per)
plot(lt.hn.t10per)
#there are now 94 objects in total

#Half normal detection, cosine adjustments, no truncation
lt.hn.cos <- ds(data=LTExercise,key="hn",adjustment="cos",convert_units=conversion.factor_1)
#AIC is used to determine how many, if any, adjustment terms are required
#in this case, adding 2 cosine adjustments increases the AIC so no adjustment terms are required

#Other detection functions:
#key="unif" for uniform
#key="hr" for hazard rate
#adjustment="cos" for cosine, default, can be used with uniform or half normal key functions
#adjustment="herm" for hermite polynomial, can be used with half normal key function
#adjustment="poly" for simple polynomial, can be used with hazard rate key function

#non-truncated
lt.uf <- ds(LTExercise,key="unif",adjustment=NULL,convert_units=conversion.factor_1)
summary(lt.uf)
plot(lt.uf,nc=30,main="Uniform, no adjustments")

lt.uf.cos <- ds(LTExercise,key="unif",adjustment="cos",convert_units=conversion.factor_1)
summary(lt.uf.cos)
plot(lt.uf.cos,nc=30,main="Uniform, cosine adjustments")

lt.hn <- ds(LTExercise,key="hn",adjustment=NULL,convert_units=conversion.factor_1)
summary(lt.hn)
plot(lt.hn,nc=30,main="Half normal, no adjustments")

lt.hn.herm <- ds(LTExercise,key="hn",adjustment="herm",convert_units=conversion.factor_1)
summary(lt.hn.herm)
plot(lt.hn.herm,nc=30,main="Half normal, herm adjustments")

lt.hr <- ds(LTExercise,key="hr",adjustment=NULL,convert_units=conversion.factor_1)
summary(lt.hr)
plot(lt.hr,nc=30,main="Hazard rate, no adjustments")

lt.hr.poly <- ds(LTExercise,key="hr",adjustment="poly",convert_units=conversion.factor_1)
summary(lt.hr.poly)
plot(lt.hr.poly,nc=30,main="Hazard rate, poly adjustments")

#Truncate at 20 m
lt.uf.t20m <- ds(LTExercise,key="unif",adjustment=NULL,truncation=20,convert_units=conversion.factor_1)
summary(lt.uf.t20m)
plot(lt.uf.t20m,nc=30,main="Uniform, no adjustments")

lt.uf.cos.t20m <- ds(LTExercise,key="unif",adjustment="cos",truncation=20,convert_units=conversion.factor_1)
summary(lt.uf.cos.t20m)
plot(lt.uf.cos.t20m,nc=30,main="Uniform, cosine adjustments")

lt.hn.cos.t20m <- ds(LTExercise,key="hn",adjustment="cos",truncation=20,convert_units=conversion.factor_1)
summary(lt.hn.cos.t20m)
plot(lt.hn.cos.t20m,nc=30,main="Half normal, cosine adjustments")

lt.hn.herm.t20m <- ds(LTExercise,key="hn",adjustment="herm",truncation=20,convert_units=conversion.factor_1)
summary(lt.hn.herm.t20m)
plot(lt.hn.herm.t20m,nc=30,main="Half normal, herm adjustments")

lt.hr.t20m <- ds(LTExercise,key="hr",adjustment=NULL,truncation=20,convert_units=conversion.factor_1)
summary(lt.hr.t20m)
plot(lt.hr.t20m,nc=30,main="Hazard rate, no adjustments")

lt.hr.poly.t20m <- ds(LTExercise,key="hr",adjustment="poly",truncation=20,convert_units=conversion.factor_1)
summary(lt.hr.poly.t20m)
plot(lt.hr.poly.t20m,nc=30,main="Hazard rate, poly adjustments")

#Truncate largest 10% of distances
lt.uf.t10per <- ds(LTExercise,key="unif",adjustment=NULL,truncation="10%",convert_units=conversion.factor_1)
summary(lt.uf.t10per)
plot(lt.uf.t10per,nc=30,main="Uniform, no adjustments")

lt.uf.cos.t10per <- ds(LTExercise,key="unif",adjustment="cos",truncation="10%",convert_units=conversion.factor_1)
summary(lt.uf.cos.t10per)
plot(lt.uf.cos.t10per,nc=30,main="Uniform, cosine adjustments")

lt.hn.cos.t10per <- ds(LTExercise,key="hn",adjustment="cos",truncation="10%",convert_units=conversion.factor_1)
summary(lt.hn.cos.t10per)
plot(lt.hn.cos.t10per,nc=30,main="Half normal, cosine adjustments")

lt.hn.herm.t10per <- ds(LTExercise,key="hn",adjustment="herm",truncation="10%",convert_units=conversion.factor_1)
summary(lt.hn.herm.t10per)
plot(lt.hn.herm.t10per,nc=30,main="Half normal, herm adjustments")

lt.hr.t10per <- ds(LTExercise,key="hr",adjustment=NULL,truncation="10%",convert_units=conversion.factor_1)
summary(lt.hr.t10per)
plot(lt.hr.t10per,nc=30,main="Hazard rate, no adjustments")

lt.hr.poly.t10per <- ds(LTExercise,key="hr",adjustment="poly",truncation="10%",convert_units=conversion.factor_1)
summary(lt.hr.poly.t10per)
plot(lt.hr.poly.t10per,nc=30,main="Hazard rate, poly adjustments")

#compare the bias and precision (true density = 79.8 animals per km2)
#look for which models have similar AICs and average detectability
knitr::kable(summarize_ds_models(lt.uf,lt.uf.cos,lt.hn,lt.hn.cos,lt.hn.herm,lt.hr,lt.hr.poly,output="plain"),caption="Model results for line transect data set.",digits=3)
summary(lt.uf.cos)
plot(lt.uf.cos,nc=30,main="Uniform, cosine adjustments")
#lt.uf.cos with 2 cosine adjustment has the lowest AIC, density = 81.7 [highest precision but still has large gap in data]

knitr::kable(summarize_ds_models(lt.uf.t20m,lt.uf.cos.t20m,lt.hn.t20m,lt.hn.cos.t20m,lt.hn.herm.t20m,lt.hr.t20m,lt.hr.poly.t20m,output="plain"),caption="Model results for line transect data set.",digits=3)
summary(lt.uf.cos.t20m)
plot(lt.uf.cos.t20m,nc=30,main="Uniform, cosine adjustments")
#lt.uf.cos.t20m with 1 cosine adjustment has the lowest AIC, density = 86.4 [next highest precision but looks overdispersed]

knitr::kable(summarize_ds_models(lt.uf.t10per,lt.uf.cos.t10per,lt.hn.t10per,lt.hn.cos.t10per,lt.hn.herm.t10per,lt.hr.t10per,lt.hr.poly.t10per,output="plain"),caption="Model results for line transect data set.",digits=3)
summary(lt.uf.t10per)
plot(lt.uf.t10per,nc=30,main="Uniform, no adjustments") 
#lt.hn.t10per with no adjustment terms has the lowest AIC, density = 87.9 [lowest precision and shape doesn't fit the density function]

gof_ds(lt.uf.cos) #good fit
gof_ds(lt.uf.cos.t20m) #good fit - winner? half-normal with cosine adjustments is the true model from which the data were simulated, with similar AICs and average detectability
gof_ds(lt.uf.t10per) # bad fit

#Fitting models to real data----

data(capercaillie) #a species of large grouse in Scotland
head(capercaillie)
summary(capercaillie$distance) #max 80 m

conversion.factor_2 <- convert_units("meter","kilometer","hectare")

caper.hn <- ds(capercaillie,key="hn",adjustment=NULL,convert_units=conversion.factor_2)
summary(caper.hn)
plot(caper.hn,nc=80) #spiked and heaped (rounded at 30, 40, 50, 60, 70 m)

caper.hn.t59m <- ds(capercaillie,key="hn",adjustment=NULL,truncation=59,convert_units=conversion.factor_2)
summary(caper.hn.t59m)
plot(caper.hn.t59m,nc=80) #spiked and heaped (rounded at 30, 40, 50 m)
#went with this one to cut-off the last heap - but solution said this wasn't needed because there wasn't a long tail

caper.hn.t5per <- ds(capercaillie,key="hn",adjustment=NULL,truncation="5%",convert_units=conversion.factor_2)
summary(caper.hn.t5per)
plot(caper.hn.t5per,nc=80) #spiked and heaped (rounded at 30, 40, 50, 60 m)

#Truncate at 59 m
caper.uf.t59m <- ds(capercaillie,key="unif",adjustment=NULL,truncation=59,convert_units=conversion.factor_2)
summary(caper.uf.t59m)
plot(caper.uf.t59m,nc=80,main="Uniform, no adjustments")

caper.uf.cos.t59m <- ds(capercaillie,key="unif",adjustment="cos",truncation=59,convert_units=conversion.factor_2)
summary(caper.uf.cos.t59m)
plot(caper.uf.cos.t59m,nc=80,main="Uniform, cosine adjustments")

caper.hn.cos.t59m <- ds(capercaillie,key="hn",adjustment="cos",truncation=59,convert_units=conversion.factor_2)
summary(caper.hn.cos.t59m)
plot(caper.hn.cos.t59m,nc=80,main="Half normal, cosine adjustments")

caper.hn.herm.t59m <- ds(capercaillie,key="hn",adjustment="herm",truncation=59,convert_units=conversion.factor_2)
summary(caper.hn.herm.t59m)
plot(caper.hn.herm.t59m,nc=80,main="Half normal, herm adjustments")

caper.hr.t59m <- ds(capercaillie,key="hr",adjustment=NULL,truncation=59,convert_units=conversion.factor_2)
summary(caper.hr.t59m)
plot(caper.hr.t59m,nc=80,main="Hazard rate, no adjustments")

caper.hr.poly.t59m <- ds(capercaillie,key="hr",adjustment="poly",truncation=59,convert_units=conversion.factor_2)
summary(caper.hr.poly.t59m)
plot(caper.hr.poly.t59m,nc=80,main="Hazard rate, poly adjustments")

knitr::kable(summarize_ds_models(caper.uf.t59m,caper.uf.cos.t59m,caper.hn.t59m,caper.hn.cos.t59m,caper.hn.herm.t59m,caper.hr.t59m,caper.hr.poly.t59m,output="plain"),caption="Model results for capercaillie data set.",digits=3)
summary(caper.hn.t59m)
plot(caper.hn.t59m,nc=80)
x <- gof_ds(caper.hn.t59m)
text(.5,.1, paste("P-value=",round(x$dsgof$CvM$p,3)))
#caper.hn.t59m has the lowest AIC and C-vM p>0.05, density = 0.05 birds per hectare

#Converting exact distances to binned distances
bins <- c(0, seq(from=7.5, to=67.5, by=10), 80) #note that the bins do not need to be equal
caper.bin <- ds(capercaillie,key="hn",cutpoints=bins,convert_units=conversion.factor_2)
summary(caper.bin)
plot(caper.bin)
gof_ds(caper.bin)

#Demonstration of two stages of model selection
data("ETP_Dolphin") #Eastern tropical Pacific dolphin data
bino <- subset(ETP_Dolphin,Search.method<3)

#first round = how many adjustment terms returns the lowest relative AIC for each key function?
hn.cos <- ds(bino,key="hn",adjustment="cos")
uf.cos <- ds(bino,key="unif",adj="cos")
hr.cos <- ds(bino,key="hr",adj="cos")

#second round = for each key function and adjustment terms selected above, what are their absolute goodness-of-fits?
summarize_ds_models(hn.cos,uf.cos,hr.cos)
#hr.cos has the lowest AIC
summary(hr.cos)
plot(hr.cos)
#but is it overfitting the spike?
gof_ds(hr.cos)
gof_ds(hn.cos) #second-lowest AIC 
gof_ds(uf.cos) #highest AIC
#Q-Q plots looks the same so it's ok to go with any model in this case
