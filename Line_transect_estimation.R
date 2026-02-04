library(Distance)

data(ducknest)
head(ducknest)
nrow(ducknest) #number of objects (nests) detected
summary(ducknest$distance) #boxplot quantities

brks <- seq(from=0,to=2.4,by=0.3)
hist(ducknest$distance,breaks=brks,xlab="Distance (m)",main="Perpendicular distances duck nests")

conversion.factor <- convert_units("meter","kilometer","square kilometer") #converts the distance_units to the area_units
#first = distance_units (unit used for perpendicular or radial distances)
#second = effort_units (unit used for length of transects, null for point transects)
#third = area_units (unit used for the entire study area)

# Fit half-normal detection function, no adjustment terms
nest.hn <- ds(data=ducknest,key="hn",adjustment=NULL,convert_units=conversion.factor)
#hn = half-normal key function, default
#AIC is returned for each adjustment term up to order 5, and the lowest value is selected

summary(nest.hn)
#Detection function parameter intercept estimate = sigma, calculate log(sigma) by taking the exponent of this value, exp(sigma)
#Average p Estimate = P hat sub a
#when Area is specified, abundance will be estimated along with density
#Effort = total length of all transect lines
#k = number of transects
#ER = encounter rates

plot(nest.hn,nc=8,main="Half normal, no adjustments") #nc = number of columns/bins

gof_ds(nest.hn)
#Goodness Of Fit tests
#Q-Q plot (close to the diagonal line = good fit) 
#chi-squared test (p>0.05 = good fit)

#Other detection functions:
#key="unif" for uniform
#key="hr" for hazard rate
#adjustment="cos" for cosine, default, can be used with uniform or half normal key functions
#adjustment="herm" for hermite polynomial, can be used with half normal key function
#adjustment="poly" for simple polynomial, can be used with hazard rate key function

nest.uf.cos <- ds(ducknest,key="unif",adjustment="cos",convert_units=conversion.factor)
summary(nest.uf.cos)
plot(nest.uf.cos,nc=8,main="Uniform, cosine adjustments")

nest.hr.herm <- ds(ducknest,key="hr",adjustment="herm",convert_units=conversion.factor)
summary(nest.hr.herm)
plot(nest.hr.herm,nc=8,main="Hazard rate, no adjustments")

#key="hn",adjustment=NULL - AIC = 928.1338, density = 49.7, 95% CI = ucl-lcl? 11.66988 #best model due to lowest AIC and 95% CI
#key="unif",adjustment="cos" - AIC = 928.4797, density = 51.0, 95% CI = 13.08653
#key="hr",adjustment="herm" - AIC = 929.7934, density = 48.6, 95% CI = 13.02348

knitr::kable(summarize_ds_models(nest.hn,nest.uf.cos,nest.hr.herm,output="plain"),caption="Model results for ducknest data set.",digits=3)
#C-vM = Cramer-von Mises test for goodness of fit
#Average detectability = P sub a estimate

#testing the sensitivity of the model with different truncation values
trunc.experiment <- function(mydata,trunc.range=0:25,cu,type="line") { #0-25% of dataset removed
  result <- data.frame(est=numeric(),
                       lcl=numeric(),
                       ucl=numeric())
  for (i in seq_along(trunc.range)) {
    this <- paste0(i-1,"%")
    m <- ds(mydata,key = "hn",adj = NULL,convert_units=cu, 
            transect=type,truncation=this)
    result[i, ] <- m$dht$individuals$D[c(2,5,6)]
  }
  return(result)
}

duck.trunc <- trunc.experiment(mydata=ducknest,trunc.range=0:25,cu=conversion.factor,type="line")

trange <- 1:26
plot(trange-1,duck.trunc$est,type="p", 
     ylim=range(c(duck.trunc[,2],duck.trunc[,3])),pch=20,
     main="Duck nest data\ntruncation experiment",ylab="Nest density",xlab="Percent data truncated")
segments(trange[trange]-1,duck.trunc[trange,2],
         trange[trange]-1,duck.trunc[trange,3])
#shows the change in density with increasing percentages of data being removed,
#which in this case decreases after ~20% but at a small magnitude, especially in comparison to the width of the confidence intervals

#applying the script to another dataset from Practical 3 where is true density is known to be 79.8 animals per hectare/0.001 square kilometers/10,000 square meters
data("LTExercise")
sim.trunc <- trunc.experiment(mydata=LTExercise,trunc.range=0:25,cu=conversion.factor,type="line")
plot(trange-1,sim.trunc$est,type="p", 
     ylim=range(c(sim.trunc[,2],sim.trunc[,3])),pch=20,
     main="Simulated data\ntruncation experiment",ylab="Density",xlab="Percent data truncated")
segments(trange[trange]-1,sim.trunc[trange,2],
         trange[trange]-1,sim.trunc[trange,3])
abline(h=79.8,lwd=2,lty=3)
#shows the change in density with increasing percentages of data being removed is again small,
#and density estimates will not differ greatly as long as the number of animals detected is sufficient