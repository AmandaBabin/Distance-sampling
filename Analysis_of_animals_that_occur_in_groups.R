library(Distance)

rissogithub <- "https://raw.githubusercontent.com/distanceworkshops/async2024-2/main/09-clusters/R-prac/Risso_survey.csv"
risso <- read.csv(rissogithub)
head(risso,n=3)

aveobs.size <- round(mean(risso$size),2)
histlabel <- paste0("Observed group sizes of Risso's dolphins\n","Mean observed size=",aveobs.size)
hist(risso$size,nc=15,main=histlabel,xlab="Observed group size")
#mean observed size = 7.21 based on this simulation of real data that had a true mean group size of 6

with(risso,scatter.smooth(distance,size,pch=20,lwd=2,xlab="Detection distance",ylab="Size of detected group",main="Risso's survey\nDiagnostic plot"))
#look at the bottom right quadrant to see if there is a lack of groups detected at greater distances
#if so, then there is size bias that would lead to overestimating overall abundance
#in this example, groups are largely absent beyond 2 nm

naive.uniform <- ds(data=risso,key="unif",adjustment="cos")
naive.hn <- ds(data=risso,key="hn",adjustment = "cos") 
naive.hr <- ds(data=risso,key="hr",adjustment = NULL) # no adjustments for simplicity

knitr::kable(summarize_ds_models(naive.uniform,naive.hn,naive.hr,output="plain"),digits=3,caption="Model selection for models not considering size bias")
#p>0.05 in all cases so they all fit, and AICs are close together - instructions say to go with hazard rate even though it has the smallest detectability with the greatest variation

plot(naive.hr,nc=40,main="Hazard rate model with no adjustments")

knitr::kable(naive.hr$dht$clusters$summary)
#n = 265 detections, k = 12 transects

knitr::kable(naive.hr$dht$clusters$N)
#Estimate = 4531 clusters/groups, compared to 4333 from the real data

knitr::kable(naive.hr$dht$individuals$N)
#Estimate = 32,677 individuals, compared to 26,000 from the real data (overestimated)

knitr::kable(naive.hr$dht$Expected.S)
#Expected.S = mean observed size = 7.21, compared to 6 from the real data (overestimated)

#add size as a covariate, then the ds function uses Horwitz-Thompson-like estimators to estimate the abundance of groups, then the abundance of individuals
#the ratio of the estimate of number of groups to number of individuals is a less biased estimate of average group size

clever.hn <- ds(data=risso,key="hn",formula = ~size) 
clever.hr <- ds(data=risso,key="hr",formula = ~size)

AIC(naive.hn,naive.hr,clever.hn,clever.hr)
#AICs are lower when adding size as a covariate

knitr::kable(summarize_ds_models(clever.hn,clever.hr,output="plain"),digits=3,caption="Comparison of hazard rate and half normal models incorporating group size as covariate")
#hazard rate has the lower AIC

knitr::kable(clever.hr$dht$clusters$N)
#Estimate = 4779 clusters/groups, which is farther away from the real data than the naive model

knitr::kable(clever.hr$dht$individuals$N)
#Estimate = 27,449 individuals, which is closer to the real data than the naive model

knitr::kable(clever.hr$dht$Expected.S)
#Expected.S = 5.74, which is closer to the real data than the naive model