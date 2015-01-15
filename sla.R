
locations <- c("IE.Dublin", "SG.Singapore",
               "TW.Changhua.County", "IN.Indianapolis")

# price/hour (USD) for a c3.large instance, see http://aws.amazon.com/ec2/pricing/
vm_baseprice <- 0.105

# parameter of group 4
max_en_savings <- 0.7

prices <- read.csv("prices.csv", header=TRUE)
# the slicing is a kludge to remove additional temperature entries
temperatures <- read.csv("temperatures.csv", header=TRUE)[1:(dim(prices)[1]), ]

sd_dc <- vector(length=length(locations))
names(sd_dc) <- locations

cop_t <- function(temperature) {
	return(1.2 + 0.128*((temperature+9)^0.5))
}

# cloud management aspects
costs <- function(en_saving) {
	vm_cost <- (1 - en_saving) * vm_baseprice
	return(vm_cost)
}

availability <- function(en_saving) {
	overhead_ratio <- min(sd_dc)/max(sd_dc)
	availability <- 1 - en_saving * overhead_ratio
	return(availability)
}

# inverse of above
ensavings <- function(avail) {
  overhead_ratio <- min(sd_dc)/max(sd_dc)
  return((1-avail) / overhead_ratio);
}

for(location in locations) {
    el_price = prices[, location]
    temp = temperatures[, location]
    cop <- ifelse(temp<-9, cop_t(-9), cop_t(temp))
    adjusted_cost <- cop*el_price
    sd_dc[location] <- sd(adjusted_cost, na.rm=TRUE)
}

# calculate different availability values 
range_en_savings<-seq(0, 0.7, by=0.001)
range_costs<-vector(length=length(range_en_savings))
range_availability<-vector(length=length(range_en_savings))

j = 1
for (i in range_en_savings) {
	range_costs[j] <- costs(i)
	range_availability[j] <- availability(i)

	cat(i, " ", range_costs[j], " ~", range_availability[j], "\n")
	j<-j+1
}

#library(rgl)
#plot3d(range_en_savings, range_costs, range_availability, col="red", size=3)
#Sys.sleep(10)


sla_availabilities = list(0.931, 0.966, 0.981, 0.987, 0.992, 0.998)
for (sla in sla_availabilities) {
    i <- match(FALSE, range_availability>sla)-1
    #cat("sla: ", sla, " (actual: ", range_availability[i], "), cost: ", range_costs[i], ", savings: ", range_en_savings[i], "\n")
}



#######
# 
# Aufgabe 3
#
#######

# prospect based satisfaction function 

##
# norm function 
##
normalize_availability <- function(avail, minavail, maxavail) {
  if(minavail == maxavail) {
    return((avail>minavail)+0)
  } else {
    return((avail-minavail) / (maxavail-minavail))
  }
}

normalize_availabilities <- function(availabilities){
  return(normalize_availability(availabilities, min(availabilities), max(availabilities)))
}

##
# prospect based satisfaction function
##
pbsf <- function(n, w=0.5) {
  if (n <= 0.5){
    sat <- -0.5*(-2*n + 1) ^ (1-w) + 0.5
  } else {
    sat <- 0.5*(2 * n - 1) ^ (1-w) + 0.5
  }

  return(sat)
}

##
# payment willingness
##
wtp <- function(avail, baseprice) {
	return(avail * baseprice + rnorm(length(avail), sd=0.001))
}

##
# test for skype
#
# min av. 86.5 (calculated in the last exercise)
# w=0.5 (professional users)
# n=normalized availabilities
##
min_avail_skype <- 0.865
max_amazon_aval <- 0.998

aval_skype <- seq(min_avail_skype, max_amazon_aval, 0.001)
user_scores_skype <- lapply(normalize_availabilities(aval_skype), pbsf);

wtp_skype <- wtp(aval_skype, vm_baseprice)
#cat("Skype WTP:", wtp_skype)

##
#  microsoft
#
# min av. 81.24 (calculated in the last exercise)
# w=0.5 (professional users)
# n=normalized availabilities
##
min_avail_ms <- 0.8124
max_amazon_aval <- 0.998

aval_ms <- seq(min_avail_ms, max_amazon_aval, 0.001)
user_scores_ms <- lapply(normalize_availabilities(aval_ms), pbsf);

wtp_ms <- wtp(aval_ms, vm_baseprice)
#cat("Microsoft WTP:", wtp_ms)

##
#  websites
#
# min av. 60.35 (calculated in the last exercise)
# w=0.5 (professional users)
# n=normalized availabilities
##
min_avail_web <- 0.6035
max_amazon_aval <- 0.998

aval_web <- seq(min_avail_web, max_amazon_aval, 0.001)
user_scores_web <- lapply(normalize_availabilities(aval_web), pbsf);

wtp_web <- wtp(aval_web, vm_baseprice)
#cat("Websites WTP:", wtp_web)


plot(aval_web, user_scores_web, col="red", type="l", xlab="availability", ylab="user score")
lines(aval_skype, user_scores_skype, col="orange")
lines(aval_ms, user_scores_ms, col="blue")

legend(x=locator(n=1), legend=c("ms", "skype", "web"), fill=c("blue", "orange", "red"))
#locator(n=1)

plot(aval_web, wtp_web)
#locator(n=1)

#######
#
# Aufgabe 4
#
#######

# alternative utility function
alpha = 60;
beta = -0.015;
gamma = 0.99;

fi <- function(reqav, av) {
  return(gamma/(gamma+beta*exp(reqav^2*alpha*(reqav-av))))
}

# calculate utility for given availability requirements (reqavt) wrt. an SLA availability (avt)
utility_model <- function(wtp, reqavt, avt) {
  # prospect-based utility model
  #return(wtp*(unlist(lapply(reqavt, function(ra) { pbsf(normalize_availability(avt, ra, max_amazon_aval)) }))/0.5) - vm_baseprice)

  # utility function
  return(unlist(mapply(function(wp, reqav) { wp*fi(reqav, avt)-costs(ensavings(avt)) }, wtp, reqavt)))
}

# availability requirements of all users (websites, desktops, skype)
availdata <- c(0.96685080256856, 0.992825395521464, 0.988075729359699, 
  0.988783069297131, 0.992974908042774, 0.922675096162584, 0.973994960186689, 
  0.9585829637388, 0.992669445712149, 0.988393682359168, 0.989098228980047, 
  0.970510471090645, 0.989832088707599, 0.979780285565785, 0.97420158711781, 
  0.982348305799232, 0.979270192796611, 0.965823503576895, 0.955838434702475, 
  0.984487116558732, 0.934142357220784, 0.973078045462524, 0.989910314639284, 
  0.981016225036856, 0.707718889113599, 0.96258011272433, 0.991582850623135, 
  0.960646392838336, 0.985135708905142, 0.996821304625283, 0.991293069381962, 
  0.990080689397929, 0.991248189836756, 0.994527178205465, 0.975772569444444, 
  0.952592967284239, 0.966929813141927, 0.992591547641035, 0.956141410508575, 
  0.856383018343798, 0.996796559592768, 0.987384006130062, 0.982598415094992, 
  0.980817937010826, 0.974570843859803, 0.603593716233245, 0.972892148591434, 
  0.986849964335785, 0.97485836843953, 0.990485813776329, 0.98145485778633, 
  0.939699880601224, 0.984701816745656, 0.955542944517004, 0.96488850498029, 
  0.959125024673736, 0.994223494821836, 0.99592943654555, 0.988719336931718, 
  0.990602213205025, 0.99164472427229, 0.982294080217658, 0.937829464981691, 
  0.987733905999085, 0.992588859208643, 0.984669178953835, 0.919595389789054, 
  0.995282041919016, 0.988725015375154, 0.894392359385183, 0.993559375847088, 
  0.982430587740358, 0.961137883228672, 0.934639293954156, 0.956554881496037, 
  0.957653737171528, 0.909110832472288, 0.931957938153378, 0.994153446989644, 
  0.98329986831067, 0.980041114243995, 0.993146469247266, 0.99212816301514, 
  0.928584297839506, 0.988464378181499, 0.985816283728847, 0.981090843855716, 
  0.979748427477479, 0.901137436625717, 0.986493995939766, 0.992136969699333, 
  0.975128011275961, 0.933742227572459, 0.983402917996541, 0.976293256114221, 
  0.990928589745603, 0.983865265495408, 0.987105329559417, 0.98988660210756, 
  0.978748628751781, 0.983699085130255, 0.947303053853022, 0.991544617781288, 
  0.992316623005965, 0.983069509326889, 0.930573781524311, 0.988699699403195, 
  0.990381948628799, 0.957791908931253, 0.986690020867663, 0.965963138809614, 
  0.986105406354222, 0.972867835216315, 0.987837762961422, 0.986456592411306, 
  0.9854749810695, 0.980428932274696, 0.99188835101676, 0.645, 
  0.6661, 0.6731, 0.6747, 0.6292, 0.4758, 0.4895, 0.6908148496038, 
  0.998, 0.9094400062374, 0.7867222744556, 0.8932209429702, 0.4852040256432, 
  0.5236998746532)


# draw the distribution of WTP values
hist(unlist(lapply(availdata, wtp, baseprice=vm_baseprice)), xlab="WTP ($/h)", ylab="#users", main="")

maxslas <- 20       # maximum number of SLAs to try
minconvrate <- 0.95 # minimum required conversion rate

# matrix with 2 columns (avail and WTP)
mat <- matrix(c(availdata, lapply(availdata, wtp, baseprice=vm_baseprice)), ncol=2)

slausers <- matrix(nrow=maxslas, ncol=maxslas,
                   dimnames=list(
                     lapply(1:maxslas, function(x) { paste0("sla", x) }),
                     lapply(1:maxslas, function(x) { paste0(x, "SLAs") })
                   ))

slas <- matrix(-1, nrow=maxslas, ncol=maxslas,
               dimnames=list(
                 lapply(1:maxslas, function(x) { paste0("sla", x) }),
                 lapply(1:maxslas, function(x) { paste0(x, "SLAs") })
               ))

# conversion rate
convrate = vector(length=maxslas)

# try a range of SLA numbers
for (numslas in seq(1, maxslas, by=1)) {

  # build the clusters
  clusters <- kmeans(mat, numslas)
  clmembers <- clusters[["cluster"]]
  centers <- clusters[["centers"]]
  satisfied <- vector(length=length(availdata))

  clustermax <- vector(length=numslas)              # maximum value of each cluster
  numcustomers <- matrix(nrow=numslas, ncol=2)
  colnames(numcustomers) <- c("total", "positive_util")

  # calculate the maximum values and number of positive-utility members per cluster
  for (i in seq(1, numslas, by=1)) {
    avails <- availdata[clmembers == i]
    clustermax[i] <- max(avails)
    ut <- utility_model(wtp(avails, vm_baseprice), avails, clustermax[i])
    satisfied[clmembers == i] <- ut > 0
    numcustomers[i,"total"] <- length(avails)
    numcustomers[i,"positive_util"] <- length(ut[ut >= 0])
  }

  # plot clusters
  plot(mat, xlab="availability", ylab="WTP ($/h)", pch=c(4, 19)[satisfied+1], col=clmembers)

  if(numslas > 1) {
    # result of clustering is unordered -> order by increasing SLA availability
    ordering <- order(clustermax)
    clustermax <- clustermax[ordering]
    numcustomers <- numcustomers[ordering, ]

    row.names(numcustomers) <- lapply(1:numslas, function(x) { paste0("sla", x, "\n(", round(clustermax[x], digits=3), "%)") })
  }

  slausers[,numslas] <- c(numcustomers[,"positive_util"], rep.int(0, maxslas-numslas))
  slas[1:numslas,numslas] <- clustermax

  # calculate share of satisfied users
  convrate[numslas] <- sum(numcustomers[,"positive_util"])/sum(numcustomers[,"total"])

  cat(clustermax, "\n", numcustomers, "\n")
}

# optimum number of SLAs: first which exceeds 95% conversion
optslas <- match(TRUE, convrate>0.95)[1]
cat('first SLA with conversion > 95%: SLA', optslas, "\n")
bestsla <- slas[1:optslas, optslas]

barplot(slausers, ylab="#users")

barplot(slausers[1:optslas,optslas], ylab="#users", main="SLA selection")

percsavings <- vector(length=optslas)
totalsavings <- 0
totalusercount <- 0
for(i in seq(1, optslas)) {
  saved <- vm_baseprice-(costs(ensavings(bestsla[i])))
  totalusercount <- totalusercount + slausers[i,optslas]
  totalsavings <- totalsavings + saved*slausers[i,optslas]
  percsavings[i] <- (saved / vm_baseprice)

  cat('SLA', i, ', av', bestsla[i]*100 ,'%, savings:', percsavings[i]*100, '%', '\n')
}

names(percsavings) <- lapply(1:optslas, function(x) { paste0("sla", x, "\n(", round(bestsla[x], digits=3), "%)") })

barplot(percsavings*100, xlab="SLAs", ylab="% savings")

cat('total saved:', totalsavings/(totalusercount*vm_baseprice)*100, '%\n')
