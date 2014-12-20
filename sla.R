
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
    cat("sla: ", sla, " (actual: ", range_availability[i], "), cost: ", range_costs[i], ", savings: ", range_en_savings[i], "\n")
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
normalize_availabilities <- function(availabilities){
  return ((availabilities - min(availabilities) ) / (max(availabilities) - min(availabilities)) )
}

##
# prospect based satisfaction function
##
pbsf <- function(n, w=0.5) {
  #assert 0 <= n <= 1
  stopifnot(n >= 0)
  stopifnot(n <= 1)
  
  if (n <= 0.5){
    return(-0.5*(-2*n + 1) ^ (1-w) + 0.5)
  } 
  
  return(0.5*(2 * n - 1) ^ (1-w) + 0.5)
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
user_scores <- lapply(normalize_availabilities(aval_skype), pbsf);
plot(aval_skype, user_scores)

