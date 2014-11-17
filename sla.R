
locations <- c("IE.Dublin", "SG.Singapore",
               "TW.Changhua.County", "IN.Indianapolis")

# price/hour (USD) for a c3.large instance, see http://aws.amazon.com/ec2/pricing/
vm_baseprice <- 0.105

prices <- read.csv("prices.csv", header=TRUE)
# the slicing is a kludge to remove additional temperature entries
temperatures <- read.csv("temperatures.csv", header=TRUE)[1:(dim(prices)[1]), ]

sd_dc <- vector(length=length(locations))
names(sd_dc) <- locations

for(location in locations) {
    el_price = prices[, location]
    temp = temperatures[, location]

    cop <- 1.2 + 0.128*((temp+9)^0.5)
    adjusted_cost <- cop*el_price
    sd_dc[location] <- sd(adjusted_cost, na.rm=TRUE)
}

#
# cloud management aspects
#

costs <- function(en_saving){
	vm_cost <- (1 - en_saving) * vm_baseprice
	
	return(vm_cost)
}

availability <- function(en_saving){
	overhead_ratio <- min(sd_dc)/max(sd_dc)
	availability <- 1 - en_saving * overhead_ratio

	return(availability)
}

# parameter of group 4
max_en_savings <- 0.7



rage_i<-seq(0, 0.7, by=0.01);
rage_costs<-vector(length=length(rage_i))
rage_avl<-vector(length=length(rage_i))

for (i in rage_i){
	rage_costs[i*100-1] <- costs(i)
	rage_avl[i*100-1] <- availability(i)

	cat(i, " ", rage_costs[i*100-1], " ~", rage_avl[i*100-1], "\n")
}

library(rgl)

plot3d(rage_i, rage_costs, rage_avl, col="red", size=3)

Sys.sleep(10)