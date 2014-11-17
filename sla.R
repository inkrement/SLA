
locations <- c("IE.Dublin", "SG.Singapore",
               "TW.Changhua.County", "IN.Indianapolis")

# price/hour (USD) for a c3.large instance, see http://aws.amazon.com/ec2/pricing/
vm_baseprice <- 0.105

prices <- read.csv("prices.csv", header=TRUE)
# the slicing is a kludge to remove additional temperature entries
temperatures <- read.csv("temperatures.csv", header=TRUE)[1:(dim(prices)[1]), ]

sd_dc <- vector(length=length(locations))
names(sd_dc) <- locations

cop_t <- function(temperature){
	return(1.2 + 0.128*((temperature+9)^0.5))
}

for(location in locations) {
    el_price = prices[, location]
    temp = temperatures[, location]

    cop <- ifelse(temp<-9, cop_t(temp), cop_t(-9))
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



range_en_savings<-seq(0, 0.7, by=0.01);
range_costs<-vector(length=length(range_en_savings))
range_avl<-vector(length=length(range_en_savings))

for (i in range_en_savings){
	range_costs[i*100] <- costs(i)
	range_avl[i*100] <- availability(i)

	cat(i, " ", range_costs[i*100], " ~", range_avl[i*100], "\n")
}