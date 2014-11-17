
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
range_en_savings<-seq(0, 0.7, by=0.01)
range_costs<-vector(length=length(range_en_savings))
range_availability<-vector(length=length(range_en_savings))

j = 1
for (i in range_en_savings) {
	range_costs[j] <- costs(i)
	range_availability[j] <- availability(i)

	cat(i, " ", range_costs[j], " ~", range_availability[j], "\n")
	j<-j+1
}
