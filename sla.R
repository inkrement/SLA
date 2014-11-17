
locations <- c("BE.St..Ghislain", "IE.Dublin", "SG.Singapore",
               "TW.Changhua.County", "IN.Indianapolis", "MI.Detroit")

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

# TODO choose an en_savings rate
# between 0 and max_en_savings
en_savings <- 0.2

vm_cost <- (1 - en_savings) * vm_baseprice
overhead_ratio <- min(sd_dc)/max(sd_dc)
availability <- 1 - en_savings * overhead_ratio

print(availability)