# This file groups the start times
# of the skype session logs

values <- scan("start_times_only", what=integer())

# aggregate(values, by=list(data$group), FUN=mean)[2]

# add minute column
minutes <- vector(length=length(values))
hours <- vector(length=length(values))

j = 1
for(session in values) {
	minutes[j] = ceiling(session/60)
	hours[j] = ceiling(session/60/60)
	j <- j + 1
}

Data <- data.frame(minute=minutes, hour=hours, values=values)

#Data
aggregate(values ~ hour, data=Data, FUN=length)

#cat(values)
#hist(values, breaks=12, plot=TRUE)
