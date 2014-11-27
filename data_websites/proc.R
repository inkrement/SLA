library("reshape2")
library("ggplot2")
library("Hmisc")

websites <- read.csv("/tmp/event_trace.tab", sep="\t")

timediv <- 24*60*60     # per day

# set dimension names to website names
wnames = list()
wnondup = websites[!duplicated(websites$node_name),]
for(i in 1:nrow(wnondup)) {
    w <- wnondup[i,]
    wnl <- unlist(w)
    wnames[wnl['node_id']+1] <- paste('', unlist(w$node_name));
}

days <- array(data=0, dim=c(max(websites['event_stop_time'])/timediv+2, max(websites$node_id)+1, 2), dimnames=list(list(), wnames))

# calculate daily availability per website
for(i in seq(1, length(websites[,1]), 1)) {
    w <- unlist(websites[i,])
    begin <- floor(w['event_start_time']/timediv)
    end <- ceiling(w['event_stop_time']/timediv)-1
    duration <- w['event_stop_time'] - w['event_start_time']
    node <- w['node_id']+1
    online <- w['event_type']+1
    for(j in seq(begin,end,1)) {
        ttd <- min(w['event_stop_time'], (j+1)*timediv) - max(w['event_start_time'], j*timediv)
        days[j+1,node,online] = days[j+1,node,online] + ttd
    }
}

uptimes <- apply(days, c(1,2), function(x) x[2]/sum(x))
d <- data.frame(x = unlist(uptimes[,c(2,4,7,15,20)]), days = 1:length(uptimes[,2]))
d.melt <- melt(d, id='days')

ggplot(data=d.melt, aes(x=days,y=((1-value)*100), color=variable))+
      ylab('% downtime')+geom_line()+theme(legend.position="bottom")

means <- colMeans(uptimes,na.rm=TRUE)
means.melt <- melt(1-means[c(1,4,8,15,21,50,72,100,115)])
means.melt$name <- unlist(dimnames(means.melt)[1])
ggplot(means.melt, aes(x=name, y=value))+geom_bar(stat="identity")

means <- na.exclude(means[means>=0.5])   # remove outliers

fmeans <- c(unlist(means), c(64.50,66.61,67.31,67.47,62.92,47.58,48.95)/100,
            c(69.21992481,100,91.12625313,78.82988722,89.50109649,48.61763784,52.47493734)/100*0.998)

#groups <- split(means, cut(means, 5))
groups <- split(fmeans, cut2(fmeans, g=6)) # split into equal-sized groups
groupmeans <- lapply(groups, mean)

groupmeans.melt <- melt(groupmeans)
ggplot(data=groupmeans.melt, aes(x=L1, y=value)) + geom_bar(stat="identity")
