source("include.R")

if(!exists("df.issues")) {
df.issues = subset(issue_closings, stars < 10000 & contributors < 400 & num_issues < 20000 & revisions < 1000)

if("RMySQL" %in% (.packages()) ) {
	detach("package:RMySQL", unload=TRUE)
}

# Aggregate average issue closing times for each project
# 'lat' refers to the issue closing time (latency)
# 'num' is the number of issues aggregated for a given project

df.issues = sqldf("select slug, avg(lat) as 'avgLat', contributors, age, downloads, revisions, num, hasQABadge, hasPopularityBadge, hasDepMgrBadge, hasSupportBadge, hasInfoBadge from 'df.issues' group by slug;")
}


mod.issues.base = lm(log(avgLat+1) ~
            log(contributors+1) +
            log(age + 1) +
            log(downloads + 1) + 
            log(revisions + 1) +    
            log(num+1),
          data=subset(df.issues, num > 1))


df.issues$hasOther = factor(df.issues$hasQABadge | df.issues$hasPopularityBadge | df.issues$hasDepMgrBadge)
df.issues$hasSupportBadge = factor(df.issues$hasSupportBadge)
df.issues$hasInfoBadge = factor(df.issues$hasInfoBadge)

mod.issues.full = lm(log(avgLat+1) ~
            log(contributors+1) +
            log(age + 1) +
            log(downloads + 1) + 
            log(revisions + 1) +    
            log(num+1) +
            hasSupportBadge +
            hasInfoBadge +
            hasOther,
          data=subset(df.issues, num > 1))


# Beanplots

# FIXME not sure if we have all the data we need for this

# pdf(file="../plots/iss_lat.pdf", width=1.75, height=2.5)
# par(mar=c(4, 4, 1, 0))
# beanplot((lat+1) ~ hasBadge, data=subset(resMore,lat>0), ll = 0.04,
#          #main = "Issue latency", 
#          ymax=c(0,10^4),
#          side = "both", xlab="", ylab="Time (hours)",
#          col = list("lightblue", c("purple", "black")), 
#          overallline="median", beanlines="median", boxwex=0.9,
#          axes=F, bw="nrd", what=c(0,1,1,0), cutmin=0, cutmax=10^3, log="y")
# axis(1, at=1:1,  labels=c("Support"))
# yrange = 0:10^5 #ceiling(log10(max(df$downloads)))+1
# axis(2, at=sapply(yrange, function(i) 10^i), 
#      labels=sapply(yrange, function(i) as.expression(bquote(10^ .(i)))))
# # legend("topright", inset = .01, fill = c("lightblue", "purple"),
# #        legend = c("Has badge: FALSE", "Has badge: TRUE"), box.lty=0, box.col = "white",
# #        bg = "white", horiz=T) #title="Has badge:",
# mtext(c("(-0.07)"),side=1,line=2,at=1:1)
# dev.off()



