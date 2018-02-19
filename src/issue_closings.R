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
