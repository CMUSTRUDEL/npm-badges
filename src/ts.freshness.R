source("include.R")

## -- Freshness
## -- Longitudinal Analysis -- ##

if(!exists("df.ts.fresh")) {
df.ts.fresh = join(subset(ts.freshness, num_na < 1), nearby_classes, by = "slug")
df.ts.fresh = join(df.ts.fresh, package_info, by = "slug")
df.ts.fresh = join(df.ts.fresh, time_since_update, by = "slug")


N = nrow(df.ts.fresh) / 25
df.ts.fresh$time = rep(seq(1, 25), N)
df.ts.fresh$time_after_intervention = rep(c(rep(0, 13), seq(1, 12)), N)
df.ts.fresh$intervention = rep(c(rep(0, 13), rep(1, 12)), N)
df.ts.fresh$intervention = as.factor(df.ts.fresh$intervention)

# Cut off ends for consistency with other data
df.ts.fresh = subset(df.ts.fresh, time >= 4 & time <= 22)

df.ts.fresh$hasDepmgr = factor(df.ts.fresh$hasDepmgr)
df.ts.fresh$hasInfo = factor(df.ts.fresh$hasInfo)
df.ts.fresh$hasOther = factor(df.ts.fresh$hasSupport | df.ts.fresh$hasPopularity | df.ts.fresh$hasQA_ci | df.ts.fresh$hasQA_cov | df.ts.fresh$hasQA_other)
}

if("RMySQL" %in% (.packages()) ) {
	detach("package:RMySQL", unload=TRUE)
}
numZeros = sqldf("select slug, count(*) as zeros from 'df.ts.fresh' where freshness==0 group by slug;")
numZerosSec = sqldf("select slug, count(*) as zeros from 'df.ts.fresh' where security==0 group by slug;")

df.ts.security = subset(df.ts.fresh, slug %in% subset(numZerosSec, zeros<19)$slug &
	dependencies<exp(4.5) & dependents<exp(9) & contributors<exp(6) & stars<exp(10) & tsu < 70)

df.ts.fresh = subset(df.ts.fresh, slug %in% subset(numZeros, zeros<19)$slug &
	dependencies<exp(4.5) & dependents<exp(9) & contributors<exp(6) & stars<exp(10) & tsu < 70)

mod.ts.fresh = lme4::lmer(log(freshness+1) ~
	log(dependencies+1) +
	log(dependents+1) +
	log(stars + 1) +
	log(contributors + 1) +
	log(tsu+1) +
	time +
	intervention +
	time_after_intervention * hasDepmgr * hasInfo +
	(1|slug),
	data=subset(df.ts.fresh, time != 10))

mod.ts.security = lme4::lmer(log(security+1) ~
	log(dependencies+1) +
	log(dependents+1) +
	log(stars + 1) +
	log(contributors + 1) +
	log(tsu+1) +
	time +
	time_after_intervention +
	intervention * hasDepmgr * hasInfo + 
	(1|slug),
	data=subset(df.ts.security, time != 10))
