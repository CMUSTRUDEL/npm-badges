source("include.R")

## -- Freshness
## -- Additional Info -- ##

if(!exists("df.fresh")) {
	df.fresh = merge(freshness, package_info, by.x="name", by.y="name")
	df.fresh = merge(df.fresh, time_since_update, by.x="slug", by.y="slug")

	df.fresh$hasExtra = factor(df.fresh$hasPopularityBadge | df.fresh$hasSupportBadge | df.fresh$hasQABadge)
	df.fresh$hasDepMgrBadge = as.factor(df.fresh$hasDepMgrBadge)
	df.fresh$hasCIBadge = as.factor(df.fresh$hasCIBadge)
	df.fresh$hasInfoBadge = as.factor(df.fresh$hasInfoBadge)
	df.fresh = subset(df.fresh, dependencies<exp(4.5) & dependents<exp(5) & contributors<exp(6) & stars<exp(9) & tsu < 70 )
	df.fresh.s = df.fresh
}

f.fresh.base = ( log(freshness+1) ~ log(dependencies+1) +
	log(dependents+1) +
	log(tsu+1) +
	log(stars + 1) +
	log(contributors + 1))

f.fresh.badge = ( log(freshness+1) ~  log(dependencies+1) +
	log(dependents+1) +
	log(tsu+1) +
	log(stars + 1) +
	log(contributors + 1) +
	hasExtra + 
	hasDepMgrBadge * hasInfoBadge)

mod.fresh.base = lm(f.fresh.base, data=subset(df.fresh, freshness > 0))
mod.fresh.badges = lm(f.fresh.badge, data=subset(df.fresh, freshness > 0))


f.fresh.base.logit = ( (freshness==0) ~  log(dependencies+1) +
	log(dependents+1) +
	log(stars + 1) +
	log(contributors + 1) +
	log(tsu+1))

f.fresh.badge.logit = ( (freshness==0) ~ log(dependencies+1) +
	log(dependents+1) +
	log(stars + 1) +
	log(contributors + 1) +
	log(tsu+1) +
	hasExtra + 
	hasDepMgrBadge * hasInfoBadge )

f.security.base.logit = ( (security==0) ~  log(dependencies+1) +
	log(dependents+1) +
	log(stars + 1) +
	log(contributors + 1) +
	log(tsu+1))

f.security.badge.logit = ( (security==0) ~ log(dependencies+1) +
	log(dependents+1) +
	log(stars + 1) +
	log(contributors + 1) +
	log(tsu+1) +
	hasExtra + 
	hasDepMgrBadge * hasInfoBadge )

mod.fresh.base.hurdle = glm(f.fresh.base.logit, data=df.fresh.s, family="binomial")
mod.fresh.badge.hurdle = glm(f.fresh.badge.logit, data=df.fresh.s, family="binomial")

df.security.s = subset(df.fresh, !is.na(security))

mod.security.base.hurdle = glm(f.security.base.logit, data=df.security.s, family="binomial")
mod.security.badge.hurdle = glm(f.security.badge.logit, data=df.security.s, family="binomial")

if(!exists("delong.roc")) {
	delong.roc = roc.test(mod.fresh.badge.hurdle$y ~ fitted(mod.fresh.badge.hurdle) + fitted(mod.fresh.base.hurdle))
}
