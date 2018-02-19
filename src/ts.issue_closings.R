source("include.R")

if(!exists("df.ts.issues")) {
	df.ts.issues = join(ts.issues, nearby_classes, by = "slug")
	df.ts.issues = join(df.ts.issues, package_info, by = "slug")
	df.ts.issues = join(df.ts.issues, time_since_update, by = "slug")

	N = nrow(df.ts.issues)/19
	df.ts.issues$time = rep(seq(1, 19), N)
	df.ts.issues$time_after = rep(c(rep(0, 10), seq(1, 9)), N)
	df.ts.issues$intervention = factor(rep( c(rep(0, 10), rep(1, 9)), N))

	df.ts.issues$hasOther = factor(df.ts.issues$hasQA_ci | df.ts.issues$hasQA_cov | df.ts.issues$hasQA_other | df.ts.issues$hasPopularity | df.ts.issues$hasDepmgr)
	df.ts.issues$hasSupport = factor(df.ts.issues$hasSupport)
	df.ts.issues$hasInfo = factor(df.ts.issues$hasInfo)
}

mod.ts.issues = lme4::lmer(log(avg+1) ~ 
	time +
	intervention +
	log(contributors+1) +
	log(age + 1) +
	log(downloads + 1) + 
	log(revisions + 1) +    
	time_after * hasSupport +
	time_after * hasInfo +
	time_after * hasOther +
	(1 + time_after|slug)
	, data=subset(df.ts.issues, time != 10))
