source("include.R")

if(!exists("df.ts.prtests")) {
df.ts.prtests = join(ts.prtests, nearby_classes, by = "slug")

N = nrow(df.ts.prtests) / 19
df.ts.prtests$time = rep(seq(1, 19), N)
df.ts.prtests$time_after = rep(c(rep(0, 10), seq(1, 9)), N)
df.ts.prtests$intervention = factor(rep( c(rep(0, 10), rep(1, 9)), N))
df.ts.prtests$f_intervention = as.factor(df.ts.prtests$intervention)
df.ts.prtests$month_idx = rep(seq(-9, 9, 1), N)

df.ts.prtests$hasCIBadge = factor(df.ts.prtests$hasQA_ci)
df.ts.prtests$hasCovBadge = factor(df.ts.prtests$hasQA_cov)
df.ts.prtests$hasOtherBadge = factor(df.ts.prtests$hasQA_other | df.ts.prtests$hasInfo | df.ts.prtests$hasPopularity | df.ts.prtests$hasSupport | df.ts.prtests$hasDepmgr)


require(sqldf)
if("RMySQL" %in% (.packages()) ) {
	detach("package:RMySQL", unload=TRUE)
}
pof = sqldf("select slug, count(proportion >= 0) as 'nonNA' from 'df.ts.prtests' group by slug;")

df.ts.prtests = subset(df.ts.prtests, slug %in% subset(pof, nonNA >= 16)$slug)
}

mod.ts.prtests = glmer(proportion ~ 
	log(contributors+1) +
	log(revisions+1) +
	log(stars+1)+
	log(downloads+1) +
	time_after +
	intervention +
	time_after * hasCovBadge * hasCIBadge +
	intervention * hasCovBadge * hasCIBadge +
	(1+f_intervention|slug),
	data=subset(df.ts.prtests, month_idx!=0),
	family="binomial",
	glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1e5)),
	weights=prs)
