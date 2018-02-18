source("include.R")

if(!exists("df.prprops")) {
df.prprops = merge(pr_tests, package_info, by.x="slug", by.y="slug")

df.prprops$hasOtherBadge = factor(df.prprops$hasSupportBadge | df.prprops$hasDepMgrBadge | df.prprops$hasInfoBadge | df.prprops$hasPopularityBadge)
df.prprops$hasCIBadge = factor(df.prprops$hasCIBadge)
df.prprops$hasCovBadge = factor(df.prprops$hasCovBadge)
}

df.prprops = subset(df.prprops, revisions < 1000 &
	stars <  40000 &
	contributors < 100 &
	age < 70)

mod.prs.base = glm(proportion ~ 
	log(contributors+1) +
	log(revisions+1) +
	log(stars+1),
	data=df.prprops,
	family="binomial",
	weights=num)

mod.prs.full = glm(proportion ~ 
	log(contributors+1) +
	log(revisions+1) +
	log(stars+1)+
	hasCovBadge * hasCIBadge +
	hasOtherBadge,
	data=df.prprops,
	family="binomial",
	weights=num)
