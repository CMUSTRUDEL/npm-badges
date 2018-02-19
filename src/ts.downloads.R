source("include.R")

if(!exists("df.ts.downloads")) {
npm_inflation$month = npm_inflation$date
df.ts.downloads = join(ts.downloads, npm_inflation, by="month")
df.ts.downloads = join(df.ts.downloads, package_info, by="name")
df.ts.downloads = join(df.ts.downloads, nearby_classes, by="name")
df.ts.downloads = join(df.ts.downloads, readme_sizes, by="slug")

N = nrow(df.ts.downloads) / 19
df.ts.downloads$time = rep(seq(1, 19), N)
df.ts.downloads$downloads_adj = df.ts.downloads$downloads * df.ts.downloads$growth
df.ts.downloads$time_after = rep(c(rep(0, 10), seq(1, 9)), N)
df.ts.downloads$intervention = factor(rep( c(rep(0, 10), rep(1, 9)), N))

df.ts.downloads$hasOtherBadge = factor(df.ts.downloads$hasDepmgr | df.ts.downloads$hasSupport)
df.ts.downloads$hasQA = factor(df.ts.downloads$hasQA_ci | df.ts.downloads$hasQA_cov | df.ts.downloads$hasQA_other)
df.ts.downloads$hasInfo = factor(df.ts.downloads$hasInfo)
df.ts.downloads$hasPopularity = factor(df.ts.downloads$hasPopularity)
}

mod.ts.downloads = lme4::lmer(log(downloads_adj+1) ~
	time +
	time_after +
	intervention +
	log(stars+1) +
	#log(control) +
	log(revisions+1) +
	log(dependents+1) +
	log(dependencies+1) +
	log(readme_size+1) +
	log(ght_age+1) +
	time_after * hasQA +
	time_after * hasInfo  +
	time_after * hasPopularity +
	time_after * hasOtherBadge +
	(1+intervention|name),
	data=subset(df.ts.downloads, time != 10))
