source("include.R")

#fixme
if(!exists("df.ts.tests")) {
df.ts.tests = join(ts.tests, package_info, by="slug")

N = nrow(df.ts.tests)/19
df.ts.tests$month = rep(seq(-9, 9, 1), N)
df.ts.tests$time = rep(seq(1, 19, 1), N)
df.ts.tests$time_after_intervention = rep(c(rep(0, 10), seq(1, 9)), N)
df.ts.tests$intervention = as.factor(rep(c(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), c(1, 1, 1, 1, 1, 1, 1, 1, 1)), N))

if("RMySQL" %in% (.packages()) ) {
	detach("package:RMySQL", unload=TRUE)
}
num_zero = sqldf("select slug, count(*) as non_zeros from 'df.ts.tests' where testBytes>0 group by slug")

cov_data = subset(df.ts.tests, (slug %in% subset(num_zero, non_zeros>=19)$slug) &
	((tool=="coveralls" | tool=="codecov" | tool=="codeclimate") | hasCovBadge==1))

ci_data = subset(df.ts.tests, (slug %in% subset(num_zero, non_zeros>=19)$slug) &
	((tool=="travis" | tool=="circle") | hasCIBadge==1))

df.ts.tests = subset(df.ts.tests, slug %in% unique(ci_data$slug) |
	slug %in% unique(cov_data$slug) )

df.ts.tests = join(df.ts.tests, nearby_classes, by="slug")
df.ts.tests$hasInfo = factor(df.ts.tests$hasInfo)
df.ts.tests$hasQA = factor(df.ts.tests$hasQA_ci | df.ts.tests$hasQA_cov | df.ts.tests$hasQA_other)
df.ts.tests$hasOther = factor(df.ts.tests$hasDepmgr | df.ts.tests$hasPopularity | df.ts.tests$hasSupport)
}

mod.ts.tests = lme4::lmer(log(testBytes+1) ~
             log(projBytes) +
             # log(commits+1) +
             log(stars+1) +
             log(revisions+1) +
             log(downloads+1) +
             time +
             intervention +
             time_after_intervention +
             intervention * hasInfo +
             intervention * hasQA +
             intervention * hasOther +
             (1 + time_after_intervention | slug),
           data=subset(df.ts.tests, time!=10))
