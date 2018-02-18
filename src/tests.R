source("include.R")

if(!exists("df.tests")) {
df.tests = merge(tests, package_info, by.x="slug", by.y="slug")
df.tests = subset(df.tests, !is.na(size))


df.tests$hasCIBadge = as.factor(df.tests$hasCIBadge)
df.tests$hasCovBadge = as.factor(df.tests$hasCovBadge)
df.tests$hasDepMgrBadge = as.factor(df.tests$hasDepMgrBadge)
df.tests$hasPopularityBadge = as.factor(df.tests$hasPopularityBadge)
df.tests$hasInfoBadge = as.factor(df.tests$hasInfoBadge)
df.tests$hasSupportBadge = as.factor(df.tests$hasSupportBadge)

df.tests = subset(df.tests, test_bytes < exp(19) &
                !is.na(downloads) &
                stars < exp(10) &
                revisions <= exp(7) &
                !is.na(size) &
                size > 0 &
                test_bytes < proj_bytes &
                !is.na(commits) &
                !is.na(contributors) &
                dependents <= exp(6) & # long tail
                dependencies <= exp(5) &
                num_issues <= exp(8) ) # long tail

df.tests$hasExtraBadge = as.factor(df.tests$hasDepMgrBadge=="1" | df.tests$hasPopularityBadge=="1" | df.tests$hasSupportBadge=="1")
}

mod.test.bytes.base = glm.nb(test_bytes ~ log(proj_bytes+1) +
	log(stars+1) +
	log(dependents+1) + 
	log(revisions+1) +
	log(age+1),
	data=subset(df.tests, test_bytes > 0))

mod.test.bytes.full = glm.nb(test_bytes ~ log(proj_bytes+1)  +
	log(stars+1) +
	log(dependents+1) + 
	log(revisions+1) +
	log(age+1) +
	hasQABadge * hasInfoBadge +
	hasExtraBadge,
	data=subset(df.tests, test_bytes > 0))


mod.test.logit.base = glm((test_bytes>0) ~  log(age+1) +
	log(stars+1) +
	log(dependents+1) + 
	log(revisions+1) +
	log(proj_bytes+1),
	data=df.tests,
	family="binomial")


mod.test.logit.full = glm((test_bytes>0) ~  log(age+1) +
	log(stars+1) +
	log(dependents+1) + 
	log(revisions+1) +
	log(proj_bytes+1) +
	hasQABadge *
	hasInfoBadge +
	hasExtraBadge,
	data=df.tests,
	family="binomial")
