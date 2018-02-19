source("include.R")

if(!exists("df.downloads")) {
df.downloads = join(package_info, readme_sizes, by="slug")

df.downloads$slug = as.factor(df.downloads$slug)

df.downloads$num_badges_sq = df.downloads$num_badges^2
df.downloads$readmeSize_sq = df.downloads$readme_size^2
df.downloads$hasQABadge = as.factor(df.downloads$hasQABadge)
df.downloads$hasCovBadge = as.factor(df.downloads$hasCovBadge)
df.downloads$hasCIBadge = as.factor(df.downloads$hasCIBadge)
df.downloads$hasDepMgrBadge = as.factor(df.downloads$hasDepMgrBadge)
df.downloads$hasInfoBadge = as.factor(df.downloads$hasInfoBadge)
df.downloads$hasPopularityBadge = as.factor(df.downloads$hasPopularityBadge)
df.downloads$hasSupportBadge = as.factor(df.downloads$hasSupportBadge)

df.downloads = subset(df.downloads, !is.na(size) & 
	!is.na(commits) & 
	!is.na(contributors) &
	!is.na(age) &
	!is.na(downloads) &
	!is.na(ght_age) &
	size > 0 &
	num_issues <= 10000 & # long tail
	dependents <= 10000 &
	revisions <= 1000 &
	num_badges <= 12)

quantile(df.downloads$downloads, prob = seq(0, 1, length = 11), na.rm=T)
df.downloads$isPopular = as.factor(df.downloads$downloads > 659)
}

f.downloads.base = (downloads ~ log(age+1) + 
	log(stars+1) +
	log(num_issues+1) +
	log(revisions+1) +
	log(dependents+1) + 
	log(readme_size+1))

f.downloads.full = (downloads ~ log(age+1) + 
	isPopular +
	log(stars+1) +
	log(num_issues+1) +
	log(revisions+1) +
	log(dependents+1) + 
	log(readme_size+1) +
	hasQABadge  +
	hasDepMgrBadge +
	hasPopularityBadge +
	hasInfoBadge +
	hasQABadge : isPopular +
	hasDepMgrBadge : isPopular +
	hasPopularityBadge : isPopular +
	hasInfoBadge : isPopular)

f.num.badges = (downloads ~ 
	log(age+1) + 
	log(size+1) +
	log(stars+1) +
	log(revisions+1) +
	log(dependents+1) + 
	log(readme_size+1) +
	num_badges +
	num_badges_sq)

mod.downloads.base = glm.nb(f.downloads.base, data=df.downloads, control=glm.control(maxit=100))
mod.downloads.full = glm.nb(f.downloads.full, data=df.downloads, control=glm.control(maxit=100))
mod.num.badges = glm.nb(f.num.badges, data=df.downloads)
