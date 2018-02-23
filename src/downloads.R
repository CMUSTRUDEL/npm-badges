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
	!is.na(readme_size) &
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
mod.num.badges = glm.nb(f.num.badges, data=subset(df.downloads, isPopular=="TRUE"))


bp_plots = function(zz.bot, main, legend_position){
  bp = data.frame(downloads=zz.bot$downloads, 
                  has_badge=zz.bot$hasQABadge==1,
                  group=rep("QA",nrow(zz.bot)))
  bp = rbind(bp, data.frame(downloads=zz.bot$downloads,
                            has_badge=zz.bot$hasPopularityBadge==1,
                            group=rep("Pop",nrow(zz.bot))))
  bp = rbind(bp, data.frame(downloads=zz.bot$downloads,
                            has_badge=zz.bot$hasInfoBadge==1,
                            group=rep("Info",nrow(zz.bot))))
  require(beanplot)
  beanplot(downloads ~ has_badge * group, data=bp, ll = 0.04,
           main = main,
           ymax=c(0,10^5),
           side = "both", xlab="", ylab="Downloads",
           col = list("lightblue", c("purple", "black")), 
           overallline="median", beanlines="median", boxwex=0.9,
           axes=F, bw="nrd", what=c(0,1,1,0), cutmin=0, cutmax=10^5, log="y")
  axis(1, at=1:length(unique(bp$group)),  
       labels=c("QA", "Popularity", "Info"))
  yrange = -1:10^5+1 #ceiling(log10(max(zz.bot$downloads)))+1
  axis(2, at=sapply(yrange, function(i) 10^(2*i)), 
       labels=sapply(yrange, function(i) as.expression(bquote(10^ .((2*i))))))
  legend(legend_position, 
         # inset = .005, 
         fill = c("lightblue", "purple"),
         legend = c("Badge: FALSE", "TRUE"), box.lty=0, box.col = "white", 
         bg = "white", horiz=T)#, title="Has badge:") 
}

cohen_func = function(zz.all, badge, resp){
  w = zz.all[zz.all[,which(names(zz.all) == badge)]=="1",]
  wo = zz.all[zz.all[,which(names(zz.all) == badge)]=="0",]
  cliff.delta(w[,which(names(w) == resp)], wo[,which(names(wo) == resp)])
}

pdf(file="../plots/popularity_monthly.pdf", width=4, height=2.5)
par(mar=c(4, 4, 1, 0))
bp_plots(subset(df.downloads, downloads>0 & downloads<=10^6), "Popularity", "topright")
mtext(c(paste("(",round(cohen_func(df.downloads, "hasQABadge", "downloads")$estimate,2),")", sep=""),
        paste("(",round(cohen_func(df.downloads, "hasPopularityBadge", "downloads")$estimate,2),")", sep=""),
        paste("(",round(cohen_func(df.downloads, "hasInfoBadge", "downloads")$estimate,2),")", sep="")),
      side=1,line=2,at=1:5)
dev.off()

