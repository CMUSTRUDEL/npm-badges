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


# Beanplots

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

pdf(file="../plots/popularity_monthly.pdf", width=4, height=2.5)
par(mar=c(4, 4, 1, 0))
bp_plots(subset(df.downloads, downloads>0 & downloads<=10^6), "Popularity", "topright")
mtext(c(paste("(",round(cohen_func(df.downloads, "hasQABadge", "downloads")$estimate,2),")", sep=""),
        paste("(",round(cohen_func(df.downloads, "hasPopularityBadge", "downloads")$estimate,2),")", sep=""),
        paste("(",round(cohen_func(df.downloads, "hasInfoBadge", "downloads")$estimate,2),")", sep="")),
      side=1,line=2,at=1:5)
dev.off()


# Nonlinearity in num badges

toolvalues <- seq(0, 12, 1)
s = subset(df.downloads, isPopular==TRUE)
prd_base <- data.frame(num_badges = toolvalues, 
                       num_badges_sq = toolvalues^2,
                       age=mean(s$age),
                       dependents=mean(s$dependents),
                       contributors=mean(s$contributors),
                       num_maintainers=mean(s$num_maintainers),
                       size=mean(s$size),
                       revisions=mean(s$revisions),
                       stars=mean(s$stars),
                       num_issues=mean(s$num_issues),
                       readme_size=mean(s$readme_size),
                       hasPopularityBadge=T, hasCIBadge=T, hasCovBadge=T)

prdT = prd_base
prdT$isPopular = "TRUE"
errT <- predict(mod.num.badges, newdata = prdT, type="response", se.fit = TRUE)
prdT$lci <- errT$fit - 1.96 * errT$se.fit
prdT$downloads <- errT$fit
prdT$uci <- errT$fit + 1.96 * errT$se.fit

library(RColorBrewer)
cbbPalette <- brewer.pal(9, "Set1")
# c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(prdT, aes(num_badges)) +
  theme_bw() +
  theme(legend.position = c(0.4, 0.2), 
        legend.direction="horizontal",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle("Badge Overload Effects") +
  ylab("Downloads") + xlab("Number of distinct badges") +
  # geom_smooth(data=prdF, aes(y = downloads, ymin = lci, ymax = uci, color="False"), stat = "identity") +
  geom_smooth(data=prdT, aes(y = downloads, ymin = lci, ymax = uci, color="True"), stat = "identity") +
  # scale_y_continuous(limits = c(0, 15000)) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  scale_color_manual(name="isPopular",
                     values=c(False=cbbPalette[5], True=cbbPalette[2]))

ggsave("../plots/nonlinPop.pdf", width = 3, height = 2.7)


