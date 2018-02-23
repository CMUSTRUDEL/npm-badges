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


# Beanplots

# df = subset(td.test, test_bytes < 10000000 & test_bytes > 100)
df = subset(df.tests, test_bytes < 10000000)
bp = data.frame(resp=df$test_bytes,
                has_badge=df$hasQABadge=="1",
                group=rep("QA",nrow(df)))
bp = rbind(bp, data.frame(resp=df$test_bytes,
                          has_badge=(df$hasInfoBadge=="1"),
                          group=rep("Info",nrow(df))))

pdf(file="../plots/test_bytes.pdf", width=2.5, height=2.5)
par(mar=c(4, 4, 1, 0))
beanplot((resp+1) ~ has_badge * group, data=subset(bp, resp<=10^6), ll = 0.04,
         # main = "Test suite size", 
         ymax=c(0,10^4),
         side = "both", xlab="", ylab="Test Folder (Bytes)",
         col = list("lightblue", c("purple", "black")), 
         overallline="median", beanlines="median", boxwex=0.9,
         axes=F, bw="nrd", what=c(0,1,1,0), cutmin=0, cutmax=10^3, log="y")
axis(1, at=1:length(unique(bp$group)),  labels=c("QA", "Info"))
yrange = 0:10^5 #ceiling(log10(max(df$downloads)))+1
axis(2, at=sapply(yrange, function(i) 10^(2*i)), 
     labels=sapply(yrange, function(i) as.expression(bquote(10^ .((2*i))))))
# legend("topright", fill = c("lightblue", "purple"), inset = .01,
#        legend = c("FALSE", "TRUE"), box.lty=0, box.col = "white", 
#        bg = "white", horiz=T, title="Has badge:") 
# legend("topright", inset = .01, fill = c("lightblue", "purple"),
#        legend = c("Has badge: FALSE", "Has badge: TRUE"), box.lty=0, box.col = "white",
#        bg = "white", horiz=T) #title="Has badge:",
mtext(c(paste("(",round(cohen_func(df.tests, "hasQABadge", "test_bytes")$estimate,2),")", sep=""),
        paste("(",round(cohen_func(df.tests, "hasInfoBadge", "test_bytes")$estimate,2),")", sep="")),
      side=1,line=2,at=1:5)
dev.off()

