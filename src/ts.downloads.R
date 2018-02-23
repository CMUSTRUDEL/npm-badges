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


# RDD boxplots

keepNames = subset(df.ts.downloads, time == 19 & downloads < 1e5)$name

ggplot(subset(df.ts.downloads, name %in% keepNames & 
                downloads_adj<=10^4 & 
                downloads_adj>=10), 
       aes(x=factor(time - 10), y=downloads_adj)) + 
  geom_boxplot(outlier.shape=NA)+ #outlier.size = -10, coef = 100)  +
  # stat_summary(fun.data=MinMeanSEMMax, geom="boxplot", colour="black") + 
  # coord_fixed(ratio=1.65) + 
  geom_vline(xintercept=10, col="purple", lwd=2, alpha=0.5) + 
  scale_x_discrete(breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8)) +
  labs(x = "Month index relative to badge", y = "Downloads") + 
  # ggtitle("Download Counts") +
  scale_y_log10(breaks = c(10^1, 10^2, 10^3, 10^4), #10^5, 10^6, 10^7, 10^8),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme_bw() +
  theme(#legend.position = c(0.8, 0.1), 
    #legend.direction="horizontal",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) 

ggsave("../plots/rdd-downloads.pdf", width = 2.5, height = 2.5)

