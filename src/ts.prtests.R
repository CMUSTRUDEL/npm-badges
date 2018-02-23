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

## Diagnostics

# vif.mer(mod.ts.prtests)
# summary(mod.ts.prtests)
# Anova(mod.ts.prtests, type=2)
# 
# require(MuMIn)
# r.squaredGLMM(mod.ts.prtests)



# RDD boxplots

ggplot(df.ts.prtests, 
       aes(x=factor(month_idx), y=proportion)) + 
  geom_boxplot()+ #outlier.size = -10, coef = 100)  +
  # stat_summary(fun.data=MinMeanSEMMax, geom="boxplot", colour="black") + 
  # coord_fixed(ratio=1.65) + 
  geom_vline(xintercept=10, col="purple", lwd=8, alpha=0.5) + 
  labs(x = "Month index relative to badge", y = "Fraction PRs with tests") + 
  ggtitle("PR Best Practices") +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw() +
  theme(#legend.position = c(0.8, 0.1), 
    #legend.direction="horizontal",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())

ggsave("../plots/rdd-pr-tests.pdf", width = 3.5, height = 3)


ggplot(subset(df.ts.prtests, proportion<=0.5), 
       aes(x=factor(month_idx), y=proportion)) + 
  geom_boxplot(outlier.shape=NA)+ #outlier.size = -10, coef = 100)  +
  # stat_summary(fun.data=MinMeanSEMMax, geom="boxplot", colour="black") + 
  # coord_fixed(ratio=1.65) + 
  geom_vline(xintercept=10, col="purple", lwd=10, alpha=0.15) + 
  labs(x = "Month index relative to badge", y = "Fraction PRs with tests") + 
  # ggtitle("PR Best Practices") +
  scale_x_discrete(breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw() +
  theme(#legend.position = c(0.8, 0.1), 
    #legend.direction="horizontal",
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())

ggsave("../plots/rdd-pr-tests2.pdf", width = 2.5, height = 2.5)

