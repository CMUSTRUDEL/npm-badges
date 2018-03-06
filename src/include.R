if(!exists("badges_include")) {
	print("Loading packages.")
	require(lme4)
	require(lmerTest)
	require(MuMIn)
	require(car)
	require(MASS)
	require(effects)
	require(effsize)
	require(sqldf)
	require(MASS)
	require(pROC)
	require(plyr)
    require(beanplot)
    require(RColorBrewer)
    require(ggplot2)

	print("Loading data.")
	package_info = read.csv("../data/master_info.csv")
	time_since_update = read.csv("../data/master_time_since_update.csv")
	freshness = read.csv("../data/master_freshness.csv")
	nearby_classes = read.csv("../data/master_nearby.csv")
	pr_tests = read.csv("../data/master_prtests.csv")
	issue_closings = read.csv("../data/master_issue_closings.csv")
	tests = read.csv("../data/master_tests.csv")
	readme_sizes = read.csv("../data/readme_sizes.csv")

	print("Loading timeseries data.")
	ts.freshness = read.csv("../data/timeseries_freshness.csv")
	ts.prtests = read.csv("../data/timeseries_prtests.csv")
	ts.issues = read.csv("../data/timeseries_issue_closings.csv")
	ts.tests = read.csv("../data/timeseries_tests.csv")
	ts.downloads = read.csv("../data/timeseries_downloads.csv")
	npm_inflation = read.csv("../data/npm_inflation.csv")

	cohen_func = function(zz.all, badge, resp){
	  w = zz.all[zz.all[,which(names(zz.all) == badge)]=="1",]
	  wo = zz.all[zz.all[,which(names(zz.all) == badge)]=="0",]
	  cliff.delta(w[,which(names(w) == resp)], wo[,which(names(wo) == resp)])
	}
	
	# vif.mer <- function (fit) {
	#   ## adapted from rms::vif
	#   
	#   v <- vcov(fit)
	#   nam <- names(fixef(fit))
	#   
	#   ## exclude intercepts
	#   ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
	#   if (ns > 0) {
	#     v <- v[-(1:ns), -(1:ns), drop = FALSE]
	#     nam <- nam[-(1:ns)]
	#   }
	#   
	#   d <- diag(v)^0.5
	#   v <- diag(solve(v/(d %o% d)))
	#   names(v) <- nam
	#   v
	# }
	source('mer-utils.R')
	
	
	badges_include = TRUE
	print("Complete.")
}

