vif.mer <- function (fit) {
	## adapted from rms::vif

	v <- vcov(fit)
	nam <- names(fixef(fit))

	## exclude intercepts
	ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
	if (ns > 0) {
	v <- v[-(1:ns), -(1:ns), drop = FALSE]
	nam <- nam[-(1:ns)]
	}

	d <- diag(v)^0.5
	v <- diag(solve(v/(d %o% d)))
	names(v) <- nam
	v
}


pseudo_r2 = function(mod) {
	format(1 - (mod$deviance/mod$null.deviance), digits=3)
}

print_model = function(mod){
	print(">>> VIF:")
	print(vif(mod))
	print(">>> Pseudo R^2:")
	print(1 - (mod$deviance/mod$null.deviance))
	print(">>> Summary")
	print(summary(mod))
	print(">>> ANOVA")
	print(Anova(mod, type=2))
}

print_model_lm = function(mod) {
	cat("##### VIF\n")
	print(vif(mod))
	cat("##### Adjusted $R^2$")
	summ = summary(mod)
	print(summ$adj.r.squared)
	cat("##### Summary")
	print(summ)
	print("##### ANOVA")
	print(Anova(mod, type=2))
}

print_model_mer = function(mod){
	print(">>> VIF:");
	print(vif.mer(mod))
	print(">>>>> Pseudo R^2:")
	print(r.squaredGLMM(mod))
	print(">>> Summary")
	print(summary(mod))
	print(">>> ANOVA")
	print(Anova(mod, type=2))
}

print_model_mer_alt = function(mod){
	print(">>> VIF:");
	print(vif.mer(mod))
	print(">>> Summary")
	print(summary(mod))
	print(">>> ANOVA")
	print(Anova(mod, type=2))
}
