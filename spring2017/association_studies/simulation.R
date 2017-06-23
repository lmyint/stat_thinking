library(scales)

## What is the impact of not adjusting for a (highly/weakly) predictive covariate in a regression model?
## What is the impact of using unweighted regression with heteroskedastic data?

## numPredictors: number of predictors (not including intercept)
## numZero: number of predictors whose coefficients are zero (not intercept)
## numObs: total number of observations
simulateRegressionData <- function(numPredictors, numZero = 0, numObs = 1000, numDatasets = 1000, equalVar = TRUE) {
	datasets <- lapply(seq_len(numDatasets), function(i) {
		## Simulate coefficients
		coeffs <- runif(numPredictors+1-numZero, 0, 5)
		coeffs <- c(coeffs, rep(0, numZero))
		names(coeffs) <- c("intercept", paste0("x", seq_len(numPredictors)))
		## Make a numObs x numPredictors matrix
		predictors <- do.call(cbind, lapply(seq_len(numPredictors), function(j) {
			rt(numObs, df = 10, ncp = 10)
		}))
		## Multiply predictors matrix by coefficients
		outcome <- rowSums(sweep(predictors, 2, tail(coeffs, -1), FUN = "*")) + coeffs[1]
		if (equalVar) {
			outcome <- outcome + rnorm(numObs, 0, 5)
		} else {
			outcome <- outcome + rnorm(numObs, 0, 10*((outcome/max(outcome))+0.1)^2)
		}
		df <- as.data.frame(cbind(outcome, predictors))
		colnames(df) <- c("y", paste0("x", seq_len(numPredictors)))
		attr(df, "trueCoeffs") <- coeffs
		return(df)
	})
	attr(datasets, "equalVar") <- equalVar
	return(datasets)
}

fitModels <- function(datasets, weighted = FALSE, dropLowest = 0, dropHighest = 0) {
	numCoeffs <- ncol(datasets[[1]])-1
	if (dropLowest + dropHighest >= numCoeffs) {
		stop("Need to keep at least one predictor")
	}
	fits <- lapply(seq_along(datasets), function(i) {
		df <- datasets[[i]]
		coeffs <- attr(df, "trueCoeffs")
		if (dropLowest > 0) {
			whichDropLow <- order(coeffs)[1:dropLowest]
		} else {
			whichDropLow <- c()
		}
		if (dropHighest > 0) {
			whichDropHigh <- order(coeffs)[(numCoeffs-dropHighest+1):numCoeffs]
		} else {
			whichDropHigh <- c()
		}
		dropCols <- c(whichDropLow, whichDropHigh)
		keepCols <- c(1, setdiff(seq_len(numCoeffs), dropCols)+1)
		## Subset data
		df <- df[,keepCols]
		lmfit <- lm(y ~ ., data = df)
		if (weighted) {
			res <- residuals(lmfit)
			ycut <- as.numeric(cut(df$y, breaks = 30))
			ressd <- tapply(res, ycut, sd)
			w <- 1/ressd^2
			w <- w[ycut]
			lmfit <- lm(y ~ ., data = df, weights = w)
		}
		attr(lmfit, "trueCoeffs") <- coeffs[keepCols]
		return(lmfit)
	})
	return(fits)
}

summarizeResults <- function(fits, ...) {
	rmse <- sapply(fits, function(fit) {
		trueCoeffs <- attr(fit, "trueCoeffs")
		sqrt(mean((coefficients(fit)-trueCoeffs)^2))
	})
	plot(density(rmse, from = 0), xlab = "Square root(mean squared error)", ...)
	invisible(rmse)
}

set.seed(1)
simdata <- simulateRegressionData(numPredictors = 15, numZero = 10, numObs = 1000, numDatasets = 1000)
fits <- fitModels(simdata)
fits2 <- fitModels(simdata, dropHighest = 1)
fits3 <- fitModels(simdata, dropLowest = 1)
fits4 <- fitModels(simdata, dropLowest = 2)
plotResults(fits, main = "Drop none")
plotResults(fits2, main = "Drop highest (1)")
plotResults(fits3, main = "Drop lowest (1)")
plotResults(fits4, main = "Drop lowest (2)")

simdataHetero <- simulateRegressionData(numPredictors = 1, numZero = 0, numObs = 1000, numDatasets = 1000, equalVar = FALSE)
fitsHeteroNoWeight <- fitModels(simdataHetero)
par(mfrow = c(2,2))
plot(fitsHeteroNoWeight[[1]])
fitsHeteroWeight <- fitModels(simdataHetero, weighted = TRUE)
plot(fitsHeteroWeight[[1]])
par(mfrow = c(1,2))
accnow <- summarizeResults(fitsHeteroNoWeight)
accw <- summarizeResults(fitsHeteroWeight)
boxplot(accnow, accw, outline = FALSE)


acc <- accuracyMeasures(fits)
acc2 <- accuracyMeasures(fits2)
acc3 <- accuracyMeasures(fits3)
acc4 <- accuracyMeasures(fits4)

boxplot(acc, acc2, acc3, acc4, outline = FALSE, ylim = c(0,0.01))
boxplot(acc, acc2, acc3, acc4, outline = FALSE)

par(mfrow = c(2,5))
for (i in 10:1) {
	simdata <- simulateRegressionData(numPredictors = 15, numZero = i, numObs = 1000, numDatasets = 500)
	fits <- fitModels(simdata)
	plotResults(fits, main = paste("# zero:", i))
}

x = rnorm(1000)
y = 3 + 2*x + rnorm(1000, 0, abs(x))
df <- data.frame(y = y, x = x)
lmfit <- lm(y ~ x, data = df)
par(mfrow = c(2,2))
plot(lmfit)
summary(lmfit)
w <- 1/abs(x)
lmfitw <- lm(y ~ x, data = df, weights = w)
par(mfrow = c(2,2))
plot(lmfitw)
summary(lmfitw)


