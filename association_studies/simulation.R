library(scales)

## What is the impact of not adjusting for a (highly/weakly) predictive covariate in a regression model?
## What is the impact of using unweighted regression with heteroskedastic data?

## numPredictors: number of predictors (not including intercept)
## numZero: number of predictors whose coefficients are zero (not intercept)
## numObs: total number of observations
simulateRegressionData <- function(numPredictors, numZero = 0, numObs = 1000, numDatasets = 1000, equalVar = TRUE) {
	coeffs <- runif(numPredictors+1-numZero, 0, 5)
	coeffs <- c(coeffs, rep(0, numZero))
	datasets <- lapply(seq_len(numDatasets), function(i) {
		## Make a numObs x numPredictors matrix
		predictors <- do.call(cbind, lapply(seq_len(numPredictors), function(j) {
			rt(numObs, df = 1, ncp = 10)
		}))
		## Multiply predictors matrix by coefficients
		outcome <- rowSums(sweep(predictors, 2, tail(coeffs, -1), FUN = "*")) + coeffs[1]
		if (equalVar) {
			outcome <- outcome + rnorm(numObs, 0, 5)
		} else {
			outcome <- outcome + rnorm(numObs, 0, outcome)
		}
		df <- as.data.frame(cbind(outcome, predictors))
		colnames(df) <- c("y", paste0("x", seq_len(numPredictors)))
		return(df)
	})
	names(coeffs) <- c("(Intercept)", paste0("x", seq_len(numPredictors)))
	attr(datasets, "trueCoeffs") <- coeffs
	return(datasets)
}

fitModels <- function(datasets, dropLowest = 0, dropHighest = 0) {
	coeffs <- tail(attr(datasets, "trueCoeffs"), -1)
	numCoeffs <- length(coeffs)
	if (dropLowest + dropHighest >= numCoeffs) {
		stop("Need to keep at least one predictor")
	}
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
	print(keepCols)
	fits <- lapply(seq_along(datasets), function(i) {
		df <- datasets[[i]]
		df <- df[,keepCols]
		lmfit <- lm(y ~ ., data = df)
		return(lmfit)
	})
	attr(fits, "trueCoeffs") <- attr(datasets, "trueCoeffs")[keepCols]
	return(fits)
}

plotResults <- function(fits, ...) {
	trueCoeffs <- attr(fits, "trueCoeffs")
	coeffMat <- do.call(cbind, lapply(fits, function(fit) {
		coefficients(fit)
	}))
	par(mfrow = c(1,2))
	matplot(x = seq_len(nrow(coeffMat)), y = coeffMat, pch = 16, col = alpha("black", 0.1), xlab = "", ylab = "Coefficient estimate", xaxt = "n", ...)
	points(x = seq_len(nrow(coeffMat)), y = trueCoeffs, col = "red")
	axis(side = 1, at = seq_len(nrow(coeffMat)), labels = rownames(coeffMat))

	coeffList <- lapply(seq_len(nrow(coeffMat)), function(i) {
		coeffMat[i,]
	})
	boxplot(coeffList, outline = FALSE, xaxt = "n", ...)
	points(x = seq_len(nrow(coeffMat)), y = trueCoeffs, col = "red")
	axis(side = 1, at = seq_len(nrow(coeffMat)), labels = rownames(coeffMat))
}

accuracyMeasures <- function(fits) {
	trueCoeffs <- attr(fits, "trueCoeffs")
	medianAbsError <- sapply(fits, function(fit) {
		median(abs(coefficients(fit)-trueCoeffs))
	})
	return(medianAbsError)
}

set.seed(1)
simdata <- simulateRegressionData(numPredictors = 5, numZero = 1, numObs = 1000, numDatasets = 1000)
simdataHetero <- simulateRegressionData(numPredictors = 5, numZero = 1, numObs = 1000, numDatasets = 1000, equalVar = FALSE)
fits <- fitModels(simdata)
fitsHetero <- fitModels(simdataHetero)
fits2 <- fitModels(simdata, dropHighest = 1)
fits3 <- fitModels(simdata, dropLowest = 1)
fits4 <- fitModels(simdata, dropLowest = 2)
plotResults(fits, main = "Drop none")
plotResults(fitsHetero, main = "Drop none (hetero)", ylim = c(-5,5))
plotResults(fits2, main = "Drop highest (1)")
plotResults(fits3, main = "Drop lowest (1)")
plotResults(fits4, main = "Drop lowest (2)")
acc <- accuracyMeasures(fits)
acc2 <- accuracyMeasures(fits2)
acc3 <- accuracyMeasures(fits3)
acc4 <- accuracyMeasures(fits4)

boxplot(acc, acc2, acc3, acc4, outline = FALSE, ylim = c(0,0.01))
boxplot(acc, acc2, acc3, acc4, outline = FALSE)






