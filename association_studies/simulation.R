## What is the impact of not adjusting for a (highly/weakly) predictive covariate in a regression model?
## What is the impact of using unweighted regression with heteroskedastic data?

simulateRegressionData <- function(numPredictors, numObs = 1000) {
	numDatasets <- 1000
	coeffs <- runif(numPredictors+1, 0, 5)
	datasets <- lapply(seq_len(numDatasets), function(i) {
		predictors <- do.call(cbind, lapply(seq_len(numPredictors), function(j) {
			rt(numObs, df = 1, ncp = 10)
		}))
		outcome <- rowSums(sweep(predictors, 1, tail(coeffs, -1), FUN = "*")) + coeffs[1] + rnorm(numObs, 0, 3)
		df <- as.data.frame(cbind(outcome, predictors))
		colnames(df) <- c("y", paste0("x", seq_len(numPredictors)))
		return(df)
	})
	attr(datasets, "coeffs") <- coeffs
	return(datasets)
}

fitModels <- function(datasets, dropLowest = 0, dropHighest = 0) {
	coeffs <- tail(attr(datasets, "coeffs"), -1)
	numCoeffs <- length(coeffs)
	if (dropLowest + dropHighest >= numCoeffs) {
		stop("Need to keep at least one predictor")
	}
	if (dropLowest > 0) {
		whichDropLow <- match(1:dropLowest, order(coeffs))
	} else {
		whichDropLow <- c()
	}
	if (dropHighest > 0) {
		whichDropHigh <- match((numCoeffs-dropHighest+1):numCoeffs, order(coeffs))
	} else {
		whichDropHigh <- c()
	}
	dropCols <- c(whichDropLow, whichDropHigh)
	keepCols <- c(setdiff(seq_len(numCoeffs), dropCols)+1, 1)
	fits <- lapply(seq_len(datasets), function(i) {
		df <- datasets[[i]]
		df <- df[,keepcols]
		lmfit <- lm(y ~ ., data = df)
		return(lmfit)
	})
	return(fits)
}