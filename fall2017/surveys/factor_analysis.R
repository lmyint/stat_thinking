## We will be looking at data on 240 participants' self-reported ratings of 32 personality traits

########## FACTOR ANALYSIS ##########
## Are there fewer underlying personality "pieces" that underlie these 32 traits?
pdata <- read.table("http://www.stanford.edu/class/psych253/data/personality0.txt")
head(pdata)

## Exploration of the relationship between various traits
plot(jitter(pdata$anxious), jitter(pdata$tense))
plot(jitter(pdata$laidbck), jitter(pdata$easygon))
plot(jitter(pdata$kind), jitter(pdata$lazy))
plot(jitter(pdata$hardwrk), jitter(pdata$lazy))

## Correlation matrix
cor_pdata <- cor(pdata)
### Set up a color palette
pal <- colorRampPalette(c("dodgerblue", "white", "red"))
breaks <- seq(-1, 1, 0.1)
col <- pal(length(breaks)-1)
### Plot the correlation matrix
par(mar = c(5.1,5.1,1,1))
image(x = 1:32, y = 1:32, cor_pdata, col = col, breaks = breaks, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(side = 1, at = 1:32, labels = colnames(pdata), las = 2)
axis(side = 2, at = 1:32, labels = colnames(pdata), las = 2)

fa <- factanal(pdata, factors = 10)
## The columns (the "factors") are the latent "personality pieces"
fa$loadings
## By looking at the numbers in a particular column, we can see how observed personality traits
## are coming together to form this "personality piece"
par(mar = c(5.1,4.1,3.1,1))
plot(fa$loadings[,1], xaxt = "n", xlab = "", ylab = "Loadings (contributions)", main = "Factor 1")
axis(side = 1, at = 1:32, labels = colnames(pdata), las = 2)

plot(fa$loadings[,2], xaxt = "n", xlab = "", ylab = "Loadings (contributions)", main = "Factor 2")
axis(side = 1, at = 1:32, labels = colnames(pdata), las = 2)

## For each of the 32 observed personality traits, we can plot loadings from the first two factors
## Are any groupings apparent?
plot(fa$loadings[,1:2], type = "n")
text(fa$loadings[,1:2], labels = names(pdata), cex = 0.5)

########## CLUSTERING ##########
hc <- hclust(dist(t(pdata)))
plot(hc)
