gradschool <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

View(gradschool)
head(gradschool)
summary(gradschool)

sapply(gradschool, sd)
gradschool$rank <- as.factor(gradschool$rank)
logit_model <- glm(admit ~ gre + gpa + rank, data = gradschool, family = "binomial")
summary(logit_model)

exp(coef(logit_model))

#Install/load pscl if needed
if(!require(pscl)) install.packages("pscl", dependencies = TRUE)
library(pscl)

#pR2() function gives multiple pseudo-R² values
pR2(logit_model)

#McFadden’s R² specifically:
McFadden_R2 <- 1 - (logLik(logit_model) / logLik(update(logit_model, . ~ 1)))
McFadden_R2

gradschool$Predicted <- predict(logit_model, type = "response")
  
percent_change <- (exp(coef(logit_model)) - 1) * 100
round(percent_change, 2)

head(gradschool)

table(Observed = gradschool$admit, Predicted = gradschool$Predicted > 0.5)


library(ROCR)
# Generate prediction and performance objects
ROCRpred <- prediction(gradschool$Predicted, gradschool$admit)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
acc_perf <- performance(ROCRpred, "acc")         # Accuracy at same thresholds


# Build a table of thresholded metrics
roc_tbl <- data.frame(
  threshold   = unlist(ROCRperf@alpha.values),
  tpr         = unlist(ROCRperf@y.values),
  fpr         = unlist(ROCRperf@x.values),
  accuracy    = unlist(acc_perf@y.values)
)

# Clean-up + derived metrics
roc_tbl <- subset(roc_tbl, !is.na(threshold))     # drop NAs that can appear at extremes
roc_tbl <- roc_tbl[order(-roc_tbl$threshold), ]   # sort from high to low threshold
roc_tbl$specificity <- 1 - roc_tbl$fpr
roc_tbl$youdenJ     <- roc_tbl$tpr - roc_tbl$fpr  # useful for picking a cutoff

head(roc_tbl, 10)

plot(ROCRperf, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1, length = 10),
     text.adj = c(-0.2, 1.7))



# Using pROC to confirm best cutoff by Youden’s J and show nearby points
if (!require(pROC)) install.packages("pROC", dependencies = TRUE)
library(pROC)

roc_obj <- roc(gradschool$admit, gradschool$Predicted, quiet = TRUE)
best_j   <- coords(roc_obj, "best", ret = c("threshold","sensitivity","specificity"), transpose = FALSE)
best_j

pts <- as.data.frame(coords(roc_obj, x = c(0.3, 0.4, 0.5),
                     ret = c("threshhold", "sensitivity", "specificity", "accuracy"),
                     transpose = FALSE))
pts$one_minus_spec <- 1 - pts$specificity
pts




