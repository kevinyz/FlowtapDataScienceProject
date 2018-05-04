adultData <- read.table("./adult.data", sep=",", header=FALSE,
                        col.names=c("age","workclass","fnlwgt","education","education-num",
                                    "marital-status","occupation","relationship","race",
                                    "sex","capital-gain","capital-loss","hours-per-week",
                                    "country","income"), strip.white = TRUE, stringsAsFactors = FALSE)

is.na(adultData) = adultData=='?'
is.na(adultData) = adultData==' ?'
adultData = na.omit(adultData)

adultData$above50k <- as.integer(as.logical(">50K" == adultData$income))

adultData$marital[adultData$marital.status=="Never-married"] = "Never-Married"
adultData$marital[adultData$marital.status=="Married-AF-spouse"] = "Married"
adultData$marital[adultData$marital.status=="Married-civ-spouse"] = "Married"
adultData$marital[adultData$marital.status=="Married-spouse-absent"] = "Not-Married"
adultData$marital[adultData$marital.status=="Separated"] = "Not-Married"
adultData$marital[adultData$marital.status=="Divorced"] = "Not-Married"
adultData$marital[adultData$marital.status=="Widowed"] = "Widowed"

adultData$area[adultData$country=="United-States"] = "US"
adultData$area[adultData$country!="United-States"] = "Non-US"

set.seed(100)
split <- sample(seq_len(nrow(adultData)), size = floor(0.75 * nrow(adultData)))
trainData <- adultData[split, ]
testData <- adultData[-split, ]
head(trainData)
head(testData)

lrMod <- glm(above50k ~ factor(sex) + factor(education) + hours.per.week + age + factor(occupation) + factor(relationship) + fnlwgt + capital.gain + capital.loss + factor(marital) + factor(area),family=binomial(link='logit'),data=trainData)
summary(lrMod)

prediction <- predict(lrMod,newdata=testData,type='response')
prediction <- ifelse(prediction > 0.5,1,0)
head(prediction)
head(testData$above50k)


error <- mean(prediction != testData$above50k)
print(paste("Accuracy: ",  1-error))
