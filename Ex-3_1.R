adultData <- read.table("./adult.data", sep=",", header=FALSE,
                        col.names=c("age","workclass","fnlwgt","education","education-num",
                                    "marital-status","occupation","relationship","race",
                                    "sex","capital-gain","capital-loss","hours-per-week",
                                    "country","income"), strip.white = TRUE, stringsAsFactors = FALSE)
is.na(adultData) = adultData=='?'
is.na(adultData) = adultData==' ?'
adultData = na.omit(adultData)

require("ggplot2")

p <- qplot(adultData$income, data=adultData, geom="bar", fill=adultData$sex, alpha=I(.5),
           main="Number of people with a salary above/below 50k by Gender", xlab="Salary", ylab="Number of people")
p <- p + labs(fill = "Gender") +
  theme(
    plot.title = element_text(size=15, face="bold.italic"),
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold")
  )
print(p)

# The chart shows that the group of people earning more than 50k is heavily dominated by males, while the group earning less
# than 50k is more evenly distributed.



adultData[["age_group"]] <- ordered(cut(as.numeric(gsub(",","",adultData$age)),breaks = seq(0,100,by=10)))
adultData[["hour_group"]] <- ordered(cut(as.numeric(gsub(",","",adultData$hours.per.week)),breaks = seq(0,80,by=20)))

p2 <- ggplot(adultData, aes(x = adultData$income, y = adultData$age_group)) + 
  theme(legend.position = "top", axis.text = element_text(size=10)) + 
  geom_point(aes(colour=adultData$hour_group), alpha = 0.7, size = 2, position = position_jitter(width = 0.4, height = 0.4))
p2 <- p2 + labs(title="Relationship between Working Hours, Age and Salary", colour="Working Hours/Week", y="Age", x="Salary") +
  theme(
    plot.title = element_text(size=15, face="bold.italic"),
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold")
  )
print(p2)

# The right half of the chart shows that most of the people earning more than 50k are between 20 and 
# 60 years old, and work mostly 40-60 hours per week. The group earning less than 50k, between 20 and 70 years old, primarily works 20-40 hours.
# The age group outside of 20-70 years, earning less than 50k is working mostly 0-20 hours.