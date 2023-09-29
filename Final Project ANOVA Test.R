cases_df <- read.csv("~/My Documents/Alexis/College/SJSU/2021-2022/Spring 2022/BUS2 194A/cases.csv", header = TRUE, colClasses = c("factor", "numeric"))
summary(cases_df)

boxplot(Cases ~ ï..Regions, data=cases_df, main ="Total Omicron Cases",
        xlab="Regions", ylab="Number of Omicron Cases")

cases_anova <- aov(ï..Regions ~ Cases, data = cases_df)
summary(cases_anova)

library(car)
leveneTest(`Cases` ~ ï..Regions, data = cases_df)
