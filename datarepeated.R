#install.packages("afex")
#install.packages("lsmeans")
#devtools::install_github("singmann/afex@master")
library(afex, lsmeans)
library(car)
library(reshape)
##names(data) <- c("new_name", "another_new_name")

data <- read.csv("datarepeated.csv", header = TRUE)
datLong <- melt(data, id = "Subject")

head(datLong)
names(datLong) <- c("subject", "time", "outcome")
dim(datLong)
fit_all <- aov_ez("subject","outcome", datLong, within=c("time"))
summary(fit_all)

#fit_mixed <- mixed(outcome~time+(time|subject),data=datLong)
#summary(fit_mixed)

library(lsmeans)
ref <- lsmeans(fit_all,specs = "time")
summary(ref)


ref_df<- as.data.frame(summary(ref))
summary(ref_df)
plot(ref_df$time,ref_df$lsmean, lty=2, type="b", pch = c(16,18))
plot.new()


#Testing
