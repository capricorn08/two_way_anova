dtwo <- read.csv('twoaov.csv', header = FALSE)
library(reshape)
dtwo <- `colnames<-`(dtwo,c("group", "x1","x2","x3", "x4","x5"))
colnames(dtwo)
str(dtwo)
md <- melt(dtwo, id = "group")
str(md)



attach(md)

interaction.plot(variable,group,value, 
                 type="b", pch = c(16,18),   
                 legend = "TRUE",    col = c(3,4), 
                 main = "Mean of Outcome", ylab="recognition score", xlab = "type of strategy")

library(ggplot2)
p <- qplot(as.factor(variable), value, data=md, geom = "boxplot", color = group) + theme_bw()
p <- p + labs(x="type of level processing", y="Response")
p <- p + stat_summary(fun.y = mean, geom = "point", color = "blue", aes(group=group))
p <- p + stat_summary(fun.y = mean, geom = "line", aes(group = group))
p <- p  + theme(axis.title.x = element_text(size = 12, hjust = 0.54, vjust = 0))
p <- p  + theme(axis.title.y = element_text(size = 12, angle = 90,  vjust = 0.25))
print(p)
library(moonBook)
library(ztable)
basicstat <- mytable(variable+group~value, data=md)
ztable(basicstat)

############################## Two-way ANOVA #########################################
library(multcomp)
mdaov <- aov(value~group+variable+group:variable)
summary(mdaov)
str(mdaov)
dim(mdaov)
names(mdaov)
plot(mdaov$residuals, main = "residual plot")
############################ post-hoc analysis ####################################

md$group <- as.numeric(md$group)
md$variable <- as.numeric(md$variable)
md$gv <- as.factor (10*md$group + md$variable)

GVaov <- aov(value~gv, data=md)
summary(GVaov)
contra <- rbind("old - young_x2" = c( 0,1,0,0,0,0, -1,0,0, 0),
                "old - young_x3" =c( 0,0,1,0,0,0, 0,-1,0 ,0),
                "old - young_x4" =c( 0,0,0,1,0,0, 0,0,-1, 0),
                "old - young_x5" =c( 0,0,0,0,1,0,0,0,0, -1))

GVcom <- glht(GVaov, linfct=mcp(gv=contra), alternative = "two.sided")
summary(GVcom)
