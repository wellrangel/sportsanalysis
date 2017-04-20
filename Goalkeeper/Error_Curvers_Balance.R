library(ggplot2)

Balance <- read.csv(file.choose(), sep=";")

head(Balance)


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}




#summary(Balance/Unbalance)
CS = summarySE(Balance, measurevar = "Goal.potential", groupvars=c("Situation.type", "State"))

CS
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right


plot.title = "Balance:save in green; goal in red"
plot.subtitle = "Goal potential dynamics"

ggplot(CS, aes(x=State, y=Goal.potential, colour=Situation.type)) + 
  geom_errorbar(aes(ymin=Goal.potential-se, ymax=Goal.potential+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") +
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +  #format title and subtitle
  expand_limits(y=0) +                        # Expand y range
  theme_bw() +
  theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16)) + #font axis x
  theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20)) + #font title axis x
  theme(legend.title = element_text(colour="blue", size=16, face="bold"), #font legend
        text = element_text(colour="black", size=16), #font text legend
        legend.justification=c(.5,0), #justification legend
        legend.position=c(.5,0))      # Position legend in bottom center

