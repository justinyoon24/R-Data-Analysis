arrivals.9to5 <- c(25,15,20,30,20,15,10,35);
barplot(arrivals.9to5,
        names.arg=c('9am','10am','11am','12pm','1pm','2pm','3pm','4pm'))
title('Mean Customer Arrivals')
abline(h=mean(arrivals.9to5),col='red',lwd=2)
avg.day <- function(overall.mean){
  arrivals.by.hour <- rpois(n=8,lambda=overall.mean);
  return(arrivals.by.hour)
}
# Simulate 1000 days;
avgDay.m <- matrix(NA,nrow=1000,ncol=8);
for (j in 1:1000){
  avgDay.m[j,] <- avg.day(overall.mean=21.25);
}
# Convert matrix to data frame
avgDay.df <- as.data.frame(avgDay.m);
colnames(avgDay.df) <- c('9am','10am','11am','12pm','1pm','2pm','3pm','4pm');
head(avgDay.df)
boxplot(avgDay.df)
abline(h=mean(arrivals.9to5),col='red',lwd=2)
title('Time Homogenous Simulation')
sim.day <- function(mean.vec){
  arrivals.by.hour <- rep(NA,length(mean.vec));
  for (j in 1:length(mean.vec)){
    arrivals.by.hour[j] <- rpois(n=1,lambda=mean.vec[j]);
  }
  return(arrivals.by.hour)
}
# Simulate 1000 days;
simDay.m <- matrix(NA,nrow=1000,ncol=8);
for (j in 1:1000){
  simDay.m[j,] <- sim.day(mean.vec=c(25,15,20,30,20,15,10,35));
}
# Convert matrix to data frame
simDay.df <- as.data.frame(simDay.m);
colnames(simDay.df) <- c('9am','10am','11am','12pm','1pm','2pm','3pm','4pm');
head(simDay.df)
boxplot(simDay.df)
abline(h=mean(arrivals.9to5),col='grey',lwd=2,lty=2)
points(c(25,15,20,30,20,15,10,35),pch=20,col='red',cex=2)
title('Time Non-Homogenous Simulation')
# Define quantile points of interest
p <- c(0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99);
# Compute hourly quantiles for average day simulation
avgDay.quantiles <- lapply(avgDay.df,MARGIN=2,FUN=quantile,p);
# Check that lapply() was used correctly
quantile(avgDay.df$'3pm',p)
ks.test(avgDay.df$'9am',simDay.df$'9am')
avgDay.grade9am <- ifelse(avgDay.df$'9am'>30,'01: Understaffed',
                          ifelse(avgDay.df$'9am'<13,'02: Overstaffed',
                                 '03: Staffed Correctly'))
avgDay.grade10am <- ifelse(avgDay.df$'10am'>30,'01: Understaffed',
                          ifelse(avgDay.df$'10am'<13,'02: Overstaffed',
                                 '03: Staffed Correctly'))
avgDay.grade11am <- ifelse(avgDay.df$'11am'>30,'01: Understaffed',
                          ifelse(avgDay.df$'11am'<13,'02: Overstaffed',
                                 '03: Staffed Correctly'))
avgDay.grade12pm <- ifelse(avgDay.df$'12pm'>30,'01: Understaffed',
                          ifelse(avgDay.df$'12pm'<13,'02: Overstaffed',
                                 '03: Staffed Correctly'))
avgDay.grade1pm <- ifelse(avgDay.df$'1pm'>30,'01: Understaffed',
                          ifelse(avgDay.df$'1pm'<13,'02: Overstaffed',
                                 '03: Staffed Correctly'))
avgDay.grade2pm <- ifelse(avgDay.df$'2pm'>30,'01: Understaffed',
                          ifelse(avgDay.df$'2pm'<13,'02: Overstaffed',
                                  '03: Staffed Correctly'))
avgDay.grade3pm <- ifelse(avgDay.df$'3pm'>30,'01: Understaffed',
                          ifelse(avgDay.df$'3pm'<13,'02: Overstaffed',
                                  '03: Staffed Correctly'))
avgDay.grade4pm <- ifelse(avgDay.df$'4pm'>30,'01: Understaffed',
                          ifelse(avgDay.df$'4pm'<13,'02: Overstaffed',
                                  '03: Staffed Correctly'))
avgDay.performance <- rbind(table(avgDay.grade9am),
                            table(avgDay.grade10am),
                            table(avgDay.grade11am),
                            table(avgDay.grade12pm),
                            table(avgDay.grade1pm),
                            table(avgDay.grade2pm),
                            table(avgDay.grade3pm),
                            table(avgDay.grade4pm)
);
avgDay.performance
rownames(avgDay.performance) <- c('9am','10am','11am','12pm','1pm','2pm','3pm','4pm');
avgDay.performance
avgDay.performance/1000
table(simDay.grade12pm)
simDay.grade9am <- ifelse(simDay.df$'9am'>30,'01: Understaffed',
                          ifelse(simDay.df$'9am'<13,'02: Overstaffed',
                                 '03: Staffed Correctly'))
simDay.grade10am <- ifelse(simDay.df$'10am'>30,'01: Understaffed',
                           ifelse(simDay.df$'10am'<13,'02: Overstaffed',
                                  '03: Staffed Correctly'))
simDay.grade11am <- ifelse(simDay.df$'11am'>30,'01: Understaffed',
                           ifelse(simDay.df$'11am'<13,'02: Overstaffed',
                                  '03: Staffed Correctly'))
simDay.grade12pm <- ifelse(simDay.df$'12pm'>30,'01: Understaffed',
                           ifelse(simDay.df$'12pm'<13,'02: Overstaffed',
                                  '03: Staffed Correctly'))
simDay.grade1pm <- ifelse(simDay.df$'1pm'>30,'01: Understaffed',
                          ifelse(simDay.df$'1pm'<13,'02: Overstaffed',
                                 '03: Staffed Correctly'))
simDay.grade2pm <- ifelse(simDay.df$'2pm'>30,'01: Understaffed',
                          ifelse(simDay.df$'2pm'<13,'02: Overstaffed',
                                 '03: Staffed Correctly'))
simDay.grade3pm <- ifelse(simDay.df$'3pm'>30,'01: Understaffed',
                          ifelse(simDay.df$'3pm'<13,'02: Overstaffed',
                                 '03: Staffed Correctly'))
simDay.grade4pm <- ifelse(simDay.df$'4pm'>30,'01: Understaffed',
                          ifelse(simDay.df$'4pm'<13,'02: Overstaffed',
                                 '03: Staffed Correctly'))
simDay.performance <- rbind(table(simDay.grade9am),
                            table(simDay.grade10am),
                            table(simDay.grade11am),
                            table(simDay.grade12pm),
                            table(simDay.grade1pm),
                            table(simDay.grade2pm),
                            table(simDay.grade3pm),
                            table(simDay.grade4pm)
);
simDay.performance
rownames(simDay.performance) <- c('9am','10am','11am','12pm','1pm','2pm','3pm','4pm');
simDay.performance
simDay.performance/1000
simDay.performance['10am', '01: Understaffed'] <- 0
simDay.performance['10am', '02: Overstaffed'] <- 278
simDay.performance['10am', '03: Staffed Correctly'] <- 722
simDay.performance['12pm', '02: Overstaffed'] <- 0
simDay.performance['12pm', '03: Staffed Correctly'] <- 508
simDay.performance['2pm', '01: Understaffed'] <- 0
simDay.performance['2pm', '02: Overstaffed'] <- 282
simDay.performance['2pm', '03: Staffed Correctly'] <- 718
simDay.performance['3pm', '01: Understaffed'] <- 0
simDay.performance['3pm', '02: Overstaffed'] <- 790
simDay.performance['3pm', '03: Staffed Correctly'] <- 210
simDay.performance['4pm', '02: Overstaffed'] <- 0
simDay.performance['4pm', '03: Staffed Correctly'] <- 244
simDay.performance
simDay.performance/1000
