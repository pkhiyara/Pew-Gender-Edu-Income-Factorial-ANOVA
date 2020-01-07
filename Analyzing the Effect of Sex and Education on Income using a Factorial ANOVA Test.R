d = read.csv("/GenderEduIncome.csv") # read in the Higher Education, Income, and Gender dataset
# Conducted a full-factorial two-way ANOVA test
a = aov(appxincome~sex*educ,data=d)
summary(a)
summary(d$educ)
# Created subsets and histograms for each main effect to test for the normality assumption for ANOVA
d.male = subset(d, sex=="Male")
hist(d.male$appxincome)
d.female = subset(d, sex=="Female")
hist(d.female$appxincome)
d.college = subset(d, educ=="College")
hist(d.college$appxincome)
d.highschool = subset(d, educ=="High School Grad")
hist(d.highschool$appxincome)
d.lesshighschool = subset(d, educ=="Less High School")
hist(d.lesshighschool$appxincome)
# Imported the car library
library(car)
# Conducted multiple Levene's Tests for the homogeneity of variance assumption for ANOVA
leveneTest(d$appxincome~d$sex)
leveneTest(d$appxincome~d$educ)
# Created subsets for each interaction between the two main effects
d.lesshighschoolm = subset(d, educ=="Less High School" & sex=="Male")
d.lesshighschoolf = subset(d, educ=="Less High School" & sex=="Female")
d.highschoolf = subset(d, educ=="High School Grad" & sex=="Female")
d.highschoolm = subset(d, educ=="High School Grad" & sex=="Male")
d.collegem = subset(d, educ=="College" & sex=="Male")
d.collegef = subset(d, educ=="College" & sex=="Female")
# Conducted Corrected Pairwise t-tests for each main effect and possible interaction between the two main effects
TukeyHSD(a)
# Found the mean and standard deviation for each interaction subset created
mean(d.collegef$appxincome)
sd(d.collegef$appxincome)
mean(d.collegem$appxincome)
sd(d.collegem$appxincome)
mean(d.highschoolf$appxincome)
sd(d.highschoolf$appxincome)
mean(d.highschoolm$appxincome)
sd(d.highschoolm$appxincome)
mean(d.lesshighschoolm$appxincome)
sd(d.lesshighschoolm$appxincome)
mean(d.lesshighschoolf$appxincome)
sd(d.lesshighschoolf$appxincome)
# Calculated the 95% Confidence Intervals for each interaction subset
qt(0.025,43,lower.tail=FALSE)
qt(0.025,184,lower.tail=FALSE)
qt(0.025,516,lower.tail=FALSE)
qt(0.025,59,lower.tail=FALSE)
qt(0.025,242,lower.tail=FALSE)
qt(0.025,538,lower.tail=FALSE)
# Created a dataframe with the mean song duration, upper confidence limit, and lower confidence limit for each interaction
d.ci = data.frame(Education=c("Less High School","Less High School","High School Grad","High School Grad", "College", "College"), Sex = c("Female", "Male", "Female", "Male", "Female", "Male"), Income = c(68465.91, 71791.67, 59837.84, 90390.95, 93428.43, 91071.43), ci.upper = c(104582.1579, 102677.4164, 73372.68557, 104851.796, 102251.0192, 98451.61902), ci.lower = c(32349.66212, 40905.92362, 46302.99443, 75930.104, 84605.84079, 83691.24098))
# Imported the ggplot2 library
library(ggplot2)
# Plotted a bar graph with error bars to visualize the effect of sex and educational attainment on income
dodge <- position_dodge(width = 0.9)
ggplot(d.ci, aes(x = Education, y = Income, fill = Sex))+geom_bar(stat="identity", position = position_dodge())+geom_errorbar(aes(ymin=ci.lower,ymax=ci.upper), position = dodge, width=.2)+scale_y_continuous(expand = c(0,0))+coord_cartesian(ylim = c(0, 125000))+theme_classic()+ ylab("Income ($)") + xlab("Educational Attainment")

