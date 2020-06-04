
setwd("C:/Users/NCHM/Desktop/my office work and reference/Re_work/statistical_verification/tingtibi")
my_data<-read.csv("C:/Users/NCHM/Desktop/my office work and reference/Re_work/statistical_verification/tingtibi/djf_data.csv", header=T,stringsAsFactors = F)
View(my_data)
class(my_data)

time<-as.POSIXlt(my_data$Date,format='%d/%m/%y')
time
class(time) ###checks the class again
head(time)

###extracting year,month and day
year<-time$year+1900
month<-time$mon+1
day<-time$mday
View(month)
class(my_data$Automatic)
class(my_data$Manual)
summary(my_data$Manual)

####automatic<-as.numeric(my_data$awwl)###if not numeric    


###BOXPLOT########################################

boxplot(my_data$Automatic,my_data$Manual,names = c('Automatic','Manual'), boxwex=0.35,staplewex=0.5, col = c("blue", "green"), ylab='Water Level(meters)',ylim=c(2.1,3))
legend(x=1.4,y=2.3,legend=c("Automatic","Manual"),fill=c("blue","green"),cex=0.6)



#####LABEL QUANTILES###########################

text(y = boxplot.stats(my_data$Automatic)$stats, labels = boxplot.stats(my_data$Automatic)$stats,x=0.72,cex=2/3)
text(y = boxplot.stats(my_data$Manual)$stats, labels = boxplot.stats(my_data$Manual)$stats,x=2.3,cex=2/3)
##################################################################
sd(my_data$Manual,na.rm =TRUE)
#####scatterplot using qplot##################
####qplot(my_data$Manual,automatic,xlab='Manual',ylab='Automatic',xlim=c(2.5,11))+geom_abline()

#######scatterplot using simple plot############
reg<-lm(my_data$Automatic~my_data$Manual) ####depended variable should come first,and then independent
coeff<-coefficients(reg)
coeff
equation<-paste0("y = ", round(coeff[2],1), "*x  ", round(coeff[1],1))
plot(my_data$Manual,my_data$Automatic,main=equation,xlab='Manual',ylab='Automatic',ylim=c(2.3,3),xlim=c(2.3,3),col='blue',cex.axis=0.9,cex.lab=0.9) ###independent variable first and then dependent variable
abline(reg,col='black')

####correlation test######
cor.test(my_data$Automatic,my_data$Manual)
summary(my_data$Manual)


