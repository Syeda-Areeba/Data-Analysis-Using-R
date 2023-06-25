#-----------------------------------------------------------------------------#
#                             QUESTION NO.1
#-----------------------------------------------------------------------------#
color<-c('purple','cyan','red','orange')

design_of_robots<-c("Legs only","Wheels only","Both", "None")
number_of_each_robot<-c(63,20,8,15)

robots_labels<- round(number_of_each_robot/sum(number_of_each_robot)*100,1)
robots_labels<-paste(robots_labels,'%',sep=' ')

pie(number_of_each_robot,labels = robots_labels,cex=0.8,
    col = color,main="Pie Chart of design of social robots")
legend('topright',c("Legs only","Wheels only","Legs and Wheels both",
        "Neither legs nor wheels"),fill = color, cex=0.8)

barplot(number_of_each_robot,names.arg=design_of_robots,ylim=c(0,70),
        xlab = "Design of Robots",ylab = "Number of Robots",
        main = "Bar Graph of design of Social Robots",font.main=4,
        col=color)
#-----------------------------------------------------------------------------#
#                             QUESTION NO.2
#-----------------------------------------------------------------------------#
colors<-c('blue','yellow','green','red','grey','purple')

#---------------------------------PART (a)------------------------------------#
dataset<-BOD
dataset
barplot(dataset$demand,names.arg = dataset$Time,col = colors,ylim=c(0,25),
        xlab= 'Time in Days', ylab='BOD',main= "Biochemical oxygen demand 
        versus time in an evaluation of water quality")
#-----------------------------OR---------------------------#
data<-InsectSprays
data
#grouping and summing up the data and then plotting it
A<-subset(data,data$spray == 'A')
A<-sum(A$count)
A
B<-subset(data,data$spray == 'B')
B<-sum(B$count)
B
C<-subset(data,data$spray == 'C')
C<-sum(C$count)
C
D<-subset(data,data$spray == 'D')
D<-sum(D$count)
D
E<-subset(data,data$spray == 'A')
E<-sum(E$count)
E
F<-subset(data,data$spray == 'F')
F<-sum(F$count)
F

barplot(c(A,B,C,D,E,F),names.arg=c('A','B','C','D','E','F'),col=colors,
        ylim = c(0,200), xlab = "Types of treatment Insecticides",
        main= "Bar Graph of Effectiveness of different Insecticides on 
        no.of Insects",
        ylab = "No of Insecticides under treatment")
legend('top',c("A","B","C","D","E","F"),
       fill = colors,cex = 0.7) 
#---------------------------------PART (b)------------------------------------#
#install.packages('ggplot2')
library('ggplot2')
df=diamonds
df
#?diamonds
summary(diamonds)

par(mfrow = c(3, 1))
plot(diamonds$x,diamonds$price,pch=19,xlab = 'length in mm',ylab = "Price",
     main='length vs price of diamond',col='purple')
plot(diamonds$y,diamonds$price,pch=19,xlab = 'width in mm',ylab = "Price",
     main='width vs price of diamond',col='orange')
plot(diamonds$z,diamonds$price,pch=19,xlab = 'depth in mm',ylab = "Price",
     main='depth vs price of diamond',col='brown')
par(mfrow = c(3, 1))

par(mfrow = c(3, 1))
plot(diamonds$carat,diamonds$price,pch=19,xlab = 'carat(weight)',
     ylab = "Price",main='weight vs price of diamond',col='blue')
plot(diamonds$depth,diamonds$price,pch=19,xlab = 'Total depth',
     ylab = "Price",main='total depth vs price of diamond',col='yellow')
plot(diamonds$table,diamonds$price,pch=19,xlab = 'width of top',
     ylab = "Price", main='width of top vs price of diamond',col='green')
par(mfrow = c(3, 1))

par(mfrow = c(2, 1))
t1<-table(diamonds$cut)
barplot(t1,diamonds$price,xlab = 'Cut',ylab = "Price",col=rainbow(length(t1)),
        main="cut vs price of diamond")

t2<-table(diamonds$clarity)
barplot(t2,diamonds$price,xlab = 'Clarity',ylab = "Price",
        col=rainbow(length(t2)),main="clarity vs price of diamond")
par(mfrow = c(2, 1))

#-----------------------------------------------------------------------------#
#                             QUESTION NO.3
#-----------------------------------------------------------------------------#
#continuous_data <- as.integer(rnorm(50,mean=30,sd=10))
continuous_data <-as.integer(runif(50,min=5,max=100))
continuous_data

min <- min(continuous_data)
max <- max(continuous_data)
range <- max - min

class_width <- range/7
class_width <- ceiling(class_width)

min <- min-0.5
max <- max+0.5

classes<-seq(min,max+class_width,by=class_width) #breaks
class_boundaries<-cut(continuous_data,classes)
freq_distribution<-transform((table(class_boundaries)))
relative_freq=prop.table(freq_distribution$Freq)
cummulative_freq = cumsum(freq_distribution$Freq)

i<-1:7 
mid_points<-round(c(classes[i]+(class_width/2)),0) 
end_mid_point= max(mid_points)+class_width
freq_distribution<-transform(freq_distribution,Relative_Freq=relative_freq,
                             Cummulative_Freq=cummulative_freq,
                             Mid_Points=mid_points)
freq_distribution

graph<-hist(continuous_data,breaks = classes,xlab = "Class Boundaries",
            col = rainbow(length(classes)),ylab = "Frequency",
            xlim = c(0,end_mid_point+class_width),
            ylim = c(0,max(freq_distribution$Freq)),main = "Histogram of 
            continuous data with frequency polygon superimposed")

freq <- c(0,freq_distribution$Freq,0)
freq
graph<-lines(c(0,mid_points,end_mid_point),freq,col='black',lwd=3,
      type='o',)
graph
legend('topright',c('Frequency polygon'),fill='black',cex=0.7)

#-----------------------------------------------------------------------------#
#                             QUESTION NO.4
#-----------------------------------------------------------------------------#
getwd()
setwd("C:/Users/HP/Desktop/R - prob & stat Assignment 2")
covid_data<-read.csv("covid_19_data.csv",header=TRUE)
covid_data
#---------------------------------PART (a)------------------------------------#
countries<-unique(covid_data$Country.Region)
print("Countries included in this study are: ")
#countries
print(length(countries))
#---------------------------------PART (b)------------------------------------#
china<-subset(covid_data,covid_data$Country.Region=='Mainland China')

china_provinces<-subset(china,china$Confirmed>0,Province.State)
china_provinces<-unique(china_provinces$Province.State)
#china_provinces
#removing unknown
china_provinces= china_provinces[0:(length(china_provinces)-1)]
print("Covid cases were found in the following countires of China:")
china_provinces
length(china_provinces)

china_death<-subset(china,china$Deaths>=0,Deaths)
china_death<-sum(china_death$Deaths)
china_death

#---------------------------------PART (c)------------------------------------#
summary(covid_data)

#---------------------------------PART (d)------------------------------------#
south_aus<-subset(covid_data,covid_data$Province.State=="South Australia",
                  c(Confirmed,Deaths,Recovered,ObservationDate))
south_aus$ObservationDate <- as.POSIXct(south_aus[['ObservationDate']],
                                    format = c("%m/%d/%Y"))
#south_aus

par(mfrow = c(3, 1))
confirm <-subset(south_aus,south_aus$Confirmed>=0,c(Confirmed,ObservationDate))
confirm
plot(confirm$ObservationDate, confirm$Confirmed, pch= 19, col = "blue",
     xlab = "Time Period in Years", ylab = "Confirmed Cases",
     main="Confirmed Covid cases in South Australia over time")

death <- subset(south_aus,south_aus$Deaths>=0,c(Deaths,ObservationDate))
plot(death$ObservationDate, death$Deaths,  pch=19, col = "red", 
     xlab = "Time Period in Years", ylab = "Death Cases",
     main="Covid Death cases in South Australia over time")

recover <- subset(south_aus,south_aus$Recovered>=0,c(Recovered,ObservationDate))
plot(recover$ObservationDate, recover$Recovered, pch=19, col = "green", 
     xlab = "Time Period in Years", ylab = "Recovered Cases",
     main="Covid Recovered cases in South Australia over time")
par(mfrow = c(3, 1))
#---------------------------------PART (e)------------------------------------#
new_df<-covid_data
new_df
new_df$ObservationDate <- as.POSIXct(new_df[['ObservationDate']],
                                    format = "%m/%d/%Y" )
new_df
new_df <- subset(new_df, as.Date(ObservationDate) > as.Date("2021-01-01"))
new_df

write.csv(new_df,"Covid-19 after 2021-01-01.csv")
new_covid_data <- read.csv("Covid-19 after 2021-01-01.csv")
print(new_covid_data)
#-----------------------------------------------------------------------------#