install.packages("ggplot2")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggthemes")
install.packages("plot3D")
library(ggplot2)
library(sqldf)
library(dplyr)
library(plotrix)
library(ggthemes)
library(plot3D)
# Setting the path for importing CSV file
getwd()
setwd()
#Importing file from folder

teleData = read.csv("C:/Users/sachi/Desktop/Data Science/R Project/bank-additional-full.csv", stringsAsFactors = FALSE)
head(teleData)

######################################################################################
teleData$month <- factor(teleData$month,levels = c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),order = TRUE)
teleData$month
teleData$day_of_week <- factor(teleData$day_of_week,levels = c("mon","tue","wed","thu","fri"),order = TRUE)
teleData$day_of_week
teleData$education <- factor(teleData$education,levels = c("illiterate","basic.4y","basic.6y","basic.9y","high.school","professional.course","university.degree","unknown"),order = TRUE)
teleData$education
teleData$poutcome <- factor(teleData$poutcome,levels = c("failure","success","nonexistent"),order = TRUE)
teleData$poutcome
####################################################################################
head(teleData)
m_efficient_tele <- teleData %>% filter(.,y == "yes")  %>%group_by(month)
head(m_efficient_tele)
#####################################################################################
#Success month wise fill by days( Monday to friday )
count_day_tele <- m_efficient_tele %>% count(day_of_week)
head(count_day_tele)
gplot_day_wise <-ggplot(count_day_tele,aes(x = month,y = n)) + geom_bar(stat= "identity",width = 0.5,aes(fill = day_of_week),position = position_dodge()) +
labs(x ="Months",y = "No.of Success day wise",color = "Working Days") 
g_month_day <- gplot_day_wise +  ggtitle("Count of Converted Calls day wise within month") 
final_plot_2 <- g_month_day  + scale_y_continuous( breaks = seq(0,250,10)) 
final_plot_2  + theme_economist()+scale_x_discrete(labels= c("Apr","Aug","Dec","Jul","Jun","Mar","May","Nov","Oct","Sep"))+theme(plot.title = element_text(hjust = 0.5))+ scale_fill_discrete(name = "Day of Week",labels = c("Mon","Tue","Wed","Thu","Fri","Sat"))+theme(axis.title = element_text(size = 12,face = "bold"))+(theme(axis.text.x = element_text(face = "bold")))+(theme(axis.text.y = element_text(face = "bold")))
#####################################################################################
#no of success count according to education
e_efficient_tele <- teleData %>% filter(.,y == "yes") %>%group_by(education)
head(e_efficient_tele)
count_education <- e_efficient_tele %>% count(education)
count_education
gplot_education_success <-ggplot(count_education,aes(x = education,y = n)) + geom_bar(stat= "identity",position = position_dodge(),fill = "Black",width = 0.5)+
labs(x ="Education Type",y = "No.of Success Education wise",color = "Working Days")
g_education <- gplot_education_success +  ggtitle("Count of Converted Calls education wise") 
final_plot_3<- g_education + theme_economist() 
final_plot_3+scale_x_discrete(labels= c("Illeterate","","Till 4th Grade","Till 6th Grade","Till 9th Grade","High School","Professional Course","University Degree","Unknown Edu","Dec"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.title = element_text(size = 12,face = "bold"))+(theme(axis.text.x = element_text(face = "bold")))+(theme(axis.text.y = element_text(face = "bold")))
####################################################################################
#Success count age wise
age_efficient_tele <- teleData %>% filter(.,y == "yes") %>%group_by(age)
head(age_efficient_tele)
age_count_success <- age_efficient_tele %>% count(age)
age_count_success

gplot_age_success <- ggplot(age_count_success,aes(x = age,y = n)) + geom_bar(stat= "identity",position = position_dodge(),fill = "Black", width = 0.5) +
labs(x ="Age",y = "No.of Success age wise")  + theme_economist()
g_age <- gplot_age_success +  ggtitle("Count of Converted Calls age wise") 
final_plot_4<- g_age  + scale_x_continuous(breaks= seq(17,100,5))+ scale_y_continuous(breaks = seq(0,300,10))
final_plot_4 +  theme(plot.title = element_text(hjust = 0.5))+theme(axis.title = element_text(size = 12,face = "bold"))+(theme(axis.text.x = element_text(face = "bold")))+(theme(axis.text.y = element_text(face = "bold")))
  