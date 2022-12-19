#Loading Libraries 
library(ggplot2)
library(dplyr)
library(scales)
library(data.table)
library(MASS) 
library(stargazer)

#install.packages("readxl")
library(readxl)

#my_data <- read_excel("Salaries.xls")


df<-read.csv(choose.files())

df<-data.frame(df)
options(scipen = 100, digits = 4)


df['OvertimePay']<-as.numeric(as.character(df$OvertimePay))
df['BasePay']<-as.numeric(as.character(df$BasePay))
df['OtherPay']<-as.numeric(as.character(df$OtherPay))

#*Question:1

# hypothsis testing
df$BasePay %>% t.test(mu=66000)

#Corelation table

relationdf<-df[c(2,38,40,45,55,95),]
Pre <- lm(OvertimePay~BasePay ,data=relationdf)
summary(Pre)

cor(df$BasePay,df$OvertimePay, use = "complete.obs") # corelation between variables 

# visula represention 
p1<-relationdf  %>% ggplot( aes(BasePay,OvertimePay))+
  geom_point()+
  labs(
    title = "Base pay vs OvertimePay",
    subtitle = "OvertimePay Vs BasePay",
    x="Base Pay ",
    y="Overtime"
  )+
  theme(plot.title = element_text(size=22,colour = "Red"),
        axis.text = element_text(size = 12,colour = "#229954"),
        axis.title = element_text(size = 15,color = "#78281F"),
        axis.line = element_line(colour = "darkblue", 
                                 size = 1, linetype = "solid")
  )

##Linear regression model

mdata<-cbind.data.frame(relationdf$BasePay ,relationdf$OvertimePay)
mdata

te<- ggplot(mdata,aes(x=relationdf$BasePay,y=relationdf$OvertimePay))+
  geom_point(color="black")+
  labs(
    title = "Base pay vs OvertimePay",
    subtitle = "OvertimePay Vs BasePay",
    x="Base Pay ",
    y="Overtime"
  )+
  theme(plot.title = element_text(size=22,colour = "Red"),
        axis.text = element_text(size = 12,colour = "#229954"),
        axis.title = element_text(size = 15,color = "#78281F"),
        axis.line = element_line(colour = "darkblue", 
                                 size = 1, linetype = "solid")
  )
  theme_bw()

## addline
p2<-te + geom_smooth(method = lm ,color='red',se=FALSE)

#ploting PI band

p3<-te+geom_smooth(method = lm ,color='red',fill="green",se=TRUE)


#2 How much  money earn by various   police officers based on the rank, do that dependent on basepay 

Gr<-df %>% filter(JobTitle %in% c('CAPTAIN III (POLICE DEPARTMENT)','INSPECTOR III, (POLICE DEPARTMENT)')) 
dfGr<-Gr[c(2,37,45,55),]


dfGr['OvertimePay']<-as.numeric(as.character(dfGr$OvertimePay))
dfGr['BasePay']<-as.numeric(as.character(dfGr$BasePay))
dfGr['OtherPay']<-as.numeric(as.character(dfGr$OtherPay))


#hypothsis testing

t.test(dfGr$BasePay, dfGr$OtherPay,mu=18500, alternative = "two.sided", var.equal = FALSE)


# Corelation table
Pre2 <- lm(BasePay~OtherPay ,data=dfGr)
summary(Pre2)
cor(df$OtherPay,df$BasePay, use = "complete.obs") # corelation between variables 

## Visula represntion
dfGrV<-Gr[c(2,37,45,55,5,25,78,98,45,78,5,6,62,85,112),]
dfGrV %>% ggplot(aes(BasePay,OtherPay))+
  geom_point(aes(colour = factor(JobTitle)), size = 2)+
  guides(color = guide_legend(title = "Roles"))+
  labs(
    title = "Correlation, base pay and other pay",
    subtitle = "Base and other pay",
    x="Base Pay",
    y="Other Pay"
  )+
  theme(plot.title = element_text(size=22,colour = "#6E2C00"),
        axis.text = element_text(size = 12,colour = "#F4D03F"),
        axis.text.x = element_text(size = 10,colour = "#196F3D"),
        axis.title = element_text(size = 15,color = "#78281F"),
        axis.line = element_line(colour = "#717D7E", 
                                 size = 1, linetype = "solid")
  )


##Linear regression models

plot(df$BasePay, df$OtherPay, main = "Regression between base pay and Other pay",
     xlab = "Base Pay", ylab = "Other Pay",
     pch = 5, frame = FALSE)
abline(lm(y ~ x, data = df), col = "Red")


#3 What is the relation between base pay and total pay, if both variables are dependent how much change in one can impact other 
#Hypothesis Testing
df['TotalPay']<-as.numeric(as.character(df$TotalPay))
df$TotalPay %>% t.test(mu=75000)


# Corelation table 
mdata3<-cbind.data.frame(relationdf$TotalPay,relationdf$OtherPay )

Pre <- lm(BasePay~TotalPay ,data=relationdf)
summary(Pre)

cor(df$BasePay,df$TotalPay, use = "complete.obs") # corelation between variables 


# Visual represntation
df %>% ggplot(aes(df$TotalPay,df$BasePay))+
  geom_line()+
  scale_y_continuous(labels = comma)+
  xlab("Total Pay")+
  ylab("Base Pay ")+
  ggtitle("Total Pay and base Pay")+
  theme(plot.title = element_text(size=22,colour ="#78281F"),
        axis.text = element_text(size=12,colour = "#F1C40F"),
        axis.title = element_text(size=15,colour = "#AF601A"),
        axis.text.x  = element_text(size=11,colour = "#AF601A"),
        
  )

# Liner Regration model 
plot(df$TotalPay, df$BasePay, main = "comparison between base pay and total pay",
     xlab = "Total Pay", ylab = "Base Pay",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = df), col = "blue")



