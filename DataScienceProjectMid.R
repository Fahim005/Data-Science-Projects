mydata <- read.csv("D:/Dataset_midterm_Section(A).csv",header = TRUE,sep = ",")
mydata

names(mydata)
View(mydata)
missmap(mydata, col=c("black", "grey"))

str(mydata)
mydata$survived = factor(mydata$survived)
summary(mydata)
s<-mydata$Loan
sd(s)
mydata %>% summarise_if(is.numeric, sd)


sample_n(mydata,3)
colSums(is.na(mydata))
which(is.na(mydata$Loan))
mydata
 
remove<- na.omit(mydata)

mydata
starwars %>% skim()
starwars %>% filter(is.na(gender))

head(mydata)
tail(mydata)


view(mydata)
mydata %>% filter(age < 1)

mydata %>% filter(age == max(age, na.rm=TRUE))

mydata %>% filter(who=="female") %>% filter(age == max(age, na.rm=TRUE))

mydata %>% filter(sibsp == max(sibsp, na.rm=TRUE))

mydata %>% filter(survived == 0) %>% filter(age < quantile(age, 0.05, na.rm=TRUE))

mydata %>% filter(survived == 1) %>% filter(age > quantile(age, 0.98, na.rm=TRUE))

mydata("Titanic")
titanic 
  head(5) 
  kable() 
  kable_styling()

otree <- outlier.tree(titanic)
otree

str(mydata)
ggplot(mydata, aes(x= who, fill = survived)) +
  geom_bar(width=0.3)

ggplot(mydata, aes(who)) +
  facet_wrap(~class) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=survived), stat= "count")+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) +
  ggtitle("class") + labs(y = "percent")


train <- mydata[1:60,]
test <- mydata[60:100,]
set.seed(75)
rpois(3,5)

