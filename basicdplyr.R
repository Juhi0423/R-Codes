install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)
setwd("C:\\Users\\Administrator\\Desktop\\Casestudy-1")
cases<-read.csv("oj.csv",header=TRUE,stringsAsFactors = FALSE)
View(cases)
system.time(oj<-read.csv("oj.csv"))
#OR
system.time(oj<-fread("oj.csv",header=TRUE,sep=","))#takes less time as compared to the previous one
View(oj)

#1. Find the dimensions of the oj data set
dim(oj)

#2. Find the structure of the data set
casestudy<-read.table("oj.csv",header = TRUE, sep = ",",as.is = T)
summary(casestudy)

str(casestudy)

#3. Find out the column names in the data set
colnames(oj)

#4. Update the column names of the data set, such that every column name is in
#lowercase
colnames(oj)<-tolower(colnames(oj))
colnames(oj)
View(oj)

#1. Fetch the first row 3rd column from the data set
oj[1,3]

#OR by using dplyr pakage

head(select(oj,3),1) #select for column and head is used as 1st row has to be implemented

#OR by applying pipeline

oj %>% select(3) %>% head(1)

#2. Fetch the first, second and Third columns of the oj data frame
oj[,c(1,2,3)]

#OR
oj[,1:3]

#OR
subest(oj,select=c(1,2,3)) #it subets 3 columns from the given dataset but this is not 

#OR
select(oj,1,2,3)

#OR
oj %>% select(1,2,3)

#3. Fetch the first, second, eighth and the 456th rows of the 1st, third and the sixth
#columns of the data frame
oj[c(1,2,8,456),c(1,3,6)]

#OR
select(filter(oj,row.names(oj) == 1|row.names(oj) == 2|row.names(oj) == 8|row.names(oj) == 456),1,3,6)

#OR

oj %>% filter(row.names(oj) == 1|row.names(oj) == 2|row.names(oj) == 8|row.names(oj) == 456) %>% select(1,3,6)

#4. Fetch the top 5 rows of the brand column
oj[1:5,]
#OR

head(oj$brand,5)

#OR
oj %>% filter(row.names(oj) == 1|row.names(oj) == 2|row.names(oj) == 3|row.names(oj) == 4|row.names(oj) == 5) %>% select(2) 

#OR
oj %>% select(brand) %>% head(5)

#5. Fetch all the observations for Tropicana brand
View(oj)

subset(oj,oj$brand == "tropicana")
#OR

oj[oj$brand == "tropicana",]
#do it by using which as well

#OR

oj %>% filter(brand == "tropicana")

#6. Fetch the details of all the stores

oj %>% select(store)

#7. Fetch bottom 5 observations for those who have bought Tropicana or
#dominics

oj %>% filter(brand == "tropicana"|brand == "dominics") %>% tail(5)

#OR

tail(filter(oj, brand == "tropicana" | brand == "dominics"),5)

#8. Fetch the income, brand, price observations with Tropicana brand without
#feature advertisement

oj %>% filter(brand=="tropicana" & feat == 0)  %>% select(income,brand,price)

#9. Fetch top 5 rows of the brand, week and feat details

oj %>% select(2,3,5) %>% head(5)

#10. Add a new column in the dataset: logInc which is the logarithm of the income

oj$logInc <- log(oj$income) #adding a column
dim(oj)
View(oj)

oj$logInc <- NULL #deleting a column
dim(oj)

#OR adding with a mutate 
oj <- mutate(oj, logInc = log(income))
dim(oj)

#calculate revenue for each row
(revenue <- round(exp(oj$logmove),0) * oj$price)
(mutate(oj,revenue <- round(exp(oj$logmove),0) * oj$price))#it jsut create a new data frame and does not updates therefore we need to assign it back to the oj
oj<-(mutate(oj,revenue <- round(exp(oj$logmove),0) * oj$price)) #assigning it to a data frame "oj"
oj
View(oj)

#11. Sort the Data in the increasing order of the week
order(oj$week)
oj$week[order(oj$week)]

#12. Sort the data in the decreasing order of Income
order(-oj$income)
oj$income[order(-oj$income)]

#13. Find the mean of the juice price for each brand
class(oj$brand)
aggregate(oj$price,by=list(oj$brand),mean)

#OR

oj %>% group_by(brand) %>% summarise(mean(price))


#14. Find the average income for each brand and at each store

oj %>% group_by(brand,store) %>% summarise(mean(income))

#OR
aggregate(oj$income,by=list(oj$brand,oj$store),mean)

#Find all the observations of the tropicana brand excluding store price and income
subset(oj,brand == "tropicana", select = -c(store,price,income))
#OR
oj[oj$brand == 'tropicana',-c("store","price","income")]
#OR
oj %>%  filter(brand=="tropicana") %>% select(-store,-price,-income) #run the entire code while performing above three


#Sorting
numbers<-c(10,100,5,8)
order(numbers) #it give me the positions
numbers[order(numbers)] #ascending order
numbers[order(-numbers)] #descending order
View(oj[order(oj$week),c("week","brand","price")])
View(oj[order(-oj$week),c("week","brand","price")])
arrange(oj,income)
View(arrange(oj,income))
View(oj %>% select(week,brand,income) %>% arrange(oj,income))

#data for weeks 40 and 46

View(oj %>% select(week,brand,income) %>% filter(week == 40|week == 46) %>% arrange(income))
              

#Q. For each brand find the number of observations
oj %>% group_by(brand) %>% count()


#15. Using dplyr package Find: (use function pipelines)
#a. Mean and std deviation of the income 
oj %>%  summarise(mean(income),sd(income,na.rm = FALSE))

#a1 Mean and std deviation of the income for each feature advertisement
oj %>% group_by(feat) %>%  summarise(mean(income),sd(income,na.rm = FALSE))

#b. For income greater than or equal to 10.5, find the mean income
oj %>% filter(income>=10.5) %>% summarise(mean(income))

#c. For each brand having price >=2.5 find the mean, median, sd of the log of income
oj %>% filter(price>=2.5) %>% summarise(mean(log(income)),median(log(income)),sd(log(income)))

#16. Find the Cross tabulation of brands and feature advertisement
table(oj$brand)
table(oj$brand, oj$feat)

#Find the average income for each brand
tapply(oj$price,oj$brand,mean)

#Find the average income for each brand and feature adv
tapply(oj$price,list(oj$brand,oj$feat),mean)

#Find the sum income for each brand and feature adv
tapply(oj$price,list(oj$brand,oj$feat),sum)
#OR
xtabs(oj$price~oj$brand+oj$feat) #only for addition
