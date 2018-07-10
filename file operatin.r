setwd("C:\\Users\\Administrator\\Desktop\\R PROGRAMMING\\Datasets")

x<-scan(what="")
a
b
c
d

x<-scan(what = logical())
TRUE
FALSE
TRUE
TRUE

plaintext<-scan("PlainText.txt", what = "", sep = "")
plaintext<-scan("PlainText.txt", what = "", sep = "a")

customers<-scan("customers.txt", what = "", sep = "\t")
customers<-scan("customers.txt", what = "", sep = "\t", skip = 1)

customers<-scan("customers.csv", what = "", sep = ",", skip = 1)

customers<-scan("customers.txt",
                what = list(firts_name="",last_name="",city="", county="",state="",zip=0),
                sep = "\t", skip = 1)

customers.df<-as.data.frame(customers)
customers.df

customers.df<-read.table("customers.txt",header =TRUE, sep="\t")
customers.df
View(customers.df)

customers.df<-read.table("customers.csv",header =TRUE, sep=",")
customers.df

class(customers.df$first_name)
class(customers.df$zip)

customers.df<-read.table("customers.csv",header =TRUE, sep=",",stringsAsFactors = FALSE)
customers.df
class(customers.df$zip)
class(customers.df$first_name)

customers<-read.table("customers.csv",header =TRUE, sep=",",as.is = T)
summary(customers)
str(customers)

customers<-read.csv("customers.csv",header =TRUE, stringsAsFactors = FALSE)
customers

customers<-read.csv("customers_missing.csv",header =TRUE, stringsAsFactors = FALSE,na.strings=c("","NA","-"))
View(customers)  

customers<-read.csv("customers_missing.csv",header=F,stringsAsFactors =F,na.strings = c("","NA","-"))
customers

names(customers)<-c("first_name","last_name","city","country","state","zipcode")
View(customers)

customers<-read.csv("customers_missing.csv",header=F,
                    col.names= c("first_name","last_name","city","country","state","zipcode"),
                    stringsAsFactors =F,na.strings = c("","NA","-"))
View(customers)


customers<-read.csv("customers_missing.csv",header=F,
                    col.names= c("first_name","last_name","city","country","state","zipcode"),
                    stringsAsFactors =F,na.strings = c("","NA","-"),nrows = 100)
View(customers)


customers<-read.csv("customers_missing.csv",header=F,
                    col.names= c("first_name","last_name","city","country","state","zipcode"),
                    stringsAsFactors =F,na.strings = c("","NA","-"),skip = 100)
View(customers)

cat(customers$first_name, sep=",")

#saving output to external file

cat(customers$first_name,
    file = "customers_names.txt",
    sep = "\n")   #

write.csv(customers<"customers_missing_new.csv",    #we have created a new file
          row.names = FALSE)     #as we are not intrested to find the number of rows
      
      
 # loading Excel Workbook

wb<-loadWorkbook("customers.xlsx")
wb     
      
      
# reading "newyork" sheet from workbook connection

newyork<-readWorksheet(wb,"newyork",header=T)
newyork
      

#readind california sheet from workbook connection

california<-readWorksheet(wb,"california",header=T)
california   

#using .xls excel file

newyork<-readWorksheetFromFile("customers.xls","newyork",header=T)
newyork 







































library(XLConnect)  

  install.packages("XLConnect")
    