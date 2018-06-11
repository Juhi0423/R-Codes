library(dplyr)
library(data.table)
library(tidyr)
edu <- fread("E:/Term 2/Data Visualization/Data Set/rajanand-crime-in-india/18_01_Juveniles_arrested_Education.csv")
eco <- fread("E:/Term 2/Data Visualization/Data Set/rajanand-crime-in-india/18_02_Juveniles_arrested_Economic_setup.csv")
fa <- fread("E:/Term 2/Data Visualization/Data Set/rajanand-crime-in-india/18_03_Juveniles_arrested_Family_background.csv")

## Yearwise 
#Education
Education <- edu%>%group_by(Year)%>%summarise(Education_Above_Primary_but_below_Matric_or_Higher_Secondary=sum(Education_Above_Primary_but_below_Matric_or_Higher_Secondary),
                                         Illiterate=sum(Education_Illiterate),
                                         `Education_Matric_or_Higher_Secondary_&_above`=sum(`Education_Matric_or_Higher_Secondary_&_above`),
                                         Education_Upto_primary=sum(Education_Upto_primary))
values <- gather(year,Group,value,-Year)
plot <- ggplot(values,aes(x=factor(Year),y=value)) + geom_bar(stat = "identity", position = "dodge",aes(fill=Group)) +
  labs(x="Year",y="Number of Incidents",title="Juveniles Crimes based on Education(Yearwise)") 
pl <- plot + theme(legend.title=element_blank(),legend.position = "bottom",legend.key.width = unit(.2, "cm")) 

#Family Background
bd_year <- fa %>% group_by(Year) %>% summarise(Family_back_ground_Homeless = sum(Family_back_ground_Homeless),Family_back_ground_Living_with_guardian = sum(Family_back_ground_Living_with_guardian),Family_back_ground_Living_with_parents = sum(Family_back_ground_Living_with_parents))

fbd_year <- gather(fbd_year,group,count,-Year)

p <- ggplot(fbd_year,aes(x = factor(Year),y = count)) + geom_bar(stat = "identity",position = "dodge",aes(fill = group))

p1 <- p + labs(x = "Year",y = "Number of Incident",subtitle = "Juveniles Crimes based on Family Background(Yearwise)",fill = "Family Background")

p2 <- p1 + theme(legend.position = "bottom")

#Economic 
economic2<- eco[,c(2,4,5,6,7,9,10)]
economic3<- gather(economic2,Economic_setup,Juvenile_No.,-Year)

P4<- ggplot(economic3,aes(x=factor(Year),y= Juvenile_No.,fill=Economic_setup))+
  geom_bar(stat="Identity",width = 1.5,position = "dodge")+theme(axis.text.x = element_text(angle = 90)) + labs(title="Juveniles Crimes based on Economic Background (Yearwise)")
P4

## Area Wise
#Family
crime2<- fa[,c(1,4,5,6)]
crime3<- gather(crime2,Background,Juvenile_No.,-Area_Name)


P1<- ggplot(crime3,aes(x=Area_Name, y= Juvenile_No.,fill=Background))+ geom_bar(stat="Identity",width = 0.5)
P2<- P1+ theme(axis.text.x = element_text(angle = 90)) + coord_flip()

#Education
edu2<- edu[,c(1,4,5,6,8)]
edu3<- gather(edu2,Education_level,Juvenile_count,-Area_Name)

P3<- ggplot(edu3,aes(x=Area_Name,y=Juvenile_count,fill=Education_level))+
  geom_bar(stat="Identity",width = 0.5)+theme(axis.text.x = element_text(angle = 90)) + coord_flip()

#Economic 
est_area <- eco %>% group_by(Area_Name) %>% summarise(
  Low = sum(Economic_Set_up_Annual_Income_upto_Rs_25000),
  Above.Low = sum(Economic_Set_up_Annual_Income_250001_to_50000),
  Middle = sum(Economic_Set_up_Middle_income_from_50001_to_100000),
  Above.Middle = sum(Economic_Set_up_Middle_income_from_100001_to_200000),
  High = sum(Economic_Set_up_Upper_middle_income_from_200001_to_300000),
  Above.High = sum(Economic_Set_up_Upper_income_above_Rs_300000)
)

est_area <- gather(est_area,Family_Group,Income,-Area_Name)

est_area <- est_area %>% group_by(Area_Name,Family_Group) %>% summarise(Income = sum(Income)) %>% arrange(-Income)

p <- ggplot(est_area,aes(x = reorder(Area_Name,Income), y = Income))

p1 <- p + geom_bar(stat = "identity",aes(fill = Family_Group)) + coord_flip()

p2 <- p1 + labs(x = "Sum of Incomes",y = "Area_Name",subtitle = "Economic Status Year Wise")

