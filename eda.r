#install and load the packages
install.packages("data.table")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
install.packages("xgboost")
install.packages("cowplot")
install.packages("caret")

library("data.table")
library("ggplot2")
library("dplyr")
library("corrplot")
library("xgboost")
library("cowplot")
library("caret")

#read the train and test datasets
train<- fread(train)
test<- fread(test)

#column names of train and test dataset
names(train)
names(test)

#structure of the train and test datasets
str(train)
str(test)

test[ , Item_Outlet_Sales:=NA]  #adding Item_Output_Sales column in test dataset and assigned NA value
View(test)  #viewing the test dataset

full<- rbind(train,test)

ggplot(train)+geom_histogram(aes(train$Item_Outlet_Sales),bindwidth=50,fill="blue")+xlab(Item_Outlet_Sales)
p1<- ggplot(full)+geom_histogram(aes(Item_Weight),binwidth=0.5,fill="green")
p2<- ggplot(full)+geom_histogram(aes(Item_Visibility),binwidth=0.005,fill="green")
p3<- ggplot(full)+geom_histogram(aes(Item_MRP),binwidth=1,fill="green")


plot_grid(p1,p2,p3,nrow=1)

ggplot(full%>%group_by(Item_Fat_Content)%>%summarise(count=n()))+geom_bar(aes(Item_Fat_Content,count),stat="identity",fill="orange")
full$Item_Fat_Content[full$Item_Fat_Content=="LF"]= "Low Fat"
full$Item_Fat_Content[full$Item_Fat_Content=="low fat"]= "Low Fat"
full$Item_Fat_Content[full$Item_Fat_Content=="reg"]= "Regular"
ggplot(full%>%group_by(Item_Fat_Content)%>%summarise(count=n()))+geom_bar(aes(Item_Fat_Content,count),stat="identity",fill="orange")


p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")
  p4
  
  
  
  
  # plot for Outlet_Identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p5



# plot for Outlet_Size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6
plot_grid(p4,p5,p6.ncol=1)



# plot for Outlet_Establishment_Year
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  xlab("Outlet_Establishment_Year") +
  theme(axis.text.x = element_text(size = 8.5))

p7

# plot for Outlet_Type
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))

p8

# ploting both plots together
plot_grid(p7, p8, ncol = 2)















