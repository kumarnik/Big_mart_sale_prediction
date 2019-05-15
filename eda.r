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




## EDA - Bivariate

train = combi[1:nrow(train)]

# Item_Weight vs Item_Outlet_Sales
p9 = ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "green", alpha = 0.3) +
     theme(axis.title = element_text(size = 10))

p9

# Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(train) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "green", alpha = 0.3) +
      theme(axis.title = element_text(size = 10))

p10

# Item_MRP vs Item_Outlet_Sales
p11 = ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "green", alpha = 0.3) +
      theme(axis.title = element_text(size = 10))

p11

second_row_2 = plot_grid(p10, p11, ncol = 2)

plot_grid(p9, second_row_2, nrow = 2)





# Item_Type vs Item_Outlet_Sales
p12 = ggplot(train) + geom_boxplot(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
p12

p12 = ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
p12

# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train) + geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
p13

# Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(train) + geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
p14
second_row_3 = plot_grid(p13, p14, ncol = 2)

plot_grid(p12, second_row_3, ncol = 1)


ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")

p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")

plot_grid(p15, p16, ncol = 1)




## Missing Value Treatment

missing_index = which(is.na(full$Item_Weight))
for(i in missing_index){
  
  item = full$Item_Identifier[i]
  full$Item_Weight[i] = mean(full$Item_Weight[full$Item_Identifier == item], na.rm = T)
  
}

# replacing 0 in Item_Visibility with mean
zero_index = which(full$Item_Visibility == 0)
for(i in zero_index){
  
  item = full$Item_Identifier[i]
  full$Item_Visibility[i] = mean(full$Item_Visibility[full$Item_Identifier == item], na.rm = T)
  
}
#-------------------------------------------------------------------------------------------------------------------------
## Feature Engineering

# create a new feature 'Item_Type_new' 
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene",
                   "Household", "Soft Drinks")

full[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable",
                               ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]


full[,Item_category := substr(combi$Item_Identifier, 1, 2)]

full$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"

# years of operation of outlets
full[,Outlet_Years := 2013 - Outlet_Establishment_Year]
full$Outlet_Establishment_Year = as.factor(full$Outlet_Establishment_Year)

# Price per unit weight
full[,price_per_unit_wt := Item_MRP/Item_Weight]


ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3)

# creating new independent variable - Item_MRP_clusters
Item_MRP_clusters = kmeans(full$Item_MRP, centers = 4)
table(Item_MRP_clusters$cluster) # display no. of observations in each cluster

full$Item_MRP_clusters = as.factor(Item_MRP_clusters$cluster)

#or group them manually
# combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st", 
#                                    ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
#                                           ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]















