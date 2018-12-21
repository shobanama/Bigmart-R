
#------------------------------------------------------------------------------------------#
## Load packages
library(plyr)
library(dplyr)      
library(ggplot2)    
library(cowplot)
library(data.table)
library(stringr)


#------------------------------------------------------------------------------------------#
## Read Data

bigmart <- read.csv("BigMart.csv")



#------------------------------------------------------------------------------------------#
##Check data structure.


summary(bigmart)
glimpse(bigmart)



#------------------------------------------------------------------------------------------#
## Data Wrangling.
#Data is ploted and cleaned here.

#------------------------------------------------------------------------------------------#


#1. Item_Indentifier
#This is the unique Id for all the Products. It is a character Variable.

head((as.data.frame(table(bigmart$Item_Identifier))),n=10)


#------------------------------------------------------------------------------------------#

#2. Item_weight
#This gives the weight of all the products. We can see from the summary() that this attribute contains 2439 NA values. 

summary(bigmart$Item_Weight)

table(is.na(bigmart))



# boxplot of weights vs. Outlet Identifier

ggplot(bigmart, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Outlet identifier")


#  OUT019 and OUT027 have not provided any weight data
# assuming that each item identifier actually identifies a unique item, thus computing the mean value.

missing_index = which(is.na(bigmart$Item_Weight))
for(i in missing_index){
  item = bigmart$Item_Identifier[i]
  bigmart$Item_Weight[i] = mean(bigmart$Item_Weight[bigmart$Item_Identifier == item], na.rm = T)
  
}

#missing index are : 928,1923,4188,5023
bigmart[c(928,1923,4188,5023),]

#final check before compute plot
table(is.na(bigmart))



weightsByItem <- as.data.frame( ddply(na.omit(bigmart), ~Item_Type, summarise, mean=mean(Item_Weight), sd=sd(Item_Weight)))


bigmart$Item_Weight <- ifelse(is.na
                              (bigmart$Item_Weight), weightsByItem$mean [match(bigmart$Item_Type, weightsByItem$Item_Type)], bigmart$Item_Weight
)




# boxplot of weights vs Item type
ggplot(bigmart, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, colour = "orange")) + 
  xlab("Item Type") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Item Type")


#------------------------------------------------------------------------------------------#

#3. Item_Fat_Content
#This gives the FAT content of all the products. We can see from the table that there are 5 types of fat content in this attribute but the values lowfat n fat,now to be converted into a categorical variable to make the analysis .

table(bigmart$Item_Fat_Content)

bigmart$Item_Fat_Content <- bigmart$Item_Fat_Content %>% 
  str_replace(pattern ="LF",replacement = "Low Fat") %>%
  str_replace(pattern = "low fat",replacement = "Low Fat") %>% 
  str_replace(pattern = "reg",replacement = "Regular") %>%
  str_replace(pattern ="Low Fat",replacement = "Low_Fat")
table(bigmart$Item_Fat_Content)

bigmart$Item_Fat_Content <- as.factor(bigmart$Item_Fat_Content)

#plot item_fat_content
ggplot(bigmart) + theme_light() + geom_bar(aes(Item_Fat_Content))



#------------------------------------------------------------------------------------------#

#4. Item_Visibility
#The % of total display area of all products in a store allocated to the particular product. This is a numeric variable and we can see that the visibility of the products is distributed from 0 to 0.32. Visibility cant be zero , needs cleaning. 

summary(bigmart$Item_Visibility)

#Items_Visibility

zero_index = which(bigmart$Item_Visibility == 0)
for(i in zero_index){
  
  item = bigmart$Item_Identifier[i]
  bigmart$Item_Visibility[i] = mean(bigmart$Item_Visibility[bigmart$Item_Identifier == item], na.rm = T)
} 

#Plot for Item Visibility
ggplot(bigmart) + theme_light() +geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "green")

#------------------------------------------------------------------------------------------#

#5. Item_Type
#The category to which the product belongs. It is clear from the table that there are a total of 16 types of Items. This column is also converted into a categorical variable using as.factor() to make the analysis simpler.

table(bigmart$Item_Type)

bigmart$Item_Type <- as.factor(bigmart$Item_Type)




#------------------------------------------------------------------------------------------#

#6. Item_MRP
#Maximum Retail Price (list price) of the product. This is a numeric variable. It can be seen from the summary of the attribute that the range of the Maximum Retail Price is from 31.29 to 266.89. Further Analysis is required to check if this attribute can be broken down into Intervals.

summary(bigmart$Item_MRP)

#Plot for Item MRP
ggplot(bigmart) + theme_light() +geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")


#------------------------------------------------------------------------------------------#


#7. Outlet_Identifier
#This gives the Unique Store Id. This is character Variable.

head((as.data.frame(table(bigmart$Outlet_Identifier))),n=10)


#------------------------------------------------------------------------------------------#

#8. Outlet_Establishment_Year
#This tells the year in which store was established. It can be seen from the table that all the observations have 9 Unique years in the column. So, this column can also be converted into a categorical variable.

table(bigmart$Outlet_Establishment_Year)

bigmart$Outlet_Establishment_Year <- as.factor(bigmart$Outlet_Establishment_Year)


#------------------------------------------------------------------------------------------#

#9. Outlet_Size
#The size of the store in terms of ground area covered. It can be seen from the table that the column has 3 types of size and 4016 NA values. All the NA values are replaced with unknown and this column is also convereted into a categorical variable with 4 levels.

table(bigmart$Outlet_Size)
bigmart$Outlet_Size <- as.factor(bigmart$Outlet_Size)

#plot to replace NA in Outlet Size to small
p1 = ggplot(bigmart) + geom_boxplot(aes(Outlet_Size, Item_Outlet_Sales), fill = "orange")
p2 = ggplot(bigmart) + geom_boxplot(aes(Outlet_Size, Outlet_Type))
plot_grid(p1, p2, nrow = 1)

bigmart$Outlet_Size[(bigmart$Outlet_Size == "")] <- "Small"
ggplot(bigmart) + geom_boxplot(aes(Outlet_Size, Item_Outlet_Sales), fill = "pink")




#------------------------------------------------------------------------------------------#

#10. Outlet_Location_Type
#This tells the type of city in which the store is located. There are only 3 Unique Tiers in this attribute. So, this column can also be converted into a factor using as.factor()

table(bigmart$Outlet_Location_Type)

bigmart$Outlet_Location_Type <- as.factor(bigmart$Outlet_Location_Type)

#------------------------------------------------------------------------------------------#

#11. Outlet_Type
#The Outlet_Type tells us Whether the outlet is just a grocery store or some sort of supermarket so converted into a categorical variable.

table(bigmart$Outlet_Type)

bigmart$Outlet_Type <- as.factor(bigmart$Outlet_Type)


#------------------------------------------------------------------------------------------#

#12. Item_Outlet_Sales
#It can be seen from the summary call that the Sales in the train dataset ranges from 33.29 to 13086.97.

summary(bigmart$Item_Outlet_Sales)

#Plot for Item Outlet Sales
ggplot(bigmart) + theme_light()+ geom_histogram(aes(Item_Outlet_Sales),binwidth = 100, fill = "blue")

#------------------------------------------------------------------------------------------#
#Feature Engineering..
#Data is not sufficient thus defining couple of new data.

#profit - total revenue
#profitmargin - percentage revenue

#------------------------------------------------------------------------------------------#


bigmart$Profit <- bigmart$Item_Outlet_Sales - bigmart$Item_MRP

bigmart$Profit_Margin <- round((bigmart$Profit / bigmart$Item_Outlet_Sales)*100, digits = 3)



##Computing Profit_Margin vs Size of an Outlet vs Outlet location vs Outlet Type


# boxplot of  Sales vs. Outlet location
ggplot(bigmart, aes(x = Outlet_Location_Type, y = Profit, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet location") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet location")



# boxplot of  Sales vs. Outlet type
ggplot(bigmart, aes(x = Outlet_Type, y = Profit, fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet type")



# boxplot of  Sales vs. Item type vs Outlet Size
ggplot(bigmart, aes(x = Item_Type, y = Profit , fill = Outlet_Size)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type vs Outlet Size")



# boxplot of  Sales vs. Item type
ggplot(bigmart, aes(x = Item_Type, y = Profit, fill = Outlet_Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type vs Outlet Type")



# boxplot of Sales vs Outlet Established Year
ggplot(bigmart, aes(x = Outlet_Establishment_Year, y = Item_Outlet_Sales, fill = Outlet_Type )) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab(" Established Year") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet Established Year")



#Clearly shown that the profits of Grocery stores are lest then other supermarkets.
#Computing the Profit Margin of the Outlets.- clearly grocery stores has much loss.

ggplot(bigmart, aes(x = Item_Type, y = Profit, fill = (Outlet_Type== "Grocery Store"))) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Items") + 
  ylab("Sales") + 
  ggtitle("Grocery Store Sales")



##clear profit plot vs manufacturing price

ggplot(bigmart, aes(Item_MRP, Profit)) +
  geom_point(size = 2.5, aes(colour = factor(Outlet_Type))) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item MRP") + 
  ylab("Profit") +
  ggtitle("Profit vs MRP")


##count the number per Outlet_Identifier and Outlet_Type to do checking on location



OutletDetails <- as.data.frame( setNames(
  aggregate(
    bigmart$Outlet_Size, 
    by=list(Category=bigmart$Outlet_Identifier, 
            Category=bigmart$Outlet_Type,
            Category=bigmart$Outlet_Location_Type,
            Category=bigmart$Outlet_Size), 
    FUN= length),
  c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "Number")
))
OutletDetails







par(mfrow=c(2,2))
boxplot(bigmart$Item_Visibility,horizontal = TRUE,main="Item Visibility",col="Blue")
boxplot(bigmart$Item_Weight,horizontal = TRUE,main="Item Weight",col="Blue")
boxplot(bigmart$Item_MRP,horizontal = TRUE,main="Item MRP",col="Blue")
boxplot(bigmart$Item_Outlet_Sales,horizontal = TRUE,main="Item Output Sales",col="Blue")





par(mfrow=c(1,3))
plot(x=bigmart$Item_MRP,y=bigmart$Item_Outlet_Sales  ,col=c("Red","Yellow"),xlab="MRP",ylab="Outlet Sales")
plot(y=bigmart$Item_Outlet_Sales,x=bigmart$Item_Weight,col=c("Red","Yellow"),xlab="Item Weight",ylab="Output Sales")
plot(x=bigmart$Item_Visibility ,y=bigmart$Item_Outlet_Sales,col=c("Red","Yellow"),xlab="Item Visibility",ylab="Output Sales")






par(mfrow=c(2,2))
hist(bigmart$Item_Visibility,main="Item Visibility",col="Blue",xlab = "Visiblity")
hist(bigmart$Item_Weight,main="Item Weight",col="Blue",xlab = "Weight")
hist(bigmart$Item_MRP,main="Item MRP",col="Blue",xlab = "MRP")
hist(bigmart$Item_Outlet_Sales,main="Item Output Sales",col="Blue",xlab = "Outlet Sales")



write.csv(bigmart, file = "D:/RStudio/WQD7009/bigmartnew.csv")


#-----------------------------------------------------------------------------------------------------#


bigmartnew <- read.csv("bigmartnew.csv")
glimpse(bigmartnew)


mydf = bigmartnew %>% select("Item_Visibility", "Profit_Margin") %>% sample_frac(0.1) 


linearModel <- lm(Item_Visibility ~ Profit_Margin, data=mydf)
summary(linearModel)

plot(linearModel)
abline(linearModel, col = 'Red')

names(bigmartnew)












