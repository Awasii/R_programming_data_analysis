winedeeds<-read.csv("C:\\Users\\abdul\\Downloads\\BurgundySip (1).csv");
winedeeds;

# Ran the summary and structure function to figure out the data present in the table.
str(winedeeds);
summary(winedeeds);
View(winedeeds);
# removing DN values seems like a possible option because its values are almost 1.
length(winedeeds);
# Had to see had the last index number is in the data.frame
winedeeds<-winedeeds[,(-14)];
winedeeds;
====
#Best time to now analyze the data for its structure | for independant and dependant variables 
#'data.frame':	7500 obs. of  13 variables:
#$ SN  : chr  "W4339-20718" "W9737-31436" "W8398-63402" "W4418-44312" ... | INDE
#$ NAME: chr  "A Coroa" "Aalto" "Aalto" "Aalto" ... | INDE
#$ WINE: chr  "200 Cestos Godello" "Blanco de Parcela" "PS (Pagos Seleccionados) Ribera del Duero" "PS (Pagos Seleccionados) Ribera del Duero" ...| INDE
#$ YR  : chr  "2020" "2019" "2011" "2015" ...
#$ REG : chr  "Valdeorras" "Ribera del Duero" "Ribera del Duero" "Ribera del Duero" ...
#$ TP  : chr  NA "Verdejo" "Ribera Del Duero Red" "Ribera Del Duero Red" ...
#$ RT  : num  4.06 4.21 4.31 4.43 4.32 4.37 4.34 4.32 4.09 4.31 ...
#$ NUMR: int  33 80 2207 2858 4411 3383 3239 1108 2844 1884 ...
#$ PR  : num  23.7 41.9 64 172.5 78.7 ...
#$ BD  : int  NA NA 5 5 5 5 5 5 NA 5 ...| DEP 
#$ ACD : int  NA NA 3 3 3 3 3 3 NA 3 ...| DEP
#$ RSG : chr  " 9.9868 " " 8.9416 " " 8.2189 " " 6.9998 " ... - | DEP
#$ AL  : chr  " 11.1057 " " 11.6007 " " 11.9328 " " 11.4113 " ...| DEP

# noticing rsg and al values are in character form, better to convert them. They also have spaces, must be one of the reasons its showing up as a character.

winedeeds$RSG <-gsub(" ","",winedeeds$RSG);
winedeeds$RSG <- as.numeric(winedeeds$RSG);
winedeeds$AL <-gsub(" ","",winedeeds$AL);
winedeeds$AL <- as.numeric(winedeeds$AL);
str(winedeeds)
winedeeds$YR<- as.numeric(winedeeds$YR); 
#mode(winedeeds)- without using the $ sign caused the whole data frame to change 
#class(winedeeds)

# after adjsuting the data frame to our needs , we got to factorize the char , which instead of factorzeing the whole data set, we can use the lapply function instead 
# of stringAsfactor. We use lapply becasue we are treating each individual box so you need to use a listed based function. 
winedeeds[,c(1,2,3,5,6)]<- lapply(winedeeds[,c(1,2,3,5,6)], factor);   
winedeeds;
wine


# To look for duplicates you need to figure out the primary key, and according to that you remove the duplicates. So our assumption tells us that two serial numbers would
# not be the same. 

anyDuplicated(winedeeds$SN); #The index of the first number is 215
duplicated(winedeeds$SN); # This returns a logical vector for where the duplicates are present.
duplicated(winedeeds$SN, fromLast = T)
# To see both top to bottom and bottom to top toget

dups <-duplicated(subset(winedeeds ,select= "SN" )) | duplicated(winedeeds$SN, fromLast = T);
dups;

winedeeds[dups,]
#Since the first missing values have na values , i would be going from bottom to top methord 

winedeeds <- winedeeds[!duplicated(winedeeds$SN, fromLast = T  ),];winedeeds; #Clean Dataset
View(winedeeds)
summary(winedeeds)


#Time to treat missing values for the independant var, for the missingness
#is.na is used to find the count of missing vlaues a varibale in this case 
sum(is.na(winedeeds$SN))/length(winedeeds$SN)

missingprob<- function(x){
  
  return(sum(is.na(x))/length(x)*100);
  
  
}

apply(winedeeds,2 , missingprob); 

var(winedeeds)

# what to check the clear dataset wihout na values as a percentage by na values 
na.omit(winedeeds)
nrow(na.omit(winedeeds))
(nrow(na.omit(winedeeds))/nrow(winedeeds))*100
(nrow(winedeeds))-((nrow(na.omit(winedeeds))/nrow(winedeeds))*100)                      

#calculating cor with factors and numeric values, got to make the data set numeric first 

winedeeds2 <-na.omit(winedeeds);winedeeds2;
is.na(winedeeds2)
sum(is.na(winedeeds2))
winedeeds2<- lapply(na.omit(winedeeds), as.numeric)                     
winedeeds2;                            
summary(winedeeds2)                   
str(winedeeds2)
str(winedeeds)
winedeeds2<- as.data.frame(lapply(na.omit(winedeeds), as.numeric))  
winedeeds2;

round(cor(winedeeds2),2)

install.packages("ggcorrplot");
library(ggplot2);
library(ggcorrplot);
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics) 

summary(winedeeds)

cor(winedeeds2)

winedeeds$YR
winedeeds<- winedeeds[!(is.na(winedeeds$SN)),]    
YR_RT<-round(aggregate(YR~RT,data= winedeeds, FUN= mean, na.rm = T),);
YR_RT;

winedeed3 <-merge(winedeeds,YR_RT, by= "RT");
winedeed3;

winedeed3[is.na(winedeeds$YR.x),"YR.x"]<- winedeed3[is.na(winedeeds$YR.y),"YR.y"];
View(winedeed3)
