#Predicting the age of abalone (rings+1.5)based on its sex,Length,Diameter,Height,Whole weight
#and Shucked weight ,Viscera weight,Shell weight
#will use Linear Regression 
ablon <- as.data.frame(read.csv("C:/Users/ABDELRAHMAN ARAFA/Desktop/abalone.data.csv"),sep=",")
colnames(ablon)[1]="sex" ;colnames(ablon)[2]="Length"  ;colnames(ablon)[3]="Diameter" 
colnames(ablon)[4]="Height" ;colnames(ablon)[5]="Whole_weight"  ;colnames(ablon)[6]="Shucked_weight" 
colnames(ablon)[7]="Viscera_weight" ;colnames(ablon)[8]="Shell_weight"  ;colnames(ablon)[9]="Rings"   

#there is no missing valuse ... but to be sure 
ablon=na.omit(ablon)

summary(ablon)
head(ablon)

#now Data is ready to use 

X_data = ablon[,0:8]
Y_data = ablon[9]

#Scatterplot Matrix
pairs(~Length + Diameter + Height + Whole_weight+ Shucked_weight + Viscera_weight + Shell_weight + Rings,data = ablon)

#Scatterplot , Relatioon between each x and Rings
for(i in 2:9)
  plot(X_data[1:30,i],Y_data[1:30,],ylab="Rings",xlab=colnames(X_data)[i])


#lets know about lm then use it

?lm   # help(lm)  # will use ols 

#is there is any feature can ignore it to save time ?
output <- lm(Rings ~ sex +Length + Diameter + Height + Whole_weight+ Shucked_weight + Viscera_weight + Shell_weight ,ablon)
#sumary will define the residuals 
#Residual is the diff between Prediction and actual results 
#the less Residual value the best Feature used to prediction model
#if p-value >0.05 then model is doing nothing
#if pr(>|t|) is closely to 0 then this feature is not significatn and u can ignore it 

summary(output)
#Residual standard error: 2.193
#p-value: < 2.2e-16
#we note that  pr(>|t|) for sex and Length Feature is closly to 0
#lets try to ignore both of them and check the residual

outputLM <- lm(Rings ~  Diameter + Height + Whole_weight+ Shucked_weight + Viscera_weight + Shell_weight ,ablon)
summary(outputLM)
#Residual standard error: 2.216 
#p-value: < 2.2e-16
#the residual error increase ,,but with small value and u savr time because of u decrease number of fearure where the algorithm will save time 

#start Prediction 
Diameter= 0.265 ; Height= 0.090;  Whole_weight= 0.2255 ; Shucked_weight= 0.0995  ; Viscera_weight = 0.0485   ; Shell_weight =0.070
testpoint <- data.frame(  Diameter ,Height , Whole_weight, Shucked_weight , Viscera_weight , Shell_weight )
y_test <- predict(outputLM,testpoint)
Age_of_ablon <- y_test+1.5



# Draw  Line 
with(outputLM , {plot(fitted.values,residuals)
  points(c(min(fitted.values,residuals),max(fitted.values,residuals)),type="l" )  })

with(output , {plot(fitted.values,residuals)
  points(c(min(fitted.values),max(fitted.values)),type="l" )  })