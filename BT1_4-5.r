setwd("D://BTLXSTK")
#1#############
Data=read.csv("gia_nha.csv") # doc file .csv
#2a#############
new_DF=data.frame(price=Data$price,
                   sqft_living15=Data$sqft_living15 ,
                   floors=Data$floors ,
                   condition=Data$condition ,
                   sqft_above =Data$sqft_above,
                   sqft_living=Data$sqft_living)
#2b#############
colSums(is.na(new_DF))
new_DF$price[which(is.na(new_DF$price))]=mean(new_DF$price,na.rm = TRUE)
new_DF$price=round(new_DF$price)
colSums(is.na(new_DF))
#3a#############
new_DF$price=log(new_DF$price)
new_DF$sqft_living15=log(new_DF$sqft_living15)
new_DF$sqft_above=log(new_DF$sqft_above)
new_DF$sqft_living=log(new_DF$sqft_living)
#3b#############
bienlientuc<-function(x){
  trungBinh=mean(x)
  trungVi=median(x)
  doLech=sd(x)
  gtNN=min(x)
  gtLN=max(x)
  
  c(trungBinh=trungBinh, trungVi=trungVi,doLech=doLech,gtNN=gtNN,gtLN=gtLN)
}

Price=bienlientuc(new_DF$price)
Sqft_living15=bienlientuc(new_DF$sqft_living15)
Sqft_above=bienlientuc(new_DF$sqft_above)
Sqft_living=bienlientuc(new_DF$sqft_living)

thongKe=data.frame(Price,Sqft_living15,Sqft_above,Sqft_living)
#3c#############
floorsSL=table(new_DF$floors)
floorsSL=as.data.frame(floorsSL)
floorsSL=setnames(floorsSL,c("Var1","Freq"),c("loai","soLuong"))

conditionSL=table(new_DF$condition)
conditionSL=as.data.frame(conditionSL)
conditionSL=setnames(conditionSL,c("Var1","Freq"),c("loai","soLuong"))
#3d#############
hist(new_DF$price,main="Bieu do tan so",xlab="log(price)",ylab="So luong",
     xlim=c(10,17),ylim = c(0,2000),breaks=40)
 
#3e#############
ggboxplot(new_DF, x = "floors", y = "price", 
          color = "floors", 
          palette = c("blue", "green", "red","brown","orange","violet"),
          order = c("1","1.5", "2","2.5", "3","3.5"),
          ylab = "log(price)", xlab = "Loai floor")

ggboxplot(new_DF, x = "condition", y = "price", 
          color = "condition", 
          palette = c("blue", "green", "red","brown","orange"),
          order = c("1", "2", "3","4","5"),
          ylab = "log(price)", xlab = "condition")

#3f#############
pairs(new_DF[ , 1:2],
      col = "brown", 
      pch = 1,                                             
      labels = c("price", "sqft_living15"),                
      main = "Phan phoi")       

pairs(~price + sqft_above, data=new_DF,
      col = "brown",                                         
      pch = 18,                                             
      labels = c("price", "sqft_above"),                
      main = "Phan phoi")  

pairs(~price + sqft_living,data=new_DF,
      col = "brown",                                          
      pch = 18,                                             
      labels = c("price", "sqft_living"),                
      main = "Phan phoi") 

#4a##############
hoiqui1=lm(price ~. , data=new_DF )
summary(hoiqui1)        
#4b############## ai lam latex nhan xet luon nhe
# t0.05=2,365> -12,72 -> loai bien sqft_above 
#4c##############
hoiqui2=lm(price ~ sqft_living15 + sqft_above 
          + sqft_living + floors, data=new_DF )
summary(hoiqui2) 

 
CONDITION=as.factor(new_DF$condition)
PRICE=new_DF$price
anova1=data.frame(CONDITION,PRICE)
price.anova1=anova(lm(PRICE~CONDITION,data=anova1 ))
#p-value=3.168169e-64 < alpha=0,05 -> bac bo H0 -> mo hinh 1 hop ly hon
#4d############## ai lam latex nhan xet luon nhe
#4e##############
#https://www.statology.org/residual-plot-r/
resid(hoiqui1)
fitted(hoiqui1) 
op <- par(mfrow=c(2,2)) #yêu c???u R dành ra 4 c???a s???
plot(hoiqui1) #v??? các d??? th??? trong reg
#5a############## ai lam latex nhan xet luon nhe
predict(hoiqui1, 
        data.frame(sqft_living15 = mean(new_DF$sqft_living15), 
                   sqft_above = mean(new_DF$sqft_above), 
                   sqft_living= mean(new_DF$sqft_living), 
                   floors = c(2), condition = 3))
predict(hoiqui1, 
        data.frame(sqft_living15 = max(new_DF$sqft_living15), 
                   sqft_above = max(new_DF$sqft_above), 
                   sqft_living= max(new_DF$sqft_living), 
                   floors = c(2), condition = 3))
