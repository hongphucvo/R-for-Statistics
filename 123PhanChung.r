libary(dplyr)
libary(readxl)

#Cau1: Import data:
gianha=read.csv("gia_nha.csv", header=TRUE, sep=",")

#Cau2: Data cleaning:
    #Tao new_DF:
new_DF=data.frame(price=gianha["price"],sqft_living15=gianha["sqft_living15"],floors=gianha["floors"], condition=gianha["condition"],sqft_above=gianha["sqft_above"],sqft_living=gianha["sqft_living"] )
    #Kiem tra du lieu bi khuyet + De xuat phuong an:
new_DF=na.omit(new_DF) #Xoa hoan toan cac dong khuyet gia tri

#Cau3: Data visualization
    #a) Chuyen doi sang log:
new_DF["price"]=log(new_DF["price"])
new_DF["sqft_living15"]=log(new_DF["sqft_living15"])
new_DF["sqft_living"]=log(new_DF["sqft_living"])
new_DF["sqft_above"]=log(new_DF["sqft_above"])
    #b) Tinh cac gia tri thong ke va xuat ra dang bang:
Values = data.frame(
  INFOR = c("price","sqft_living15","sqft_above","sqft_living"),
  MEAN =c(mean(new_DF$price),mean(new_DF$sqft_living15),mean(new_DF$sqft_above),mean(new_DF$sqft_living)), 
  MEDIAN =c(median(new_DF$price),median(new_DF$sqft_living15),median(new_DF$sqft_above),median(new_DF$sqft_living)),
  SD =c(sd(new_DF$price),sd(new_DF$sqft_living15),sd(new_DF$sqft_above),sd(new_DF$sqft_living)),
  MIN =c(min(new_DF$price),min(new_DF$sqft_living15),min(new_DF$sqft_above),min(new_DF$sqft_living)),
  MAX =c(max(new_DF$price),max(new_DF$sqft_living15),max(new_DF$sqft_above),max(new_DF$sqft_living)))
Values
     #c) Thong ke so luong cho tung chung loai
table(new_DF$floors)
table(new_DF$condition)

     #d) Ve do thi phan phoi cua bien price
hist(new_DF$price,main="Do thi phan phoi cua bien price",xlab="Price")

     #e)
  #Phan phoi cua bien price cho tung nhom phan loai cua bien floors
boxplot(new_DF$price ~ new_DF$floors,
        main="different boxplots for each floors",
        xlab="floors",
        ylab="price",
        col="orange",
        border="brown")
  #Phan phoi cua bien price cho tung nhom phan loai cua bien condition
boxplot(new_DF$price~new_DF$condition,
        main="different boxplots for each condition",
        xlab="condition",
        ylab="price",
        col="orange",
        border="brown")

      #f) Phan phoi cua bien price lan luot theo cac bien sqft_living15, sqft_above va sqft_living
pairs(new_DF$price~new_DF$sqft_living+new_DF$sqft_above+new_DF$sqft_living15, col="red", pch = 18, labels = c("price", "sqft_living","sqft_above","sqft_living15"))