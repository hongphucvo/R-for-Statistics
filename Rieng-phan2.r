library(readr)
library(dplyr)
library(readxl)
library(moments)
library(utf8)
library(stringr)

set_col_name <- function(df){
  colnames(df)[1] <- "Time"
  colnames(df)[7] <- "Freq"
  colnames(df)[8] <- "Proper_Price"
  colnames(df)[9] <- "Time_saving"
  colnames(df)[10] <- "Da_dang"
  colnames(df)[11] <- "Khuyen_Mai"
  colnames(df)[12] <- "Thiet_bi_di_dong"
  colnames(df)[13] <- "Web"
  colnames(df)[14] <- "Social_network"
  colnames(df)[15] <- "Thiet_bi_dien_tu"
  colnames(df)[16] <- "Quan_tam_chat_luong"
  colnames(df)[17] <- "Quan_tam_thanh_toan"
  colnames(df)[18] <- "Quan_tam_thuong_hieu"
  colnames(df)[19] <- "Quan_tam_gia_ca"
  colnames(df)[20] <- "Payment"
  colnames(df)[21] <- "Su_dung_tiep"
  colnames(df)[22] <- "Giao_dien"
  colnames(df)[23] <- "Nhu_cau"
  colnames(df)[24] <- "Chat_luong"
  colnames(df)[25] <- "Dich_vu"
  colnames(df)[26] <- "De_dang"
  colnames(df)[27] <- "Anh_huong_gia_ca"
  colnames(df)[28] <- "Nguon_goc"
  colnames(df)[29] <- "Vuot_troi_hon"
  colnames(df)[30] <- "courier_time"
  colnames(df)[31] <- "Uy_tin"
  colnames(df)[32] <- "Marketing"
  colnames(df)[33] <- "Sale"
  colnames(df)[34] <- "Danh_gia_dich_vu"
  colnames(df)[35] <- "Sinh_vien_nam"
  colnames(df)[36] <- "Gender"
  colnames(df)[37] <- "Chi_tieu"
  return(df)
}
Excel <- read_excel("Input.xlsx")
Excel = set_col_name((Excel))

## Loai bo cac gia tri na ra khoi map

data <- cbind( Excel[][2], Excel[][8:11], Excel[][21:34])

for(i in 1:19) {
  data <- subset(data, !is.na(data[][i]))
}
data$Su_dung_tiep[data$Shopee == 0] = 0
data$Su_dung_tiep[data$Su_dung_tiep == 'Có'] = 1
data$Su_dung_tiep[data$Su_dung_tiep == 'Không'] = -1
data$Su_dung_tiep[data$Su_dung_tiep == "Sẽ xem xét"] = 0
data$Su_dung_tiep = as.numeric(data$Su_dung_tiep)

temp <- data[][7]
colnames(temp)[1] = "Tong_diem"
for(i in 8:19) {
  temp <- temp[][1] + data[][i]
}
data <- cbind(data, temp)
View(data)


#Lam ro du lieu

Average = apply(data[7:20], 2, mean)
Median= apply(data[7:20], 2, median)
SD = apply(data[7:20], 2, sd)
Max = apply(data[7:20], 2, max)
Min = apply(data[7:20], 2, min)

statistical_table1 <- cbind(Average, Median)
statistical_table1 <- cbind(statistical_table1, SD)
statistical_table1 <- cbind(statistical_table1, Max)
statistical_table1 <- cbind(statistical_table1, Min)
View(statistical_table1)


statistical_table2 <- apply(data[][1:6], 2, table)
View(statistical_table2)


h = hist(data$Su_dung_tiep)
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE)



boxplot(data$Tong_diem[data$Su_dung_tiep == 1])

boxplot(data$Tong_diem[data$Su_dung_tiep == 0])

boxplot(data$Tong_diem[data$Su_dung_tiep == -1])

#################################################
#BAt DAU PHAN 2 PHAN RIENG (DUA THEO BAI 1 PHAN CHUNG)
M0<-lm(data$Su_dung_tiep~data$Giao_dien+data$Nhu_cau+data$Chat_luong+data$Dich_vu+data$De_dang+data$Anh_huong_gia_ca+data$Nguon_goc+data$Vuot_troi_hon+data$courier_time+data$Uy_tin+data$Marketing+data$Sale+data$Danh_gia_dich_vu)
summary(M0)
M1<-lm(data$Su_dung_tiep~data$Chat_luong+data$De_dang+data$Anh_huong_gia_ca+data$Nguon_goc+data$courier_time+data$Marketing+data$Danh_gia_dich_vu)
summary(M1)
#M2 loai danh gia dich vu
M2<-lm(data$Su_dung_tiep~data$Chat_luong+data$De_dang+data$Anh_huong_gia_ca+data$Nguon_goc+data$courier_time+data$Marketing)
summary(M2)
Anova1<-anova(M1)
summary(Anova1)
Anova2<-anova(M2)
summary(Anova2)
#2 ket qua tuong duong nen loai danh gia dich vu khong anh huong
# ve do thi
res<-resid(M1)
plot(fitted(M1),res)
abline(0,0)

predict(M1,data.frame(Anh_huong_gia_ca = mean(data$Anh_huong_gia_ca), 
                      Nguon_goc = mean(data$Nguon_goc), 
                      courier_time= mean(data$courier_time), 
                      Marketing = mean(data$Marketing),
                      Danh_gia_dich_vu = mean(data$Danh_gia_dich_vu),
                      Chat_luong=c(4), De_dang=5))
predict(M1,data.frame(Anh_huong_gia_ca = max(data$Anh_huong_gia_ca), 
                      Nguon_goc = max(data$Nguon_goc), 
                      courier_time= max(data$courier_time), 
                      Marketing = max(data$Marketing),
                      Danh_gia_dich_vu =max(data$Danh_gia_dich_vu),
                      Chat_luong=c(4), De_dang=5))
