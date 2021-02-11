library(readr)
library(dplyr)
library(readxl)
library(moments)
library(utf8)
library(stringr)


# NẠP FILE VÔ VÀ ĐỔI LẠI TÊN CÁC BIẾN CHO DỄ THAO TÁC
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

#LẤY CÁC GIÁ TRỊ QUAN TÂM LIÊN QUAN TỚI SHOPPEE
data <- cbind( Excel[][2], Excel[][8:11], Excel[][21:34])

## LOẠI BỎ HẾT CÁ GIÁ TRỊ NA
for(i in 1:19) {
  data <- subset(data, !is.na(data[][i]))
}

#ĐỔI LẠI THÔNG SÔ TRONG CỘT SỬ DỤNG TIẾP THÀNH SÓ ĐỂ THAO TÁC PHÂN TÍCH
data$Su_dung_tiep[data$Shopee == 0] = 0
data$Su_dung_tiep[data$Su_dung_tiep == 'Có'] = 1
data$Su_dung_tiep[data$Su_dung_tiep == 'Không'] = -1
data$Su_dung_tiep[data$Su_dung_tiep == "Sẽ xem xét"] = 0
data$Su_dung_tiep = as.numeric(data$Su_dung_tiep)


#THÊM BIẾN TỔNG ĐIỂM
temp <- data[][7]
colnames(temp)[1] = "Tong_diem"
for(i in 8:19) {
  temp <- temp[][1] + data[][i]
}
data <- cbind(data, temp)
View(data)


#LÀM RÕ DỮ LIỆU

#VỚI CÁC BIẾN ĐÁNH GIÁ BẰNG ĐIỂM
#B1: Tính các giá trị min, max, sd, median, average
Average = apply(data[7:20], 2, mean)
Median= apply(data[7:20], 2, median)
SD = apply(data[7:20], 2, sd)
Max = apply(data[7:20], 2, max)
Min = apply(data[7:20], 2, min)

#B2: Đưa các thông số này vô một bảng 
statistical_table1 <- cbind(Average, Median)
statistical_table1 <- cbind(statistical_table1, SD)
statistical_table1 <- cbind(statistical_table1, Max)
statistical_table1 <- cbind(statistical_table1, Min)
View(statistical_table1)

#B3: Với các biến phân loại thì xài tble để thống kê số lượng
statistical_table2 <- apply(data[][1:6], 2, table)
View(statistical_table2)

#VẼ ĐỒ THỊ

#B1:  Thống kê phần trăm người các loại quyết định sử dụng
h = hist(data$Tong_diem)
#h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, main = "Plot percent of su_dung_tiep", ylab = "%", xlab = "Quyet_dinh", border="brown", col="orange")


#B2:Boxplot của việc sử dụng tiếp theo Tỏng điểm
boxplot(data$Tong_diem[data$Su_dung_tiep == 1], data$Tong_diem[data$Su_dung_tiep == 0],
        data$Tong_diem[data$Su_dung_tiep == -1], main = "Boxplot Distribution of Tong_diem for Su_dung_tiep",
        at = c(1,2,3),
        names = c("Có", "Sẽ xem xét", "Không"), xlab = "Trang_thai", ylab = "Tong_diem", border="brown", col="orange")

boxplot(data$Tong_diem[data$Proper_Price == 1], data$Tong_diem[data$Proper_Price == 0],
        main = "Boxplot Distribution of Proper_Price for Su_dung_tiep",
        at = c(1,2),
        names = c("Có", "Không"), xlab = "Trang_thai", ylab = "Tong_diem", border="brown", col="orange")
boxplot(data$Tong_diem[data$Time_saving == 1], data$Tong_diem[data$Time_saving == 0],
        main = "Boxplot Distribution of Time_saving for Su_dung_tiep",
        at = c(1,2),
        names = c("Có", "Không"), xlab = "Trang_thai", ylab = "Tong_diem", border="brown", col="orange")

boxplot(data$Tong_diem[data$Da_dang == 1], data$Tong_diem[data$Da_dang == 0],
        main = "Boxplot Distribution of Da_dang for Su_dung_tiep",
        at = c(1,2),
        names = c("Có", "Không"), xlab = "Trang_thai", ylab = "Tong_diem", border="brown", col="orange")

boxplot(data$Tong_diem[data$Khuyen_Mai == 1], data$Tong_diem[data$Khuyen_Mai == 0],
        main = "Boxplot Distribution of Khuyen_Mai for Su_dung_tiep",
        at = c(1,2),
        names = c("Có", "Không"), xlab = "Trang_thai", ylab = "Tong_diem", border="brown", col="orange")

pairs(~data$Tong_diem +data$Chat_luong)

#################################################
#MO HINH DU LIEU
hoiqui1<-lm(data$Tong_diem~data$Su_dung_tiep + data$Chat_luong +data$Time_saving+ data$Proper_Price + data$Da_dang + data$Khuyen_Mai, data = data)
summary(hoiqui1)


hoiqui2<-lm(data$Tong_diem~data$Proper_Price + data$Chat_luong + data$Khuyen_Mai + data$Da_dang, data = data)
summary(hoiqui2)

hoiqui3<-lm(data$Tong_diem~data$Proper_Price + data$Khuyen_Mai + data$Da_dang, data = data)
summary(hoiqui2)

Anova1<-anova(hoiqui2,hoiqui3)
summary(Anova1)

  
  resid(hoiqui2)
  fitted(hoiqui2)  
  op <- par(mfrow=c(2,2)) #yeu cau R danh ra 4 cua so
  plot(hoiqui2) #ve cac do thi trong reg  
  
  
  #5 DU BAO
  
  predict(hoiqui2,
          data.frame( Da_dang = mean(new_DF$Da_dang),
                      Proper_Price = mean(new_DF$Proper_Price),
                      Khuyen_Mai = mean(new_DF$Khuyen_Mai),
                      Chat_luong = mean(new_DF$Chat_luong),
                        Time_saving= 1, Shopee = 1))
  predict(hoiqui1,
          data.frame( Da_dang = max(new_DF$Da_dang),
                      Proper_Price = max(new_DF$Proper_Price),
                      Chat_luong = max(new_DF$Chat_luong),
                      Khuyen_Mai = max(new_DF$Khuyen_Mai),
                      Time_saving= 1, Shopee = 1))
  