
mycov <- function(X, Y) {
  n <- length(X)
  cov <- 0
  for (i in 1:n) {
    cov <- cov + (X[i] - mean(X)) * (Y[i] - mean(Y))
  }
  return (1 / (n - 1) * cov)
}

mycor <- function(X, Y) {
  return (mycov(X,Y) / (sd(X) * sd(Y)))
}

X <- 1:10
Y <- 2:11

mycov(X, Y)
mycor(X, Y)

#Bai1:
#a.
snow_data <- data.frame(
  year = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979),
  snow_cover = c(6.5, 12.0, 14.9, 10.0, 10.7, 7.9, 21.9, 12.5, 14.5, 9.2)
)

#b.
# Vẽ biểu đồ
ggplot(snow_data, aes(x = year, y = snow_cover)) +
  geom_line() +
  geom_point() +
  labs(title = "Snow Cover by Year", x = "Year", y = "Snow Cover") +
  theme_minimal()

#c.
snow_data$log_snow_cover <- log(snow_data$snow_cover)

# Vẽ biểu đồ của logarit biến snow.cover theo year
ggplot(snow_data, aes(x = year, y = log_snow_cover)) +
  geom_line() +
  geom_point() +
  labs(title = "Logarithm of Snow Cover by Year", x = "Year", y = "Log(Snow Cover)") +
  theme_minimal()

#Bai2:
#a.
# Tạo data.frame lamphat1
lamphat1 <- data.frame(
  Nam = 1960:1980,
  US = c(1.5, 1.1, 1.1, 1.2, 1.4, 1.6, 2.8, 2.8, 4.2, 5.0, 5.9, 4.3, 3.6, 6.2, 10.9, 9.2, 5.8, 6.4, 7.6, 11.4, 13.6),
  Anh = c(1.0, 3.4, 4.5, 2.5, 3.9, 4.6, 3.7, 2.4, 4.8, 5.2, 6.5, 9.5, 6.8, 8.4, 16.0, 24.2, 16.5, 15.9, 8.3, 13.4, 18.0)
)

# Tạo data.frame lamphat2
lamphat2 <- data.frame(
  Nam = 1960:1980,
  Nhat = c(3.6, 5.4, 6.7, 7.7, 3.9, 6.5, 6.0, 4.0, 5.5, 5.1, 7.6, 6.3, 4.9, 12.0, 24.6, 11.7, 9.3, 8.1, 3.8, 3.6, 8.0),
  Duc = c(1.5, 2.3, 4.5, 3.0, 2.3, 3.4, 3.5, 1.5, 18.0, 2.6, 3.7, 5.3, 5.4, 7.0, 7.0, 5.9, 4.5, 3.7, 2.7, 4.1, 5.5)
)

#b.
# Trộn hai data.frame theo cột "Nam"
lamphat <- merge(lamphat1, lamphat2, by = "Nam")

#c.
# Đếm số năm các nước có tỷ lệ lạm phát trên 5%
count_over_5 <- sapply(lamphat[-1], function(x) sum(x > 5))
print(count_over_5)

#d.
# Thiết lập layout 2x2 
par(mfrow = c(2, 2))

# Vẽ đồ thị cho US
plot(lamphat$Nam, lamphat$US, type = "o", col = "blue", pch = 16,
     main = "Tỉ lệ lạm phát US (1960-1980)",
     xlab = "Năm", ylab = "Tỉ lệ lạm phát (%)")

# Vẽ đồ thị cho Anh
plot(lamphat$Nam, lamphat$Anh, type = "o", col = "red", pch = 16,
     main = "Tỉ lệ lạm phát Anh (1960-1980)",
     xlab = "Năm", ylab = "Tỉ lệ lạm phát (%)")

# Vẽ đồ thị cho Nhật
plot(lamphat$Nam, lamphat$Nhat, type = "o", col = "green", pch = 16,
     main = "Tỉ lệ lạm phát Nhật (1960-1980)",
     xlab = "Năm", ylab = "Tỉ lệ lạm phát (%)")

# Vẽ đồ thị cho Đức
plot(lamphat$Nam, lamphat$Duc, type = "o", col = "purple", pch = 16,
     main = "Tỉ lệ lạm phát Đức (1960-1980)",
     xlab = "Năm", ylab = "Tỉ lệ lạm phát (%)")

#Tất cả các nước đều có xu hướng tăng lạm phát từ năm 1960 đến khoảng năm 1974-1975
#Anh và Nhật có biến động lạm phát mạnh nhất, với mức đỉnh điểm trên 24%
#Đức có một đỉnh bất thường vào năm 1968 (18%)
#Sau năm 1975, hầu hết các nước có xu hướng giảm lạm phát, nhưng Mỹ và Anh lại tăng trở lại vào cuối thập kỷ 70

#e.
# Tạo hàm tính sai số chuẩn
std_error <- function(x) {
  sd(x) / sqrt(length(x))
}

# Tính các thông số thống kê cho từng nước
thong_ke <- data.frame(
  Chi_so = c("Trung bình", "Trung vị", "Max", "Min", 
             "Độ lệch chuẩn", "Sai số chuẩn"),
  US = c(mean(lamphat$US), median(lamphat$US), max(lamphat$US), min(lamphat$US),
         sd(lamphat$US), std_error(lamphat$US)),
  Anh = c(mean(lamphat$Anh), median(lamphat$Anh), max(lamphat$Anh), min(lamphat$Anh),
          sd(lamphat$Anh), std_error(lamphat$Anh)),
  Nhat = c(mean(lamphat$Nhat), median(lamphat$Nhat), max(lamphat$Nhat), min(lamphat$Nhat),
           sd(lamphat$Nhat), std_error(lamphat$Nhat)),
  Duc = c(mean(lamphat$Duc), median(lamphat$Duc), max(lamphat$Duc), min(lamphat$Duc),
          sd(lamphat$Duc), std_error(lamphat$Duc))
)

print(thong_ke)

#f.
# Tính hệ số biến thiên (CV = độ lệch chuẩn / trung bình)
cv <- data.frame(
  Nuoc = c("US", "Anh", "Nhat", "Duc"),
  He_so_bien_thien = c(
    sd(lamphat$US)/mean(lamphat$US),
    sd(lamphat$Anh)/mean(lamphat$Anh),
    sd(lamphat$Nhat)/mean(lamphat$Nhat),
    sd(lamphat$Duc)/mean(lamphat$Duc)
  )
)

print(cv)

#Để xác định lạm phát nước nào biến thiên nhiều hơn, ta cần dựa vào 
#hệ số biến thiên (CV = độ lệch chuẩn/trung bình). Hệ số biến thiên càng lớn thì 
#biến thiên càng mạnh. Đức có hệ số biến thiên cao nhất (0.745), cho thấy tỉ lệ 
#lạm phát của Đức biến thiên mạnh nhất trong 4 nước, tiếp theo là Anh (0.74),
#Mỹ (0.72) và Nhật (0.63).

#g.
# Tạo data.frame mới không chứa dữ liệu năm 1980
lamphat1 <- lamphat[lamphat$Nam != 1980, ]

# Kiểm tra năm cuối cùng trong data.frame mới
tail(lamphat1, 1)

#h.
Xi <- lamphat1$Nam
Yi <- lamphat1$US
n <- length(Xi)

X_mu <- mean(Xi)  # Trung bình của các năm
Y_mu <- mean(Yi)  # Trung bình của tỉ lệ lạm phát US
sum_Xi_Yi <- sum(Xi * Yi)  # Tổng tích Xi và Yi
sum_Xi_squared <- sum(Xi^2)  # Tổng bình phương của Xi

# Tính hệ số Beta2_mu (hệ số góc)
Beta2_mu <- (sum_Xi_Yi - n * X_mu * Y_mu) / (sum_Xi_squared - n * X_mu^2)

# Tính hệ số Beta1_mu (hệ số chặn)
Beta1_mu <- Y_mu - Beta2_mu * X_mu

# In kết quả
cat("Beta1_mu (hệ số chặn) =", Beta1_mu, "\n")
cat("Beta2_mu (hệ số góc) =", Beta2_mu, "\n")
cat("Phương trình hồi quy: US =", round(Beta1_mu, 2), "+", round(Beta2_mu, 4), "* Nam\n")

# Vẽ đồ thị phương trình hồi quy
plot(lamphat1$Nam, lamphat1$US, pch = 16, col = "blue",
     xlab = "Năm", ylab = "Tỉ lệ lạm phát (%)",
     main = "Mô hình hồi quy tuyến tính của lạm phát US theo thời gian")

# Thêm đường hồi quy
abline(Beta1_mu, Beta2_mu, col = "red", lwd = 2)

# Thêm phương trình vào đồ thị
text(1965, 10, paste("US =", round(Beta1_mu, 2), "+", round(Beta2_mu, 4), "* Nam"), col = "red")

#i.
# Dự đoán tỉ lệ lạm phát năm 1980 dựa vào mô hình
predicted_1980 <- Beta1_mu + Beta2_mu * 1980

# Tỉ lệ lạm phát thực tế năm 1980
actual_1980 <- lamphat$US[lamphat$Nam == 1980]

# So sánh
comparison <- data.frame(
  Loai = c("Giá trị dự đoán", "Giá trị thực tế", "Chênh lệch", "Phần trăm chênh lệch (%)"),
  Gia_tri = c(
    predicted_1980, 
    actual_1980, 
    actual_1980 - predicted_1980, 
    (actual_1980 - predicted_1980) / actual_1980 * 100
  )
)

print(comparison)

#Mô hình dự đoán tỉ lệ lạm phát của Mỹ năm 1980 là khoảng 9.72%, 
#trong khi giá trị thực tế là 13.6%. Mô hình ước tính thấp hơn khoảng 28.49% so với giá trị thực tế,
#cho thấy năm 1980 có mức lạm phát cao bất thường so với xu hướng dự đoán từ dữ liệu trước đó.


#Bai3:
#a.
# Nhập dữ liệu vào đối tượng Y
Y <- c(7,8,6,4,9,11,9,9,9,10,9,8,11,5,8,5,8,8,7,8,3,5,8,7,10,7,8,9,8,11,10,8,9,8,9,9,7,8,13,8,9,6,7,9,9,7,9,5,6,5,6,9,8,8,4,4,7,7,8,9,10,2,7,10,8,10,6,7,7,8)

# Tạo biểu đồ tần suất
hist(Y, breaks = seq(from = min(Y) - 0.5, to = max(Y) + 0.5, by = 1), 
     col = "skyblue", main = "Biểu đồ tần suất số cây có đường kính > 12cm",
     xlab = "Số cây", ylab = "Tần suất", xlim = c(0, 15))

# Thêm đường biểu diễn phân phối chuẩn
x <- seq(min(Y) - 1, max(Y) + 1, length = 100)
y <- dnorm(x, mean = mean(Y), sd = sd(Y))
y <- y * length(Y) # Điều chỉnh tỉ lệ để phù hợp với tần suất trên biểu đồ
lines(x, y, col = "red", lwd = 2)

#b.
# Tính trung bình mẫu
mu_Y <- mean(Y)

# Tính độ lệch chuẩn mẫu
sigma_Y <- sd(Y)

# In kết quả
cat("Trung bình mẫu =", round(mu_Y, 4), "\n")
cat("Độ lệch chuẩn mẫu =", round(sigma_Y, 4), "\n")

#c.
# Xây dựng hàm khoang()
khoang <- function(x) {
  # x là số độ lệch chuẩn từ giá trị trung bình
  lower <- mu_Y - x * sigma_Y  # Đầu mút dưới của khoảng
  upper <- mu_Y + x * sigma_Y  # Đầu mút trên của khoảng
  return(c(lower, upper))
}

# Tính khoảng ước lượng với các giá trị x khác nhau
x_values <- c(1, 2, 3)
intervals <- lapply(x_values, khoang)

# Tính tỷ lệ thực tế của dữ liệu nằm trong mỗi khoảng
proportions <- sapply(intervals, function(interval) {
  lower <- interval[1]
  upper <- interval[2]
  sum(Y >= lower & Y <= upper) / length(Y) * 100
})

# Tạo bảng kết quả
result <- data.frame(
  x = x_values,
  Lower = sapply(intervals, function(i) i[1]),
  Upper = sapply(intervals, function(i) i[2]),
  "Tỷ lệ thực tế (%)" = proportions,
  "Quy tắc thực nghiệm (%)" = c(68.27, 95.45, 99.73)
)

# In bảng kết quả
print(result)