# Lọc bỏ giá trị NA và lấy dữ liệu theo phân khúc
df <- read.csv("Cleaned_Intel_CPUs.csv")
df2 <- na.omit(df[, c("Vertical_Segment", "Turbo_Frequency_GHz")])
desktop_CPU <- subset(df2, Vertical_Segment == "Desktop")$Turbo_Frequency_GHz
mobile_CPU <- subset(df2, Vertical_Segment == "Mobile")$Turbo_Frequency_GHz

# Đặc trưng mẫu Desktop
n1 <- length(desktop_CPU)
x_1 <- mean(desktop_CPU)
s1 <- sd(desktop_CPU)

# Đặc trưng mẫu Mobile
n2 <- length(mobile_CPU)
x_2 <- mean(mobile_CPU)
s2 <- sd(mobile_CPU)

# Tổng hợp kết quả
result <- data.frame(
  Phan_khuc = c("Desktop", "Mobile"),
  So_luong = c(n1, n2),
  Trung_binh = c(x_1, x_2),
  Do_lech_chuan = c(s1, s2)
)
View(result)

# Chia khung hình thành 1 hàng, 2 cột để hiện cả 2 đồ thị
par(mfrow = c(1, 2))

# Q-Q plot cho phân khúc Desktop
qqnorm(desktop_CPU, main="Q-Q Plot: Desktop Turbo Frequency")
qqline(desktop_CPU, col="red")

# Q-Q plot cho phân khúc Mobile
qqnorm(mobile_CPU, main="Q-Q Plot: Mobile Turbo Frequency")
qqline(mobile_CPU, col="blue")

# Reset lại khung hình về bình thường
par(mfrow = c(1, 1))

# Kiểm định Shapiro-Wilk cho Desktop
shapiro.test(desktop_CPU)

# Kiểm định Shapiro-Wilk cho Mobile
shapiro.test(mobile_CPU)

# Ép R phải in kết quả ra màn hình Console
print(shapiro.test(desktop_CPU))
print(shapiro.test(mobile_CPU))

# Tính giá trị Z-test thực nghiệm (Z0)
z0 <- (x_1 - x_2) / sqrt((s1^2/n1) + (s2^2/n2))

# Xác định giá trị tới hạn Z_alpha với alpha = 0.05 (Kiểm định một phía bên phải)
alpha <- 0.05
z_alpha <- qnorm(1 - alpha)

# Hiển thị kết quả
data.frame(Z0 = z0, Z_alpha = z_alpha)

#Xuất kết quả và kết luận
cat("Giá trị thực nghiệm Z0:", z0, "\n")
cat("Giá trị tới hạn Z_alpha:", z_alpha, "\n")

if (z0 > z_alpha) {
  print("Kết luận: Bác bỏ H0. Xung nhịp Turbo của Desktop cao hơn Mobile.")
} else {
  print("Kết luận: Chưa đủ cơ sở bác bỏ H0.")
}