# Đọc dữ liệu
df <- read.csv("data/Cleaned_Intel_CPUs.csv")

#Lọc NA ở 2 cột dữ liệu sẽ dùng
df2 <- na.omit(df[, c("Vertical_Segment", "Max_Memory_GB")])

# Xem số lượng quan sát ở mỗi phân khúc
bang_thong_ke_1 <- table(df2$Vertical_Segment)
print(bang_thong_ke_1)

# Khai báo data_anova (Lọc 3 phân khúc)
seg3 <- c("Desktop", "Mobile", "Server")
data_anova <- df2[df2$Vertical_Segment %in% seg3, c("Vertical_Segment", "Max_Memory_GB")]
bang_thong_ke_2 <- table(data_anova$Vertical_Segment)
print(bang_thong_ke_2)

# 1. Q-Q Plot cho "Desktop"
qqnorm(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Desktop"], 
       main = "Q-Q Plot for Desktop")
qqline(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Desktop"], col = "red")

# Chạy kiểm định Shapiro-Wilk cho "Desktop"
shapiro.test(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Desktop"])
print(shapiro.test(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Desktop"]))

# 2. Q-Q Plot cho "Mobile"
qqnorm(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Mobile"], 
       main = "Q-Q Plot for Mobile")
qqline(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Mobile"], col = "red")

# Chạy kiểm định Shapiro-Wilk cho "Mobile"
shapiro.test(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Mobile"])
print(shapiro.test(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Mobile"]))

# 3. Q-Q Plot cho "Server"
qqnorm(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Server"], 
       main = "Q-Q Plot for Server")
qqline(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Server"], col = "red")

# Chạy kiểm định Shapiro-Wilk cho "Server"
shapiro.test(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Server"])
print(shapiro.test(data_anova$Max_Memory_GB[data_anova$Vertical_Segment == "Server"]))

# Gọi thư viện car
library(car)

# Ép kiểu Vertical_Segment sang dạng factor
data_anova$Vertical_Segment <- as.factor(data_anova$Vertical_Segment)

# Chạy kiểm định Levene và in kết quả ra
ket_qua_levene <- leveneTest(Max_Memory_GB ~ Vertical_Segment, data = data_anova)
print(ket_qua_levene)

model_aov <- aov(Max_Memory_GB ~ Vertical_Segment, data = data_anova)
bang_thong_ke_3 <- summary(model_aov)
print(bang_thong_ke_3)

tukey_results <- TukeyHSD(model_aov)
print(tukey_results)
png("inferential_statistic_2_3/figure/Tukey_Plot.png", width=800, height=600, res=120)
par(mar = c(5, 10, 4, 2))
plot(tukey_results, las = 1)
dev.off()