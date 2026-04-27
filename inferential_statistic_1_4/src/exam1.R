# load data
data <- read.csv("data/Cleaned_Intel_CPUs.csv", header = TRUE)
tdp <- na.omit(data$TDP_W) # drop missing values

# Thống kê mẫu
n <- length(tdp) # kích cỡ mẫu
cat("\nn = ", n)
xtb <- mean(tdp) # trung bình mẫu
cat("\ntrung binh = ", xtb)
sx <- sd(tdp) # độ lệch chuẩn mẫu
cat("\ns = ", sx)

# --- Method 1: Shapiro-Wilk test ---
# H0: TDP_W follows a normal distribution
# H1: TDP_W does NOT follow a normal distribution
sw <- shapiro.test(tdp)
print(sw)

# --- Method 2: Q-Q plot ---
par(mfrow = c(1, 2))

png("inferential_statistic/figure/TDP_W_normality.png", width = 14, height = 6, units = "in", res = 150)
par(
    mfrow = c(1, 2),
    mar = c(5, 5, 4, 2), # margins
    cex = 1.3, # text & symbol scale
    cex.main = 1.5, # title size
    cex.lab = 1.3
) # axis label size
# points = sample quantiles vs theoretical normal quantiles
qqnorm(tdp,
    main = "Q-Q Plot — TDP_W",
    xlab = "Theoretical Quantiles",
    ylab = "Sample Quantiles",
    pch = 20, col = "steelblue"
)

# reference line — points should hug this if data is normal
qqline(tdp, col = "red", lwd = 2)

# histogram + fitted normal curve
hist(tdp,    freq = FALSE, main = "Histogram — TDP_W",
    xlab = "TDP_W", col = "#89b4fa55", border = "white"
)
curve(dnorm(x, mean(tdp), sd(tdp)), add = TRUE, col = "red", lwd = 2)

dev.off()
# --- Conclusion ---
alpha <- 0.05

if (sw$p.value > alpha) {
    cat(sprintf("p = %.4f > %.2f → Fail to reject H0. TDP_W is likely normal.\n", sw$p.value, alpha))
} else {
    cat(sprintf("p = %.4f ≤ %.2f → Reject H0. TDP_W is NOT normally distributed.\n", sw$p.value, alpha))
}

# z alpha
z <- qnorm(1 - 0.05)
cat("\nz = ", z)

# Giá trị quan sát
Zqs <- (xtb - 55) / (sx / sqrt(n))
cat("\nZqs = ", Zqs, "\n")

