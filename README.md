
## Suggested works

### Part A: Descriptive Statistics (Phần Thống Kê Mô Tả)
This section is all about showing you understand the basic shape of the data. Since you have 8 variables (Price, Cores, Turbo, Launch Year, TDP, Cache, Max Mem, Segment), we will visualize them and explain the math behind them.

* **Histogram:** We will plot the distribution of `Price_USD` (dropping NAs just for this plot) and `TDP_W`. In the report, you will explain what a histogram is, show the formula for Mean and Variance, and comment on whether the data is skewed (e.g., "Most CPUs are cheap, but a few server CPUs pull the average price way up").
* **Boxplot:** We will plot `Price_USD` grouped by `Vertical_Segment` (Desktop vs. Mobile vs. Server). You will explain quartiles (Q1, Q2, Q3), the IQR formula, and how boxplots help spot outliers (like a ridiculously expensive Desktop chip).
* **Pair Plot (Biểu đồ Pair):** We will throw all 7 numeric variables into one massive matrix plot. This shows the scatter plots and correlations for every combination at once. You explain the concept of the Correlation Coefficient (r) here.
* **The "Forgotten" Section (Nhận xét và mở rộng):** This is where you set your trap for the bonus points. You write a summary: *"We noticed strong correlations between Cores, Cache, and Price. However, a major limitation is that 43% of the dataset is missing MSRP (Price). As an extension of this descriptive analysis, we will use inferential modeling later to estimate these missing values."*

### Part B: Inferential Statistics (Phần Thống Kê Suy Diễn)
For every single problem here, the rubric explicitly demands we **check the conditions** (kiểm tra các điều kiện) before running the test in R (like checking for normal distribution or equal variances).

#### Problem 1: 1-Sample t-test (Testing average TDP):
* **The Setup:** Test if the average base `TDP_W` of all Intel CPUs is greater than a certain threshold (e.g., 65W).
* **The Steps:** Check for normality (Shapiro-Wilk test) or rely on the Central Limit Theorem since our sample size is huge. Run a 1-sample t-test in R and find the confidence interval.
* **The Test:** Before you can run a t-test, you are supposed to prove the data follows a bell curve.
* **The Pre-test:** Normal Distribution Test.
* **How to do it in R:** Use the Shapiro-Wilk test `shapiro.test(df$TDP_W)` or draw a Q-Q Plot.
* **The Pro Move:** Since your dataset has over 1,200 rows, it will likely fail the Shapiro test (it's too sensitive to large data). Just write in your report: *"Due to the large sample size ($n > 30$), we rely on the Central Limit Theorem (CLT) to assume normality."* Professors love seeing you invoke the CLT.

#### Problem 2: 2-Sample t-test (Desktop vs. Mobile Turbo Freq)
* **The Setup:** Compare the mean `Turbo_Frequency_GHz` between Desktop CPUs and Mobile CPUs (ignoring Servers for this specific test). 
* **The Steps:** Check if the variances between Desktop and Mobile are equal (F-test or Levene's test). Then run a 2-sample independent t-test to see if Desktop chips boost significantly higher than Mobile chips.
* **The Test:** This is where the variance tests come in. A 2-sample t-test has two versions: one if the two groups have the same variance, and one if they have different variances (Welch's t-test). You must test them to know which version to use.
* **The Pre-test:** Test for Equal Variances (Homoscedasticity).
* **How to do it in R:** Use `var.test(Desktop_Turbo, Mobile_Turbo)`. 
* **The Logic:** If the p-value is > 0.05, they have equal variance (use `var.equal = TRUE` in your t-test). If p < 0.05, they have unequal variance (use `var.equal = FALSE`).

#### Problem 3: 1-Way ANOVA (Max Memory by Segment)
* **The Setup:** Does `Max_Memory_GB` differ significantly across all three `Vertical_Segment` categories? 
* **The Steps:** Check for normality of residuals and homogeneity of variances. Run the ANOVA. Since the prompt says "Nếu làm Anova 2 yếu tố thì vẫn cần làm Anova 1 yếu tố" (If doing 2-way, still need 1-way), we can just stick to a rock-solid 1-way ANOVA to save time, because our advanced point is coming up next.
* **The Setup:** Does `Max_Memory_GB` differ significantly across all three `Vertical_Segment` categories? 
* **The Steps:** Check for normality of residuals and homogeneity of variances. Run the ANOVA. Since the prompt says "Nếu làm Anova 2 yếu tố thì vẫn cần làm Anova 1 yếu tố" (If doing 2-way, still need 1-way), we can just stick to a rock-solid 1-way ANOVA to save time, because our advanced point is coming up next.
* ANOVA is super strict. It assumes that Desktop, Mobile, and Server segments all have roughly the same "spread" (variance) of data. 
* **The Pre-test:** Levene's Test or Bartlett's Test.
* **Why not `var.test`?:** `var.test()` only works for comparing TWO groups. Since ANOVA compares 3 or more groups (Desktop, Mobile, Server), you **must** use Levene's or Bartlett's.
* **How to do it in R:** Use `bartlett.test(Max_Memory_GB ~ Vertical_Segment, data = df)` or `leveneTest()` from the `car` package. 

#### Problem 4: Multiple Linear Regression (Predicting Price)
* **The Setup:** This replaces the simple linear regression and gets you the max score. We predict `Price_USD` (Target) using Cores, Turbo, TDP, Cache, and Max Mem (Predictors).
* **The Steps:** Drop the rows with `NA` prices to create our training set. Check the heavy MLR conditions: Linearity, Independence, Homoscedasticity, and Normality of residuals. Run the model and interpret the R-squared value.
* **The Grand Finale:** We use the R `predict()` function. We feed the 43% of CPUs that are missing prices into our trained model, and it will spit out the statistically estimated prices for all of them.
* Regression has the strictest rules of all. You aren't testing the original variables here; you are testing the **residuals** (the errors or leftovers after the model makes a guess).
1. **Normality of Residuals:** * *The Pre-test:* Shapiro-Wilk or Q-Q plot on the model's residuals.
   * *R Code:* `plot(model, 2)` (This draws a beautiful Q-Q plot).
2. **Equal Variance of Residuals (Homoscedasticity):**
   * *The Pre-test:* Breusch-Pagan test or checking the Residuals vs Fitted plot.
   * *R Code:* `plot(model, 1)`. You want the dots to look like random static, not a funnel shape.
3. **Independence of Variables:**
   * *The Pre-test:* You need to prove your predictors (Cores, Cache, TDP) aren't mathematically identical to each other. This is called checking for Multicollinearity. 
   * *How to do it in R:* Use the VIF (Variance Inflation Factor) function `vif(model)`. If any number is above 5 or 10, that variable is not independent and you should drop it!

**Summary for your Workflow:**
Whenever you start one of the 4 "Problem", your very first R code block should be the pre-tests (Shapiro, Var, Levene). Write one sentence concluding what the pre-test means, and *then* run the actual t-test, ANOVA, or Regression!