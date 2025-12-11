#data marketing
library(readxl)
marketing <- read_excel("C:/Users/bibly/OneDrive/Desktop/R SJU Practice/marketing_channel_sales.xlsx")

head(marketing)
summary(marketing)

marketing$OnlineDummy <- ifelse(marketing$Channel == "Online", 1, 0)

m1 <- lm(Revenue ~ AdSpend + Price + EmailSubs + OnlineDummy, data = marketing)
summary(m1)

m2 <- lm(Revenue ~ AdSpend + Price + OnlineDummy, data = marketing)
summary(m2)

anova(m1, m2)

par(mfrow = c(1, 2))
plot(m1$fitted.values, resid(m1),
     xlab = "fitted", ylab = "Residuals", main = "Res vs. fitted", pch = 19)
abline(h = 0, col = "red")
hist(resid(m1), main = "Hist of Residuals", xlab = "Residuals")
par(mfrow = c(1,1))

par(mfrow = c(1, 2))
plot(m2$fitted.values, resid(m1),
     xlab = "fitted", ylab = "Residuals", main = "Res vs. fitted", pch = 19)
abline(h = 0, col = "red")
hist(resid(m2), main = "Hist of Residuals", xlab = "Residuals")
par(mfrow = c(1,1))


#data employee
rm(list = ls()); graphics.off()
library(readxl)
employee <- read_excel("C:/Users/bibly/OneDrive/Desktop/R SJU Practice/employee_productivity_hours.xlsx")

head(employee)
summary(employee)

model_linear_simple <- lm(employee$Productivity ~ employee$Hours, data = employee)
summary(model_linear_simple)

plot(employee$Hours, employee$Productivity,
     main = "Linear modelbefore quadratic term",
     xlab = "Hours/week",
     ylab = "Productivity score",
     pch = 19, col = "blue")
abline(model_linear_simple, col = "red", lwd = 2)

plot(employee$Hours, employee$Productivity,
     main = "Prod vs. Hours", xlab = "Hours", ylab = "Productivity", pch = 19)

employee$Hours2 <- employee$Hours^2

plot(employee$Hours2, employee$Productivity,
     main = "Prod vs. Hours^2", xlab = "Hours^2", ylab = "Productivity", pch = 19)


m_lin <- lm(Productivity ~ Hours + Experience + Training, data = employee)
m_quad <- lm(Productivity ~ Hours + Hours2 + Experience + Training, data = employee)
summary(m_lin)
summary(m_quad)

m_quad2 <- lm(Productivity ~ Hours + Hours2 + Experience, data = employee)
summary(m_quad2)

plot(employee$Hours, employee$Productivity,
     main = "Quad:Prod vs. Hours", xlab = "Hours", ylab = "Productivity", pch = 19)
abline(model_linear_simple, col = "red", lwd = 2)

ord <- employee[order(employee$Hours),]
lines(ord$Hours, predict(m_quad2, newdata = ord, lwd = 2, col = "red"))




for (i in seq(1, 11, by = 2)) {
  cat(
    if (i == 3) {
      "three\n"
    } else {
      paste0(i, "\n")
    }
  )
}
