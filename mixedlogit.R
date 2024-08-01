library(MASS)
library(caret)
library(mlogit)
library(pscl)
library(nnet)
library(lmtest)


m1 <- multinom(DurationGroup ~ TUTIER1CODE + age_group + previous + TESEX + TELFS + TESCHENR + TRERNWA + PTDTRACE + .start, 
               data = pa,
               maxit = 1000)

m2 <- multinom(DurationGroup ~ TUTIER1CODE + age_group + previous + TESEX + TELFS + TESCHENR + TRERNWA + PTDTRACE + .start, 
               data = pa_weekend,
               maxit = 1000)

logLik_m1 <- logLik(m1)
logLik_m2 <- logLik(m2)

logLik(m1)
logLik(m2)

pR2(m1)
pR2(m2)

llr_statistic <- -2 * (logLik_m1 - logLik_m2)

df <- abs(attr(logLik_m1, "df") - attr(logLik_m2, "df"))

p_value <- pchisq(llr_statistic, df, lower.tail = FALSE)

cat("Log-Likelihood Ratio Statistic:", llr_statistic, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-Value:", p_value, "\n")


