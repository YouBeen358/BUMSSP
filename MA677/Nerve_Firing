nerve_data <- scan("nerve.dat.txt")

ecdf_plot <- plot(ecdf(nerve_data), 
                  main = "Empirical CDF of Nerve Data", xlab = "Nerve Firing Times", 
                  ylab = "Cumulative Probability",
                  col = "blue")

quantiles <- quantile(nerve_data, probs = c(0.25, 0.5, 0.75))
print(quantiles)


n <- length(nerve_data)
epsilon <- sqrt(log(2/0.05) / (2 * n)) 
Fn <- ecdf(nerve_data)

lines(sort(nerve_data), pmax(Fn(sort(nerve_data)) - epsilon, 0), col = "red", lty = 2)
lines(sort(nerve_data), pmin(Fn(sort(nerve_data)) + epsilon, 1), col = "red", lty = 2)

fraction <- Fn(0.6) - Fn(0.4)
cat("Fraction of waiting times between 0.4 and 0.6:", fraction, "\n")

summary_stats <- summary(nerve_data)
print(summary_stats)
