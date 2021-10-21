# -----------------------------------------------------
# generating new normal population
# -----------------------------------------------------
# Use this to test with new population and samples
population <- rnorm(500)

large_sample <- sample(population, 200, replace = FALSE)
large_sample_replace <- sample(population, 200, replace = TRUE)

big_sample <- sample(population, 60, replace = FALSE)
big_sample_replace <- sample(population, 60, replace = TRUE)

medium_sample <- sample(population, 30, replace = FALSE)
medium_sample_replace <- sample(population, 30, replace = TRUE)

small_sample <- sample(population, 20, replace = FALSE)
small_sample_replace <- sample(population, 20, replace = TRUE)

# -----------------------------------------------------
# saving normal population
# -----------------------------------------------------
# Use this to save the current population and samples
write.csv(population, file = "../data/ex_1/population.csv", row.names = F)
write.csv(large_sample, file = "../data/ex_1/large_sample.csv", row.names = F)
write.csv(large_sample_replace,
          file = "../data/ex_1/large_sample_replace.csv",
          row.names = F)
write.csv(big_sample, file = "../data/ex_1/big_sample.csv", row.names = F)
write.csv(big_sample_replace,
          file = "../data/ex_1/big_sample_replace.csv",
          row.names = F)
write.csv(medium_sample, file = "../data/ex_1/medium_sample.csv", row.names = F)
write.csv(medium_sample_replace,
          file = "../data/ex_1/medium_sample_replace.csv",
          row.names = F)
write.csv(small_sample, file = "../data/ex_1/small_sample.csv", row.names = F)
write.csv(small_sample_replace,
          file = "../data/ex_1/small_sample_replace.csv",
          row.names = F)

# -----------------------------------------------------
# loading normal population
# -----------------------------------------------------
# Use this to load a previous saved population
load_vector <- function(filex) {
    data_frame <- read.csv(filex)
    data_frame$x
}
population <- load_vector("../data/ex_1/population.csv")
large_sample <- load_vector("../data/ex_1/large_sample.csv")
large_sample_replace <- load_vector("../data/ex_1/large_sample_replace.csv")
big_sample <- load_vector("../data/ex_1/big_sample.csv")
big_sample_replace <- load_vector("../data/ex_1/big_sample_replace.csv")
medium_sample <- load_vector("../data/ex_1/medium_sample.csv")
medium_sample_replace <- load_vector("../data/ex_1/medium_sample_replace.csv")
small_sample <- load_vector("../data/ex_1/small_sample.csv")
small_sample_replace <- load_vector("../data/ex_1/small_sample_replace.csv")

# -----------------------------------------------------
# calculating Dispersion and Central Tendency measures
# -----------------------------------------------------
large_sample_mean <- mean(large_sample)
large_sample_replace_mean <- mean(large_sample_replace)

big_sample_mean <- mean(big_sample)
big_sample_replace_mean <- mean(big_sample_replace)

medium_sample_mean <- mean(medium_sample)
medium_sample_replace_mean <- mean(medium_sample_replace)

small_sample_mean <- mean(small_sample)
small_sample_replace_mean <- mean(small_sample_replace)

population_mean <- mean(population)

large_sample_median <- median(large_sample)
large_sample_replace_median <- median(large_sample_replace)

big_sample_median <- median(big_sample)
big_sample_replace_median <- median(big_sample_replace)

medium_sample_median <- median(medium_sample)
medium_sample_replace_median <- median(medium_sample_replace)

small_sample_median <- median(small_sample)
small_sample_replace_median <- median(small_sample_replace)

population_median <- median(population)


large_sample_var <- var(large_sample)
large_sample_replace_var <- var(large_sample_replace)

big_sample_var <- var(big_sample)
big_sample_replace_var <- var(big_sample_replace)

medium_sample_var <- var(medium_sample)
medium_sample_replace_var <- var(medium_sample_replace)

small_sample_var <- var(small_sample)
small_sample_replace_var <- var(small_sample_replace)

population_var <- var(population)

large_sample_sd <- sd(large_sample)
large_sample_replace_sd <- sd(large_sample_replace)

big_sample_sd <- sd(big_sample)
big_sample_replace_sd <- sd(big_sample_replace)

medium_sample_sd <- sd(medium_sample)
medium_sample_replace_sd <- sd(medium_sample_replace)

small_sample_sd <- sd(small_sample)
small_sample_replace_sd <- sd(small_sample_replace)

population_sd <- sd(population)

# -----------------------------------------------------
# show differences in measures betwenn pop. and samples
# -----------------------------------------------------
get_diff <- function(word, sample_val, population_val) {
    dif <- abs(population_val - sample_val)
    output <- paste("The",
                    word,
                    "differs from the population in",
                    format(dif, digits = 3)
    )
    print(output)
}

print("For large sample (n = 200) without replacement")
get_diff("mean", large_sample_mean, population_mean)
get_diff("median", large_sample_median, population_median)
get_diff("variance", large_sample_var, population_var)
get_diff("standard deviation", large_sample_sd, population_sd)

print("For large sample (n = 200) with replacement")
get_diff("mean", large_sample_replace_mean, population_mean)
get_diff("median", large_sample_replace_median, population_median)
get_diff("variance", large_sample_replace_var, population_var)
get_diff("standard deviation", large_sample_replace_sd, population_sd)

print("For big sample (n = 60) without replacement")
get_diff("mean", big_sample_mean, population_mean)
get_diff("median", big_sample_median, population_median)
get_diff("variance", big_sample_var, population_var)
get_diff("standard deviation", big_sample_sd, population_sd)

print("For big sample (n = 60) with replacement")
get_diff("mean", big_sample_replace_mean, population_mean)
get_diff("median", big_sample_replace_median, population_median)
get_diff("variance", big_sample_replace_var, population_var)
get_diff("standard deviation", big_sample_replace_sd, population_sd)

print("For medium sample (n = 30) without replacement")
get_diff("mean", medium_sample_mean, population_mean)
get_diff("median", medium_sample_median, population_median)
get_diff("variance", medium_sample_var, population_var)
get_diff("standard deviation", medium_sample_sd, population_sd)

print("For medium sample (n = 30) with replacement")
get_diff("mean", medium_sample_replace_mean, population_mean)
get_diff("median", medium_sample_replace_median, population_median)
get_diff("variance", medium_sample_replace_var, population_var)
get_diff("standard deviation", medium_sample_replace_sd, population_sd)

print("For small sample (n = 20) without replacement")
get_diff("mean", small_sample_mean, population_mean)
get_diff("median", small_sample_median, population_median)
get_diff("variance", small_sample_var, population_var)
get_diff("standard deviation", small_sample_sd, population_sd)

print("For small sample (n = 20) with replacement")
get_diff("mean", small_sample_replace_mean, population_mean)
get_diff("median", small_sample_replace_median, population_median)
get_diff("variance", small_sample_replace_var, population_var)
get_diff("standard deviation", small_sample_replace_sd, population_sd)

# -----------------------------------------------------
# box plot samples
# -----------------------------------------------------
values <- c(large_sample, large_sample_replace,
            big_sample, big_sample_replace,
            population,
            medium_sample, medium_sample_replace,
            small_sample, small_sample_replace)
sample_type <- c()
for (i in seq_len(length(large_sample))) {
    sample_type  <- append(sample_type, "l")
}
for (i in seq_len(length(large_sample_replace))) {
    sample_type  <- append(sample_type, "lr")
}
for (i in seq_len(length(big_sample))) {
    sample_type  <- append(sample_type, "b")
}
for (i in seq_len(length(big_sample_replace))) {
    sample_type  <- append(sample_type, "br")
}
for (i in seq_len(length(population))) {
    sample_type  <- append(sample_type, "p")
}
for (i in seq_len(length(medium_sample))) {
    sample_type  <- append(sample_type, "m")
}
for (i in seq_len(length(medium_sample_replace))) {
    sample_type  <- append(sample_type, "mr")
}
for (i in seq_len(length(small_sample))) {
    sample_type  <- append(sample_type, "s")
}
for (i in seq_len(length(small_sample_replace))) {
    sample_type  <- append(sample_type, "sr")
}

boxplot(values~sample_type,
        data = data.frame(values, sample_type),
        xlab = "Sample Types",
        ylab = "Values"
)

# -----------------------------------------------------
# bar plot mean
# -----------------------------------------------------
names = c("l", "lr", "b", "br", "m", "mr", "s", "sr", "p")
barplot(c(large_sample_mean,
          large_sample_replace_mean,
          big_sample_mean,
          big_sample_replace_mean,
          medium_sample_mean,
          medium_sample_replace_mean,
          small_sample_mean,
          small_sample_replace_mean,
          population_mean),
        names.arg = names,
        xlab = "Samples",
        ylab = "Mean"
)
# -----------------------------------------------------
# bar plot variance
# -----------------------------------------------------
barplot(c(large_sample_var,
          large_sample_replace_var,
          big_sample_var,
          big_sample_replace_var,
          medium_sample_var,
          medium_sample_replace_var,
          small_sample_var,
          small_sample_replace_var,
          population_var),
        names.arg = names,
        xlab = "Samples",
        ylab = "Variance"
)
# -----------------------------------------------------
# define confidence intervals functions
# -----------------------------------------------------
# Verbose output of the mean analysis
print_mean  <- function(interval, population_mean, alpha) {
    res  <- paste(
                  "With confidence level", 1 - alpha,
                  "population mean is in between (",
                  format(interval[1], digits = 3), ",",
                  format(interval[2], digits = 3), ")",
                  "which is",
                  interval[1] < population_mean | interval[2] > population_mean
    )
    print(res)
}

# Verbose output of the variance analysis
print_var  <-  function(interval, population_var, alpha) {
    res  <- paste(
                  "With confidence level", 1 - alpha,
                  "population variance is in between (",
                  format(interval[1], digits = 3), ",",
                  format(interval[2], digits = 3), ")",
                  "which is",
                  interval[1] < population_var | interval[2] > population_var
    )
    print(res)

}

get_mean_conf_interval  <-  function(data, population_mean, alpha = 0.05) {
    x <- mean(data)
    s <- sd(data)
    error <- qnorm(1 - alpha / 2) * s / sqrt(length(data))

    interval <- c(x - error, x  + error)
    print_mean(interval, population_mean, alpha)
}

get_variance_conf_interval  <- function(data, population_var, alpha = 0.05) {
    s2 <- var(data)
    n <- length(data)
    error_left  <- qchisq(1 - alpha / 2, df = n - 1)
    error_right  <- qchisq(alpha / 2, df = n - 1)
    num  <-  (n - 1) * s2

    interval <- c(num / error_left, num / error_right)
    print_var(interval, population_var, alpha)
}

get_small_mean_conf_interval  <-
    function(data, population_mean, alpha = 0.05) {
        x <- mean(data)
        s <- sd(data)
        n <- length(data)
        error <- qt(1 - alpha / 2, n - 1) * s / sqrt(n)

        interval <- c(x - error, x  + error)
        print_mean(interval, population_mean, alpha)
    }

# -----------------------------------------------------
# show samples mean confidence interval
# -----------------------------------------------------
alpha  <- 0.05
print("Large sample (n = 200) without replacement")
get_mean_conf_interval(large_sample, population_mean, alpha)
print("Large sample (n = 200) with replacement")
get_mean_conf_interval(large_sample_replace, population_mean, alpha)

print("Big sample (n = 60) without replacement")
get_mean_conf_interval(big_sample, population_mean, alpha)
print("Big sample (n = 60) with replacement")
get_mean_conf_interval(big_sample_replace, population_mean, alpha)

print("Medium sample (n = 30) without replacement")
get_small_mean_conf_interval(medium_sample, population_mean, alpha)
print("Medium sample (n = 30) with replacement")
get_small_mean_conf_interval(medium_sample_replace, population_mean, alpha)

print("Small sample (n = 20) without replacement")
get_small_mean_conf_interval(small_sample, population_mean, alpha)
print("Small sample (n = 20) with replacement")
get_small_mean_conf_interval(small_sample_replace, population_mean, alpha)

# -----------------------------------------------------
# show samples variance confidence interval
# -----------------------------------------------------
print("Large sample (n = 200) without replacement")
get_variance_conf_interval(large_sample, population_mean, alpha)
print("Large sample (n = 200) with replacement")
get_variance_conf_interval(large_sample_replace, population_mean, alpha)

print("Big sample (n = 60) without replacement")
get_variance_conf_interval(big_sample, population_mean, alpha)
print("Big sample (n = 60) with replacement")
get_variance_conf_interval(big_sample_replace, population_mean, alpha)

print("Medium sample (n = 30) without replacement")
get_variance_conf_interval(medium_sample, population_mean, alpha)
print("Medium sample (n = 30) with replacement")
get_variance_conf_interval(medium_sample_replace, population_mean, alpha)

print("Small sample (n = 20) without replacement")
get_variance_conf_interval(small_sample, population_mean, alpha)
print("Small sample (n = 20) with replacement")
get_variance_conf_interval(small_sample_replace, population_mean, alpha)
