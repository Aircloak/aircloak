library("stats")

# We allow this script to be called as `Rscript beta.r count alpha beta``
args <- commandArgs(TRUE)

count <- as.numeric(args[1])
alpha <- as.numeric(args[2])
beta <- as.numeric(args[3])

data <- rbeta(count, alpha, beta)
cat(data)