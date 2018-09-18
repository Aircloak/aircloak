library(ggplot2)

args <- commandArgs(TRUE)

csvPath <- args[1]
imagePath <- args[2]

data <- read.csv(csvPath, header=TRUE, sep=";")

graph <- ggplot(data=data) +
  geom_line(aes(x=Index, y=Result, group=Source, color=Source)) +
  xlab("Bucket index") +
  ylab("Value")

ggsave(imagePath, plot = graph, height = 20, width = 40, units = "cm")
