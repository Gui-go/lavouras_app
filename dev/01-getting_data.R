library(tidyverse)


read.csv(file = "data_raw/Lavouras_CENSO2017.RData")
lavouras


vec <- lavouras$Volume[!is.na(lavouras$Volume)]
range(lavouras$Volume, na.rm = T)
hist(vec)
ggplot(as.data.frame(vec), aes(x=vec)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")


getwd()


