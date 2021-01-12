#########################################################################################
# Profitable Zimbabwe crops
#   That can be exported
#########################################################################################


# Libraries
library(tidyverse)


# Simulations
set.seed(263)
Granadila <- 15*sample(8000:20000, 50)
Beans <- 3*sample(10000:15000, 50)
Peas <- 24*sample(2500:3500, 50)
Berries <- 18*sample(3000:4000, 50)
Carrots <- sample(20000:40000, 50)
Corn <- 12*sample(4000:5000, 50)
Marrow <- sample(7000:15000, 50)
Chillies <- 25*sample(7000:15000, 50)
Brocolli <- 10*sample(5000:12000, 50)
Avocado <- sample(8000:10000, 50)


# Data frame
e_data <- data.frame(Granadila, Beans, Peas, Berries, Carrots, Corn, Marrow, Chillies,
                     Brocolli, Avocado)

#Reshape data = Combining Cases and Deaths same column
df <- e_data %>%
  pivot_longer(., cols = c(Granadila, Beans, Peas, Berries, Carrots, Corn, 
                           Marrow, Chillies,
                           Brocolli, Avocado), names_to = "Crops", 
               values_to = "Revenue ($/ha)")

means <- aggregate(`Revenue ($/ha)` ~  Crops, df, mean)


# Graph
ggplot(df, aes(x = Crops, y=`Revenue ($/ha)`, fill= Crops)) + geom_boxplot() +
  ggtitle("Cash generating export crops for Zimbabwe") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = means, aes(label = `Revenue ($/ha)`, y = `Revenue ($/ha)` + 0.10))

