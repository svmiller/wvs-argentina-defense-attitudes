library(Zelig)
library(RCurl)
 
data <- getURL("https://raw.githubusercontent.com/svmiller/wvs-argentina-defense-attitudes/master/wvs-argentina-defense-attitudes.csv")
Data <- read.csv(text = data)
summary(Data)


# Do note this isn't supposed to be a sophisticated model.

M1 <- glm(aims.defense ~ age + female + unemployed + satisfinancial + postma4 + respectauthority + nationalpride, data=Data, family=binomial(link="logit"))

summary(M1)

Data$z.age <- with(Data, (age - mean(age, na.rm = TRUE))/(2*sd(age, na.rm = TRUE)))
Data$z.satisfinancial <- with(Data, (satisfinancial - mean(satisfinancial, na.rm = TRUE))/(2*sd(satisfinancial, na.rm = TRUE)))
Data$z.postma4 <- with(Data, (postma4 - mean(postma4, na.rm = TRUE))/(2*sd(postma4, na.rm = TRUE)))
Data$z.respectauthority <- with(Data, (respectauthority - mean(respectauthority, na.rm = TRUE))/(2*sd(respectauthority, na.rm = TRUE)))

# Compare coefficient magnitudes.

M2 <- glm(aims.defense ~ age + female + unemployed + z.satisfinancial + z.postma4 + z.respectauthority + nationalpride, data=Data, family=binomial(link="logit"))

# Zelig sims don't play well with missing data. Let's just subset what we want.

Data2 <- with(Data,  data.frame(aims.defense, age, female, unemployed, satisfinancial, postma4, respectauthority, nationalpride))
Data2 <- na.omit(Data2)

M2 <- zelig(aims.defense ~ age + female + unemployed + satisfinancial + postma4 + respectauthority + nationalpride, model = "logit", data = Data2)

M2.low <- setx(M2, respectauthority = -1, satisfinancial = 1:10)
M2.high <- setx(M2, respectauthority = 1, satisfinancial = 1:10)

M2.sim <- sim(M2, x = M2.low, x1 = M2.high)

plot.ci(M2.sim, xlab = "Financial Satisfaction",
         ylab = "Expected Value of Defense Being Most Important National Goal",
         main = "Effect of Respect for Authority and Financial Satisfaction on Attitudes toward Defense",
)
