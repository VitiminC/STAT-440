rm(list = ls())
set.seed(440)
library(data.table)
library(ggplot2)
library(boot)

setwd("C:/Users/Charlie Lu/Desktop/")
samples <- fread('./R file/student_scores.csv')

set.seed(440)
glm.fit <- glm(EntranceExam ~ GPA, family = gaussian, data = samples)

set.seed(440)
glm.fit2 <- glm(EntranceExam ~ poly(GPA, 2), family = gaussian, data = samples) 

set.seed(440)
cv.err <- cv.glm(samples, glm.fit)
cv.err$delta


# model 1
set.seed(440)
cv.error <- rep(0, 2)
glm.fit <- glm(GPA ~ EntranceExam, data = samples)
cv.error[1] <- cv.glm(samples, glm.fit)$delta[1]

set.seed(440)
glm.fit <- glm(GPA ~ poly(EntranceExam, 2) , data = samples)
cv.error[2] <- cv.glm(samples, glm.fit)$delta[1]

cv.error


#3 fold cv
set.seed(440)
glm.fit <- glm(GPA ~ EntranceExam, data = samples)
cv.error[3] <- cv.glm(samples, glm.fit, K = 3)$delta[1]

set.seed(440)
glm.fit <- glm(GPA ~ poly(EntranceExam,2), data = samples)
cv.error[4] <- cv.glm(samples, glm.fit, K = 3)$delta[1]

cv.error

