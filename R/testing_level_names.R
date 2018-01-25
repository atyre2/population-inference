library(mgcv)
data(iris)
# flip the treatment contrast so coefficient labels match
new_contrast <- contr.SAS(3)
colnames(new_contrast) <- c("setosa", "versicolor")
contrasts(iris$Species) <- new_contrast
sz_contrast <- contr.sum(3)
colnames(sz_contrast) <- c("setosa", "versicolor")
iris$szSpecies <- iris$Species
contrasts(iris$szSpecies) <- sz_contrast

summary(lm0 <- lm(Sepal.Length~szSpecies, data = iris))
summary(glm0 <- glm(Sepal.Length~szSpecies, data = iris))
summary(gam0 <- gam(Sepal.Length~szSpecies, data = iris))
