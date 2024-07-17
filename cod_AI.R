library(dplyr)
library(rpart)
date <- read.csv("loan_approval_dataset.csv")
date <- as.data.frame(date)
date <- date[, -1]
date %>% head()
table(date$loan_status)
str(date)
date <- date %>%
  mutate(
    education = ifelse(education == " Graduate", 1, 0),
    self_employed = ifelse(self_employed == " Yes", 1, 0),
    loan_status = ifelse(loan_status == " Approved", 1, 0)
  )

date$education <- as.integer(date$education)
date$self_employed <- as.integer(date$self_employed)
date$loan_status <- as.integer(date$loan_status)


#### Statistici descriptive ####
summary(date)
library(psych)
describe(date)

#Histograme
# Generare histograme pentru variabilele numerice
par(mfrow = c(3, 3))
hist(
  date$income_annum,
  main = "Histogram of Annual Income",
  xlab = "Annual Income",
  col = "blue",
  breaks = 50
)
hist(
  date$loan_amount,
  main = "Histogram of Loan Amount",
  xlab = "Loan Amount",
  col = "green",
  breaks = 50
)
hist(
  date$loan_term,
  main = "Histogram of Loan Term",
  xlab = "Loan Term (months)",
  col = "red",
  breaks = 20
)
hist(
  date$cibil_score,
  main = "Histogram of CIBIL Score",
  xlab = "CIBIL Score",
  col = "purple",
  breaks = 50
)
hist(
  date$residential_assets_value,
  main = "Histogram of Residential Assets Value",
  xlab = "Residential Assets Value",
  col = "orange",
  breaks = 50
)
hist(
  date$commercial_assets_value,
  main = "Histogram of Commercial Assets Value",
  xlab = "Commercial Assets Value",
  col = "cyan",
  breaks = 50
)
hist(
  date$luxury_assets_value,
  main = "Histogram of Luxury Assets Value",
  xlab = "Luxury Assets Value",
  col = "pink",
  breaks = 50
)

# Generare boxplot-uri pentru variabilele numerice
par(mfrow = c(3, 3))
boxplot(
  date$income_annum,
  main = "Boxplot of Annual Income",
  ylab = "Annual Income",
  col = "blue"
)
boxplot(
  date$loan_amount,
  main = "Boxplot of Loan Amount",
  ylab = "Loan Amount",
  col = "green"
)
boxplot(date$loan_term,
        main = "Boxplot of Loan Term",
        ylab = "Loan Term (months)",
        col = "red")
boxplot(
  date$cibil_score,
  main = "Boxplot of CIBIL Score",
  ylab = "CIBIL Score",
  col = "purple"
)
boxplot(
  date$residential_assets_value,
  main = "Boxplot of Residential Assets Value",
  ylab = "Residential Assets Value",
  col = "orange"
)
boxplot(
  date$commercial_assets_value,
  main = "Boxplot of Commercial Assets Value",
  ylab = "Commercial Assets Value",
  col = "cyan"
)
boxplot(
  date$luxury_assets_value,
  main = "Boxplot of Luxury Assets Value",
  ylab = "Luxury Assets Value",
  col = "pink"
)
boxplot(
  date$bank_assets_value,
  main = "Boxplot of Bank Assets Value",
  ylab = "Bank Assets Value",
  col = "brown"
)

# Generare diagramă de bare pentru variabilele categoriale
barplot(
  table(date$no_of_dependents),
  main = "Number of Dependents",
  xlab = "Number of Dependents",
  ylab = "Frequency",
  col = "lightblue"
)
barplot(
  table(date$education),
  main = "Education Level",
  xlab = "Education Level",
  ylab = "Frequency",
  col = c("lightgreen", "lightcoral"),
  names.arg = c("Not Graduate", "Graduate")
)
barplot(
  table(date$self_employed),
  main = "Self Employed Status",
  xlab = "Self Employed",
  ylab = "Frequency",
  col = c("lightgrey", "lightyellow"),
  names.arg = c("No", "Yes")
)
barplot(
  table(date$loan_status),
  main = "Loan Status Distribution",
  xlab = "Loan Status",
  ylab = "Frequency",
  col = c("red", "green"),
  names.arg = c("Rejected", "Approved")
)

library(corrplot)
corrplot(cor(date), method = "number")

install.packages('caTools')
library(caTools)

################REGRESIE LOGISTICA#####################
#1#
date_scale <- scale(date)
date_scale %>% head()
date_scale <- as.data.frame(date_scale)
date_scale$loan_status <- date$loan_status
date_scale$loan_status <- as.factor(date_scale$loan_status)
head(date_scale)
View(date_scale)

set.seed(123)
split <- sample.split(date$loan_status, SplitRatio = 0.8)
split


library(ggplot2)
plot <-
  ggplot(data = date,
         aes(
           x = date$cibil_score,
           y = date$loan_term,
           col = loan_status
         ))
plot <- plot + geom_point(aes(size = 5))
plot

training <- subset(date_scale, split == TRUE)
testing <- subset(date_scale, split == FALSE)

plot <-
  ggplot(data = date_scale,
         aes(
           x = date$cibil_score,
           y = date$loan_term,
           col = loan_status
         ))
plot <- plot + geom_point(aes(size = 5))
plot

View(testing)

# Antrenarea modelului de regresie logistică
logistic_model <-
  glm(loan_status ~ cibil_score + loan_term,
      data = training,
      family = binomial)
summary(logistic_model)


# Exponențierea coeficienților
exp(coef(logistic_model))

# Previzionarea probabilităților pe setul de testare
prob <- predict(logistic_model, testing, type = "response")
prob

# Construirea vectorului de predicții
pred <- rep("0", dim(testing)[1])
pred[prob > 0.5] <- "1"
pred

# Matricea de confuzie
conf_matrix <- table(pred, testing$loan_status)
print(conf_matrix)

# Calcularea acurateței modelului
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
print(paste(
  "Acuratețea regresiei logistice este:",
  round(accuracy * 100, 2),
  "%"
))

#Curba ROC

prob <- predict(logistic_model, testing, type = "response")

# Construirea obiectului de predicție
pred <- prediction(prob, testing$loan_status)

# Performanța modelului pentru curba ROC
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Plotarea curbei ROC
plot(perf, col = "blue", main = "Curba ROC pentru Modelul Logistic")
# Linie de referință pentru clasificare aleatorie
abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "red"
)

# Calcularea AUC
auc <- performance(pred, measure = "auc")
auc_value <- auc@y.values[[1]]
print(paste("AUC:", round(auc_value, 2)))


########REGRESIE MULTINOMIALA###########################
library(nnet)
library(MASS)
library(caTools)
set.seed(88)

date_multinom <- date_scale

date_multinom$income_category <- cut(
  date_multinom$income_annum,
  breaks = quantile(date_multinom$income_annum, probs = seq(0, 1, by = 0.25)),
  include.lowest = TRUE,
  labels = c("low", "medium-low", "medium-high", "high")
)


date_multinom <- date_multinom[, -4]

date_multinom %>% head()

date_multinom$income_category <-
  as.factor(date_multinom$income_category)


table(date_multinom$income_category)

training <- subset(date_multinom, split == TRUE)
testing <- subset(date_multinom, split == FALSE)

model <- multinom(income_category ~ ., data = training, trace = FALSE)
summary(model)
exp(coef(model))
predictions <- predict(model, testing)
predictions
predict(model, testing, type = "prob")

conf_matrix <-
  confusionMatrix(table(predictions, testing$income_category))
conf_matrix

accuracy <- mean(predictions == testing$income_category)
accuracy

##########ARBORE DE REGRESIE#####################################################
library(tree)
library(ISLR)

# Împărțim setul de date în antrenare și testare
set.seed(123)
split <-
  createDataPartition(y = date$income_annum,
                      p = 0.5,
                      list = FALSE)
split
# Definim setul de antrenare și setul de testare
train <- date_scale[split,]
test <- date_scale[-split,]
train
test

# Construim arborele de regresie
arbore_regresie <- rpart(income_annum ~ ., data = train)
arbore_regresie2 <- tree(income_annum ~ ., data = train)
# Reprezentăm grafic arborele
rpart.plot(arbore_regresie)

# Validare încrucișată
cv_regresie <- cv.tree(arbore_regresie2)
cv_regresie$size
cv_regresie$dev

# Reprezentăm grafic eroarea în funcție de marimea arborelui
plot(cv_regresie$size, cv_regresie$dev, type = 'b')

# Construim arborele curățat
arbore_regresie_curatat <- prune.tree(arbore_regresie2, best = 5)
plot(arbore_regresie_curatat)
text(arbore_regresie_curatat, pretty = 0)

# Predictie pe setul de testare
pred <- predict(arbore_regresie_curatat, test)
pred

# Calculăm eroarea de predicție
mean((pred - test$income_annum) ^ 2)

plot(x = pred , y = test$income_annum)
abline(0, 1)


###################ARBORE DE CLASIFICARE#############

# Transformăm loan_status într-un factor
date$loan_status <- as.factor(date$loan_status)
date %>% head()
# Împărțirea setului de date în antrenare (60%) și testare (40%)
set.seed(111)
split <-
  createDataPartition(y = date$loan_status,
                      p = 0.6,
                      list = FALSE)

# Setul de antrenare
train <- date_scale[split,]

# Setul de testare
test <- date_scale[-split,]

# Construim arborele de clasificare
arbore <- tree(loan_status ~ ., data = train, method = "class")
arbore2 <- rpart(loan_status ~ ., data = train, method = "class")
rpart.plot(arbore2)

# Predicție pentru setul de testare
pred <- predict(arbore, test, type = 'class')
pred
# Calculăm acuratețea
accuracy <- mean(pred == test$loan_status)
accuracy
print(paste(
  "Acuratețea inițială a modelului este:",
  round(accuracy * 100, 2),
  "%"
))

# Matricea de confuzie
confusionMatrix <- table(pred, test$loan_status)
print(confusionMatrix)

# Îmbunătățirea acurateței predictiei utilizând validarea încrucișată
set.seed(12)
cv.tree1 <- cv.tree(arbore, FUN = prune.misclass)
# Reprezentăm grafic eroarea în funcție de mărimea arborelui
plot(
  cv.tree1$size,
  cv.tree1$dev,
  type = 'b',
  xlab = "Număr de noduri terminale",
  ylab = "Eroare de validare",
  main = "Validare încrucișată"
)

# Curățăm arborele de clasificare
arbore1 <-
  prune.misclass(arbore, best = cv.tree1$size[which.min(cv.tree1$dev)])
plot(arbore1)
text(arbore1)

# Predicție pentru setul de testare cu arborele curățat
prune.predictie <- predict(arbore1, test, type = 'class')


# Calculăm acuratețea pentru arborele curățat
prune_accuracy <- mean(prune.predictie == test$loan_status)
prune_accuracy
print(paste(
  "Acuratețea modelului curățat este:",
  round(prune_accuracy * 100, 2),
  "%"
))

#Matricea de confuzie pentru arborele curățat
confusionMatrixCV <- table(prune.predictie, test$loan_status)
print(confusionMatrixCV)

#################### KNN CLASIFICARE###############
library(FNN)
library(caret)
library(caTools)
library(tidyverse)

date$loan_status <- as.factor(date$loan_status)
data_subset <- date_scale

# Împărțirea setului de date în antrenare (80%) și testare (20%)
set.seed(123)
split <-createDataPartition(data_subset$loan_status, p = 0.8, list = FALSE)
train_data <- data_subset[split,]
test_data <- data_subset[-split,]

# Normalizarea datelor
train_data %>% head()
# Antrenarea modelului KNN cu k = 3
knn_model <-
  knn(
    train = train_data[, -12],
    test = test_data[, -12],
    cl = train_data$loan_status,
    k = 3
  )

# Evaluarea performanței modelului
conf_matrix <- table(Predicted = knn_model, Actual = test_data$loan_status)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Acuratețea modelului KNN este:", round(accuracy * 100, 2), "%"))

# Determinarea valorii optime a lui k
train_control <- trainControl(method = "cv", number = 5)
knn_tuned <-
  train(
    loan_status ~ .,
    data = train_data,
    method = "knn",
    tuneLength = 20,
    trControl = train_control,
  )
sum(is.na(train_data))
sum(is.na(test_data))

# Afișarea modelului optim
print(knn_tuned)
plot(knn_tuned)

# Predictii cu modelul optim
best_k <- knn_tuned$bestTune$k
knn_best_model <-
  knn(
    train = train_data[, -12],
    test = test_data[, -12],
    cl = train_data$loan_status,
    k = best_k
  )

# Evaluarea performanței modelului optim
conf_matrix_best <-table(Predicted = knn_best_model, Actual = test_data$loan_status)
print(conf_matrix_best)

accuracy_best <- sum(diag(conf_matrix_best)) / sum(conf_matrix_best)
print(paste(
  "Acuratețea modelului KNN optim este:",
  round(accuracy_best * 100, 2),
  "%"
))
roc_curve <- roc(as.numeric(test_data$loan_status), as.numeric(as.character(knn_best_model)))
plot(roc_curve)
auc(roc_curve)

###################################RETELE NEURONALE###############
install.packages("neuralnet")
install.packages("e1071")
library(neuralnet)
library(e1071)

loan_data <- date
loan_data$education <- as.factor(loan_data$education)
loan_data$self_employed <- as.factor(loan_data$self_employed)
loan_data$loan_status <- as.factor(loan_data$loan_status)

loan_data$education <- as.numeric(loan_data$education)
loan_data$self_employed <- as.numeric(loan_data$self_employed)
loan_data$loan_status <- as.numeric(loan_data$loan_status) - 1  # Ajustare pentru a avea valori 0 și 1


# Scalare date numerice
scale_columns <- c('income_annum', 'loan_amount', 'loan_term', 'cibil_score',
                   'residential_assets_value', 'commercial_assets_value', 
                   'luxury_assets_value', 'bank_asset_value')

loan_data[scale_columns] <- scale(loan_data[scale_columns])

# Împărțirea datelor în seturi de antrenare și testare
set.seed(1234567890)
trainset <- loan_data[1:3200, ]
testset <- loan_data[3201:nrow(loan_data), ]

# Construirea rețelei neuronale
formula <- as.formula("loan_status ~ no_of_dependents + education + self_employed + income_annum + loan_amount + loan_term + cibil_score + residential_assets_value + commercial_assets_value + luxury_assets_value + bank_asset_value")
loan_net <- neuralnet(formula, data=trainset, hidden=4, lifesign="minimal", linear.output=FALSE, threshold=0.1)

# Vizualizarea rețelei neuronale
plot(loan_net, rep="best")

temp_test <- subset(testset, select=c('no_of_dependents', 'education', 'self_employed', 'income_annum', 'loan_amount', 'loan_term', 'cibil_score', 'residential_assets_value', 'commercial_assets_value', 'luxury_assets_value', 'bank_asset_value'))
compute_result <- compute(loan_net, temp_test)
predictions <- compute_result$net.result
results <- data.frame(actual=testset$loan_status, prediction=predictions)

# Rotunjirea valorilor pentru predicții
results$prediction <- round(results$prediction)

# Convertirea în factor pentru comparație
results$actual <- as.factor(results$actual)
results$prediction <- as.factor(results$prediction)

# Matricea de confuzie
confusion_matrix <- table(results$actual, results$prediction)

# Evaluarea performanței modelului
class_agreement <- classAgreement(confusion_matrix)
print(confusion_matrix)
print(class_agreement)

head(results, 20)

########################################CLUSTERIZARE FUZZY

date_numeric <- date[, sapply(date, is.numeric)]
result <-
  cmeans(
    date_numeric,
    centers = 3,
    iter.max = 100,
    m = 2,
    method = "cmeans"
  )

print(result)

plot(date$income_annum, date$cibil_score, col = result$cluster)
points(result$centers[, c(4, 7)],
       col = 1:3,
       pch = 8,
       cex = 2)

o <- order(result$cluster)
data.frame(date[o, ], Cluster = result$cluster[o])

clustered_date <- data.frame(date, Cluster = result$cluster)

res.fanny <- fanny(date[1:500, ], 3)

data.frame(Cluster = res.fanny$clustering, date[1:500, ])

fviz_cluster(
  res.fanny,
  ellipse.type = "norm",
  repel = TRUE,
  palette = "jco",
  ggtheme = theme_minimal(),
  legend = "right"
)

fviz_silhouette(res.fanny, palette = "jco", ggtheme = theme_minimal())

print(res.fanny$silinfo)

