install.packages('corrplot')
install.packages('psych')
install.packages('ggplot2')
install.packages("gmodels")
install.packages('caret')
install.packages('e1071')

library(corrplot)
library(psych)
library(ggplot2)
library(gmodels)
library(dplyr)
#library('caret')
library('e1071')



df = read.csv(file='/Users/anastasiyarutkovskaya/Downloads/gym.csv', header = TRUE, sep=",", dec='.', fill=TRUE, comment.char="")
head(df[,4:11]) # выведем первые 6 строк

data<-df[,4:11]
str(data)  # выведем информацию о типах данных и переменных

summary(data) # выведем статистические данные о каждой переменной

# расчет главных компонент
fit <- princomp(data)

# Выводим результаты анализа
summary(fit)

# расчет главных компонент с собственными значениями более 1
NumOfFactors <- sum(fit$sdev > 1)

# вывод результата
cat("Количество факторов, определенное методом Кайзера:", NumOfFactors)

# расчет собственных значений
fact <- fa.parallel(data, n.iter=500, fm="ml", fa="fa")

# вывод результата
cat("Количество факторов, определенное методом экранного теста:", fact$parallel[1])


# Применяем факторный анализ  с ограничением до 4 факторов
fa_result <- factanal(data, factors = 4)
# Выводим результаты анализа
print(fa_result)

#Получим вектор относительных весов 
loadings<-fa_result$loadings

# Определяем данные
dframe <- data.frame(var = rownames(loadings), factor = rep(colnames(loadings), each = nrow(loadings)), value = as.vector(loadings))

# Строим тепловую карту
ggplot(dframe, aes(x = factor, y = var, fill = value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 2))) +
  labs(x = "Factors", y = "Variables", fill = "Loadings")


# Выводим полученные факторы и их вклад в общую дисперсию
fa_result$loadings

# Добавляем значения факторов в исходный датасет
factor1 <- df$Class_registration_weekly
factor2 <- df$Friend_promo
factor3 <- df$Avg_additional_charges_total
factor4 <- df$Contract_period
newdata <- data.frame(factor1,factor2,factor3,factor4,df$Exited)
newdata

#Разделим данные на обучающую и тестовую выборки
index <- sample(2,nrow(newdata),prob = c(0.9,0.1),replace=TRUE) 
set.seed(1234)
train <- newdata[index==1,]
test <- newdata[index==2,]
test_data <- test[1:4]
test_data
test_label <- test[,5]
test_label
# Обучим модель:
vector <-newdata[1:dim(train),]
vector
train$factor1
vector$df.Exited
model<-naiveBayes(vector$df.Exited~.,train)
model
# Получим предсказания модели:
test_result<-predict(model,test_data)
test_data
test_result
# Оценка модели:
caret::confusionMatrix(factor(test_result),factor(test_label))


