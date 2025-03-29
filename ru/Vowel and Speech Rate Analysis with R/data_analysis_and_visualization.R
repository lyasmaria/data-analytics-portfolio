setwd("C:/Users/lyasm/Documents/RStudio/Dossier")
# устанавливаем библиотеку для работы с .xlsx
install.packages("readxl")  
library(readxl)

# импортируем данные 
data1 = read_excel("test_formants_phrases.xlsx")
data2 = read_excel("test_formants_presentation.xlsx")

## ОБРАБОТКА И ОЧИСТКА ДАННЫХ 
# удаляем ненужный столбец из data1
data1$...1 = NULL

# проверяем типы данных 
str(data1)
str(data2)

# проверяем на пустые значения 
sum(is.na(data1))
sum(is.na(data2))

# смотрим, какие именно пустые 
which(is.na(data2))
data2[!complete.cases(data2), ]

# их всего три, можно удалить. создаем новую таблицу на базе data2 
data2_clean = data2[complete.cases(data2), ]

colnames(data1)[colnames(data1) == "DURÉE"] = "DUREE"
colnames(data2_clean)[colnames(data2_clean) == "DURÉE"] = "DUREE"

summary(data1$DUREE)
summary(data2_clean$DUREE)

# в data2_clean больше данных, чем в data1. необходимо сбалансировать количество данных для анализа 
# определяем, сколько строк должно быть в data2
needed_rows = nrow(data1)  

# удаляем лишние строки случайным образом 
set.seed(42)
data2_balanced = data2[sample(nrow(data2), needed_rows), ] 

# проверяем количество строк (нам нужно 163) 
nrow(data2_balanced) 


## ВИЗУАЛИЗАЦИЯ:
# наблюдаются выбросы в двух таблицах 
ggplot(data1, aes(x = APERTURE, y = DUREE)) +
  geom_boxplot() +
  labs(title = "Duration boxplot for data1", x = "Aperture", y = "Duration")

ggplot(data2_balanced, aes(x = APERTURE, y = DUREE)) +
  geom_boxplot() +
  labs(title = "Duration boxplot for data2", x = "Aperture", y = "Duration")


# мы создаем гистограмму «распределения длительности» для data1:
ggplot(data1, aes(x = DUREE)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "lightblue", alpha = 0.7) +
  labs(title = "Duration distribution (slow speech rate)",
      x = "Duration (in seconds)", y = "Frequency")

# и для data2:
ggplot(data2_balanced, aes(x = DUREE)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "lightblue", alpha = 0.7) +
  labs(title = "Duration distribution (fast speech rate)",
      x = "Duration (in seconds)", y = "Frequency")


# мы преобразуем столбец APERTURE в коэффициент для построения гистограммы
combined_data$APERTURE = c(data1$APERTURE, data2_balanced$APERTURE)
levels(combined_data$APERTURE)
str(combined_data$APERTURE)
combined_data$APERTURE = as.factor(combined_data$APERTURE)

# составляем гистограмму, чтобы увидеть распределение уровней APERTURE
ggplot(combined_data, aes(x = APERTURE)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of aperture levels (combined data)",
      x = "Aperture", y = "Frequency")

# составляем гистограмму, чтобы увидеть распределение длительности по APERTURE
ggplot(combined_data, aes(x = DUREE, fill = APERTURE)) +
  geom_histogram(binwidth = 0.02, position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Distribution of duration by aperture (combined data)",
      x = "Duration", y = "Frequency") +
  theme_minimal()

# составляем гистограмму, чтобы увидеть распределение по скорости речи
ggplot(combined_data, aes(x = DUREE, fill = DEBIT)) +
  geom_histogram(binwidth = 0.01, position = "dodge", color = "black", alpha = 0.7) +
  labs(title = 'Duration distribution by speech rate (combined data)',
      x = 'Duration', y = 'Frequency') +
  scale_fill_manual(values = c('blue', 'red'), 
                    labels = c('Débit lent', 'Débit rapide'))

# делаем диаграмму рассеяния для aperture + duration :
ggplot(combined_data, aes(x = APERTURE, y = DUREE, color = DEBIT)) +
  geom_point() +
  labs(title = "Duration as a function of aperture and flow rate
      (combined data)",
      x = "Aperture", y = "Duration (in seconds)") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()


# проводим двухфакторный тест ANOVA :
aov_result = aov(DUREE ~ APERTURE * DEBIT, data = combined_data)
summary(aov_result)

# проводим t-test :
t_test_debit = t.test(DUREE ~ DEBIT, data = combined_data)

