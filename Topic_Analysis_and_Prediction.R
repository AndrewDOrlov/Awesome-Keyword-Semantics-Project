# загрузка данных

secure_df <- read.csv("C:/Users/andre/Dev/R/Topic_Analysys/data/Secure_df_processed.csv",
                      na = "")

# загрузка библиотек

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(trend)

# очистка данных: остаются только тексты с датами

secure_df <- secure_df[complete.cases(secure_df$Date),]

# форматирование: данные в колонке Date рассматриваются в формате даты

secure_df = secure_df |>
  mutate(Date = as.Date(secure_df$Date,
                               tryFormats = c("%Y-%m-%d", "%Y/%m/%d")))

# округление даты до месяца, квартала, полугодия 
secure_df = secure_df |>
  mutate(Month = floor_date(secure_df$Date,  unit = "month")) |>
  mutate(Quarter = floor_date(secure_df$Date,  unit = "3 months")) |>
  mutate(Halfyear = floor_date(secure_df$Date,  unit = "6 months"))

# из датасета убираются данные за неполные месяцы:

secure_df <- subset(secure_df, !Month == "2023-02-01" & !Month=="2021-04-01")

# из датасета убираются данные за неполные кварталы и полугодия (кроме января 2023, это полугодие мы дозаполним предсказанными данными):
secure_df <- subset(secure_df, !Month == "2021-04-01" &
                       !Quarter == "2021-04-01")

#Таблица сопряженности месяц - тема - количество текстов
month_count_topics <- table(secure_df$Topic_tag_1, secure_df$Month)

#Датафрейм из таблицы сопряженности в длинном формате
month_count_long_df <- data.frame(month_count_topics)
names(month_count_long_df)[1] <- "Topic"
names(month_count_long_df)[2] <- "Month"

#Датафрейм из таблицы сопряженности в широком формате
month_count_wide_df <- month_count_long_df %>%
  unnest(cols = c()) %>%
  pivot_wider(names_from = "Topic",
              values_from = "Freq", values_fill = FALSE)

# Таблица сопряженности по количеству публикаций на темы в месяц

ggplot(month_count_long_df, aes(x = Topic, y = Month, fill = Freq)) +
  geom_bin2d(colour = "white", lwd = 1, linetype = 1) +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  guides(fill = guide_colourbar(title = "Количество",
                                ticks = TRUE,
                                ticks.colour = "black",
                                barwidth = 0.5,
                                barheight = 10))+
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))+
  coord_flip()+
  labs(title = 'Количество публикаций по темам в месяц',
       x = 'Тема публикаций',
       y = 'Месяц')

# данные в колонке Month приводятся к формату даты для проведения тестов и построения частотного графика
month_count_wide_df$Month <- as.Date(month_count_wide_df$Month, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

#тесты на выделение тренда

#тест Манна-Кендалла
mk = mk.test(month_count_wide_df$утечки)
mk

#тест Петтитт
pt = pettitt.test(month_count_wide_df$утечки)
pt

#линейная регрессия
lm(утечки ~ Month, data = month_count_wide_df)
summary(lm(утечки ~ Month, data = month_count_wide_df))

# Добавляются новые даты для прогноза
new_df <- data.frame(Month = c("2023-02-01", "2023-03-01", "2023-04-01","2023-05-01","2023-06-01"))
new_df = new_df |>
  mutate(Month = as.Date(new_df$Month,
                        tryFormats = c("%Y-%m-%d", "%Y/%m/%d")))

# прогноз на частотность темы
predict(lm(утечки ~ Month, data = month_count_wide_df), newdata = new_df)


# График изменчивости частоты определенной темы по месяцам

# (2) данные в колонке Month приводятся к формату даты для проведения тестов и построения частотного графика
month_count_wide_df$Month <- as.Date(month_count_wide_df$Month, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

# График количества публикаций по теме "Утечки" + прогноз на первое полугодие 2023 г.
month_count_upd <- c(month_count_wide_df$Month, new_df$Month)
topic_freq_prediction <- c(predict(lm(утечки ~ Month, data = month_count_wide_df), newdata = new_df))
topic_freq_upd <- c(month_count_wide_df$утечки, topic_freq_prediction)
month_count_topic_upd <- data.frame(Month = month_count_upd, утечки = topic_freq_upd)

ggplot(month_count_topic_upd, aes(x = Month, y = утечки)) +
  geom_line() +
  geom_area(alpha = 0.4) +
  geom_smooth(method = 'lm', se = FALSE, color = "yellow") +
  # geom_smooth(color = 'blue', se = FALSE, span = 0.4) +
  geom_vline(xintercept = month_count_wide_df$Month[pt$estimate], color = "red") +
  annotate("Text", label = "Точка изменения тренда",
           x = month_count_wide_df$Month[pt$estimate] +50, y = -2, 
           size = 4, colour = "red") +
  geom_vline(xintercept = month_count_topic_upd$Month[19], color = "blue") +
  annotate("Text", label = "Начало прогноза",
           x = month_count_topic_upd$Month[19] +40, y = -2, 
           size = 4, colour = "blue") +
  theme_minimal() +
  labs(title = 'Количество публикаций по теме "Утечки"',
       x = 'Месяц',
       y = 'Кол-во публикаций') +
  scale_x_date(date_breaks ="month", expand = expansion(add = 5)) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))
