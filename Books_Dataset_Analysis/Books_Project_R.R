#Курсов проект по Приложна статистика на Михаела Папаринова, сп."Анализ на данни", курc II, ФН: 8MI0200032

#install.packages("tidyverse")
library(tidyverse)
library(corrplot)
library(ggplot2)


# Зареждане на данните
setwd("C:/Users/mihae/Downloads/archive (1)")
gbooks = read_csv("books.csv")

#Summary Statistics
str(gbooks)
summary(gbooks)
head(gbooks)
View(gbooks)

# Добавяне на колона - year
gbooks$publication_date = as.Date(gbooks$publication_date, format="%m/%d/%Y")
gbooks$year = as.numeric(format(gbooks$publication_date, "%Y"))
head(gbooks[, c('publication_date', 'year')])
View(gbooks)

# Проверка за липсваши данни и изчистване на данните
missing_values = is.na(gbooks)
sum(missing_values)
books = na.omit(gbooks)
sum(is.na(books))
missing_data_summary = sapply(books, function(x) sum(is.na(x)) / length(x)) * 100
print(missing_data_summary)
View(books)

# Дали някои от данните са нормално разпределени? Например средните оценки:
#shapiro_test =shapiro.test(books$average_rating)
#shapiro_test Пробвах с тест на Шапиро-Уилк, но се оказа, че извадката е твърде голяма

# Затова използвам тест на Колмогоров-Смирнов
mu_avg_rating = mean(books$average_rating)
sd_avg_rating = sd(books$average_rating)
ks= ks.test(books$average_rating, "pnorm", mean = mu_avg_rating,sd = sd_avg_rating) #Не (p-value < 2.2e-16)
ks

#1. Анализ

# 1.1 Топ 10 на най-високо оценените книги
top_books = books %>% 
  arrange(desc(average_rating)) %>% 
  head(10)

top_books %>% 
  ggplot(aes(
    x = reorder(title, average_rating),
    y = average_rating,
    fill =as.factor(title)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 10)) +
  coord_flip() +  ggtitle("Топ 10 на най-високо оценените книги")

# 1.2 Топ 10 на най-ниско оценените книги (които не са с 0 звезди)
low_books = books %>%
  filter(average_rating !=0) %>% 
  arrange(average_rating) %>% 
  head(10)

low_books %>% 
  ggplot(aes(
    x = reorder(title, average_rating),
    y = average_rating,
    fill = as.factor(title)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 10)) +
  coord_flip() +  ggtitle("Топ 10 на най-ниско оценените книги")

# 1.3 Топ 10 на най-популярните книги
most_popular_books = books %>% 
  mutate(popularity = ratings_count + text_reviews_count) %>% 
  arrange(desc(popularity)) %>% 
  head(10)

most_popular_books %>% 
  ggplot(aes(
    x = reorder(title, popularity),
    y = popularity,
    fill = as.factor(title)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 10)) +
  coord_flip() +  
  ggtitle("Топ 10 на най-популярните книги")

# 1.4 Най-дългите книги
long_books = books %>% 
  arrange(desc(num_pages)) %>% 
  head(10)

long_books %>% 
  ggplot(aes(
    x = reorder(title, num_pages),
    y = num_pages,
    fill =as.factor(num_pages)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  coord_flip() +  ggtitle("Най-дългите книги") 

# 1.5 Най-късите книги
short_books = books %>%
  filter( num_pages <= 5 ) %>%
  group_by(num_pages) %>%
  sample_n(size = 3, replace = TRUE) %>% 
  ungroup() %>%
  arrange(num_pages) 

short_books %>% 
  ggplot(aes(
    x = reorder(title, num_pages),
    y = num_pages,
    fill = as.factor(num_pages) 
  )) +
  geom_col() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()) +
  coord_flip() +  ggtitle("Най-късите книги ") 

# 1.6 Tоп 10 на книгите с най-много рецензии
most_reviewd = books %>% 
  arrange(desc(text_reviews_count)) %>% 
  head(10) 
most_reviewd %>% 
  ggplot(aes(
    x = reorder(title, text_reviews_count),
    y = text_reviews_count,
    fill = as.factor(text_reviews_count)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) + ggtitle("Tоп 10 на книгите с най-много рецензии") +
  coord_flip()

# 1.7 Tоп 10 най-четените книги
most_read_books = books %>% 
  arrange(desc(ratings_count)) %>% 
  head(10)

most_read_books %>% 
  ggplot(aes(
    x = reorder(title, ratings_count),
    y = ratings_count,
    fill = as.factor(title)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 10)) +
  coord_flip() +  
  ggtitle("Tоп 10 най-четените книги")

# 1.8 Най-често срещаният език
language = books %>%
  group_by(language_code) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

language%>%
  ggplot(aes(
    x = reorder(language_code, -count),
    y = count,
    fill = as.factor(count)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5)) + ggtitle("Най-често срещаният език")

# 1.9 Средната оценка по език
avg_rating_by_language = books %>%
  group_by(language_code) %>%
  summarise(avg_rating = mean(average_rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating))

avg_rating_by_language %>% 
  ggplot(aes(
    x = reorder(language_code, avg_rating),
    y = avg_rating,
    fill = as.factor(language_code)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Средната оценка по език") +
  coord_flip()

# 1.10 Топ 10 езика с най-дългите книги
language_pages = books %>%
  group_by(language_code) %>%
  summarise(avg_pages = mean(num_pages, na.rm = TRUE)) %>%
  arrange(desc(avg_pages)) %>%
  head(10)

language_pages %>%
  ggplot(aes(
    x = reorder(language_code, avg_pages),
    y = avg_pages,
    fill = as.factor(language_code)
  )) +
  geom_col() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Топ 10 езика с най-дългите книги") +
  coord_flip()

# 1.11 Най-често срещаните издателства
publishers = books %>% 
  group_by(publisher) %>% 
  summarise(total_books = n()) %>% 
  arrange(desc(total_books)) %>% 
  head(10) 

publishers %>% 
  ggplot(aes(
    x = reorder(publisher, total_books),
    y = total_books,
    fill =as.factor(total_books)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) + ggtitle("Най-често срещаните издателства") +
  coord_flip()

# 1.12 Авторите с най-голям брой публикувани книги
top_authors_books = books %>% 
  group_by(authors) %>% 
  summarise(total_books = n()) %>% 
  arrange(desc(total_books)) %>% 
  head(10) 

top_authors_books %>% 
  ggplot(aes(
    x = reorder(authors, total_books),
    y = total_books,
    fill =as.factor(total_books)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) + ggtitle("Топ 10 най-продуктивни автори") +
  coord_flip()

# 1.13 Топ 10 най-високо оценените автори
top_authors = books %>%
  filter(average_rating >= 4.3) %>%
  group_by(authors) %>%
  summarise(
    title_count = n(),
    average_rating = mean(average_rating, na.rm = TRUE) 
  ) %>%
  arrange(desc(title_count)) %>%
  head(10) 

top_authors %>%
  ggplot(aes(
    x = reorder(authors, title_count), 
    y = title_count, 
    fill = as.factor(title_count)
  )) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Топ 10 най-високо оценените автори")  +
  coord_flip()

# 1.14 Топ 10 на авторите с най-много рецензии
top_authors_text_reviews = books %>% 
  group_by(authors) %>% 
  summarise(total_reviews = sum(text_reviews_count, na.rm = TRUE)) %>% 
  arrange(desc(total_reviews)) %>% 
  head(10)

top_authors_text_reviews %>% 
  ggplot(aes(
    x = reorder(authors, total_reviews),
    y = total_reviews,
    fill = as.factor(total_reviews)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Топ 10 на авторите с най-много рецензии") +
  coord_flip()

# 1.15 Топ 10 на най-четените автори
top_authors_read_books = books %>% 
  group_by(authors) %>% 
  summarise(total_ratings = sum(ratings_count, na.rm = TRUE)) %>% 
  arrange(desc(total_ratings)) %>% 
  head(10)

top_authors_read_books %>% 
  ggplot(aes(
    x = reorder(authors, total_ratings),
    y = total_ratings,
    fill = as.factor(total_ratings)
  )) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Топ 10 на най-четените автори") +
  coord_flip()

# 2 Разпределение на данните
# 2.1 Как са разпределени средните оценки
ratings_distribution = books %>%
  ggplot(aes(x = average_rating)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "cornflowerblue", color = "black", alpha = 0.6) +
  geom_density(color = "orange", linewidth = 1) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Разпределение на оценките",
    x = "Average Rating",
    y = "Count"
  )
plot(ratings_distribution)

# 2.2  Как са разпределени страниците
pages = books %>%
  ggplot() +
  aes(x = (num_pages)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "cornflowerblue", color = "black", alpha = 0.6) +
  geom_density(color = "orange", size = 1) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) + labs( title = "Разпределение на страниците", x = "Number of Pages", y = "Count")
plot(pages)

# 3. Зависимости
# 3.1 Съществува ли зависимост между рецензиите и оценките?
ratings_reviews_corr = books %>%
  ggplot(aes(x = ratings_count, y = text_reviews_count)) +
  geom_point(alpha = 0.5, color = "cornflowerblue") + 
  geom_smooth(formula = y ~ x,
              method = lm, color = "orange") + 
  theme_minimal() +
  labs(
    title = "Зависимост между броя на рецензиите и броят на оценките",
    x = "Ratings Count",
    y = "Text Reviews Count"
  )
plot(ratings_reviews_corr)

# 3.2 Съществува ли зависимост между рецензиите и средните оценките?
avg_ratings_reviews_corr = books %>%
  ggplot(aes(y = average_rating,
             x = text_reviews_count)) +
  geom_point(alpha = 0.5,color = "cornflowerblue") +
  geom_smooth(formula = y ~ x,
              method = lm, color = "orange") +
  theme_minimal()+
  labs(
    title = "Зависимост между броя на рецензиите и броя на средните оценките",
    x = "Average Ratings",
    y = "Text Reviews Count"
  ) 
plot(avg_ratings_reviews_corr)

# 3.3 Съществува ли зависимост между страниците и средните оценките?
avg_ratings_num_pages_corr = books %>%
  ggplot(aes(y = average_rating,
             x = num_pages)) +
  geom_point(alpha = 0.5,color = "cornflowerblue") +
  geom_smooth(formula = y ~ x,
              method = lm, color = "orange") +
  theme_minimal()+
  labs(
    title = "Зависимост между броя на страниците и броя на средните оценките",
    x = "Average Ratings",
    y = "Number of Pages"
  ) 
plot(avg_ratings_num_pages_corr)

# 3.4 Съществува ли зависимост между оценките и страниците?
ratings_count_num_pages_corr = books %>%
  ggplot(aes(y = ratings_count,
             x = num_pages)) +
  geom_point(alpha = 0.5,color = "cornflowerblue") +
  geom_smooth(formula = y ~ x,
              method = lm, color = "orange") +
  theme_minimal()+
  labs(
    title = "Зависимост между броя на страниците и броя на оценките",
    y = "Ratings Count",
    x = "Number of Pages"
  ) 
plot(ratings_count_num_pages_corr)

# 3.5 Съществува ли зависимост между средните оценки и езиците (Т.е дали средните на средните оценки се различават по език) (Използвам ANOVA) 
anova_avg_language = aov(average_rating ~ language_code, data = books)
summary(anova_avg_language)

ggplot(books, aes(x = language_code, y = average_rating)) +
  geom_boxplot(fill = "cornflowerblue", alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Разпределение на средните оценки по език",
    x = "Language",
    y = "Average Rating"
  )
# Пробвах и с Kruskal-Wallis тест (Това е сякаш по-добрият вариант, защото средните оценки не са нормално разпределени)
kruskal_test_result = kruskal.test(average_rating ~ language_code, data = books)
print(kruskal_test_result)

# 3.6 Съществува ли зависимост между средните оценки и издателствата (Т.е дали средните на средните оценки се различават по издателство) (Използвам ANOVA) 
anova_publisher = aov(average_rating ~ publisher, data = books)
summary(anova_publisher)

ggplot(books %>% filter(publisher %in% publishers$publisher), aes(x = publisher, y = average_rating)) +
  geom_boxplot(fill = "cornflowerblue", alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Разпределение на средните оценки по издателство",
    x = "Publisher",
    y = "Average Rating"
  )

# 4. Тенденции
# 4.1 Как броят на оценките се променя през годините?
rating_trend = books %>%
  group_by(year) %>%
  summarise(total_ratings = sum(ratings_count, na.rm = TRUE)) %>%
  filter(!is.na(year))

rating_trend %>%
  ggplot(aes(x = year, y = total_ratings)) +
  geom_line(color = "cornflowerblue", linewidth = 1) +
  geom_point(color = "orange") +
  theme_minimal() +
  labs(
    title = "Как броят на оценките се променя през годините?",
    x = "Year",
    y = "Total Ratings"
  )

# 4.2 Как броят на рецензиите се променя през годините?
reviews_trend = books %>%
  group_by(year) %>%
  summarise(total_reviews = sum(text_reviews_count, na.rm = TRUE)) %>%
  filter(!is.na(year))

reviews_trend %>%
  ggplot(aes(x = year, y = total_reviews)) +
  geom_line(color = "cornflowerblue", linewidth = 1) +
  geom_point(color = "orange") +
  theme_minimal() +
  labs(
    title = "Как броят на рецензиите се променя през годините?",
    x = "Year",
    y = "Total Text Reviews"
  )

# 5.Тестване на хипотези

# 5.1 t-тестове 
# 5.1.1 Дали средните оценки се различават много от средното?
mu_avg_rating = mean(books$average_rating)
t_test_avg_rating = t.test(books$average_rating, mu = mu_avg_rating)
print(t_test_avg_rating)

## 5.1.2 Сравнение между средните оценки на два езика
language1 = "eng" 
language2 = "fre" 
t_test_languages = t.test(books$average_rating[books$language_code == language1],
                           books$average_rating[books$language_code == language2])
print(t_test_languages)

# 5.2 Сравнение на две десетилетия
books$decade = cut(books$year, breaks = seq(1950, 2020, by = 10), 
                   labels = seq(1950, 2010, by = 10), include.lowest = TRUE)
decade1 = "1960"  
decade2 = "2010"  
welch_t_test_result = t.test(books$average_rating[books$decade == decade1],
                              books$average_rating[books$decade == decade2],
                              var.equal = FALSE)
print(welch_t_test_result)

# 5.3 Chi-squared тестове

# 5.3.1 Зависимост между броя оценки и броя рецензии
books$text_reviews_category = cut(books$text_reviews_count, breaks = 
                                    quantile(books$text_reviews_count, probs = c(0, 0.33, 0.67, 1)), include.lowest = TRUE, labels = c("Low", "Medium", "High"))
chi_square_test_ratings_reviews = chisq.test(table(books$ratings_count, books$text_reviews_category))
print(chi_square_test_ratings_reviews)

# 5.3.2 Зависимост между средните оценки и броя рецензии
chi_square_test_rating_reviews = chisq.test(table(books$average_rating, books$text_reviews_category))
print(chi_square_test_rating_reviews)

# 5.3.3 Зависимост между броя страници и средните оценки
chi_square_test_pages_ratings = chisq.test(table(books$num_pages, books$average_rating))
print(chi_square_test_pages_ratings)

# 5.3.4 Зависимост между броя страници и броя рецензии
chi_square_test_pages_reviews = chisq.test(table(books$num_pages, books$text_reviews_category))
print(chi_square_test_pages_reviews)


# 6. Доверителен интервал за средните оценките
ci_mean_rating = t.test(books$average_rating)$conf.int
print(ci_mean_rating)

# 7. Линейна регресия
# 7.1 Линейна регресия между средните оценки и броя на оценките
lm_avg_rating_vs_ratings_count = lm(average_rating ~ ratings_count, data = books)
summary(lm_avg_rating_vs_ratings_count)

# 7.2 Линейна регресия между средните оценки и броя на страниците
lm_avg_rating_vs_num_pages = lm(average_rating ~ num_pages, data = books)
summary(lm_avg_rating_vs_num_pages)


