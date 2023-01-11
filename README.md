## Проект по тематическому отбору текстов

**Цель:** разработка системы, выполняющей отбор (рекомендацию) текстов по заданной теме.

**Перспективы использования** – поиск дубликатов тегов с помощью средств NER, обнаружение лишних тематических тегов, проверка тематического соответствия заголовков, тегов и выявленных ключевых слов, создание новых тегов.

**Язык:** русский.

**Уточнение цели:** информационные и коммерческие ресурсы в интернете как правило придерживаются определенной редакционной политики. Например, для публикации на новостных сайтах отбирается фактическая информация, не содержащая рекламы и лишенная оценочной составляющей (пример – ICT.Moscow).

В свою очередь, электронные торговые площадки отказываются от информации о времени появления товара на рынке, чтобы описание продукта не теряло актуальности, а также избегают информации, поданной от лица производителя, поскольку выступают исключительно в качестве продавца (пример – Amazon, Ozon, BestBuy).

В обоих случаях редакторы отказываются от текстовых данных, не несущих нужной информации. Предполагается разработать систему, которая выполнит следующие задачи: формирование текстового корпуса, тематический анализ текстов (TF-IDF, Word2Vec, Navec), отбор текстов, соответствующих заданной пользователем теме. Выделение в текстах информации, непосредственно относящейся к заданной теме. Предполагается сравнить полученные данные с тегами или заголовками на сайте для тестирования разработанной системы, а также для выявления недостатков тегирования или оглавления. Соотнесение данных возможно с помощью методов кластерного анализа с последующей визуализацией и формулировкой заключения.

**Альтернативный вариант цели:** выделение в проанализированных текстах слов и выражений, не соответствующих редакционной политике издания (например, коллокации, характерные для форм первого лица и указания времени в маркетинговых описаниях или экспрессивно окрашенная и рекламная лексика в новостных текстах)

**Аналитика:** для решения поставленной задачи – выделения лишней информации в тексте используются макросы для среды Microsoft Word, а также используются

**Ссылки:** 

**извлечение ключевых слов**
https://habr.com/ru/post/468141/
https://vc.ru/newtechaudit/449493-algoritmy-dlya-vydeleniya-klyuchevyh-slov-rake-yake-textrank
https://habr.com/ru/company/surfingbird/blog/301922/

**тематическое моделирование**
https://habr.com/ru/post/334668/
https://habr.com/ru/post/470618/
https://habr.com/ru/company/otus/blog/503398/


https://towardsdatascience.com/unsupervised-nlp-topic-models-as-a-supervised-learning-input-cf8ee9e5cf28
https://towardsdatascience.com/introduction-to-nlp-part-5a-unsupervised-topic-model-in-python-733f76b3dc2d
https://towardsdatascience.com/introduction-to-nlp-part-5a-unsupervised-topic-model-in-python-733f76b3dc2d
https://towardsdatascience.com/the-ultimate-guide-to-clustering-algorithms-and-topic-modeling-3a65129df324
https://medium.com/grabngoinfo/zero-shot-topic-modeling-with-deep-learning-using-python-a895d2d0c773

https://aclanthology.org/R19-1159/
https://aclanthology.org/D09-1026/
https://aclanthology.org/2020.acl-main.73/

**анализ тональности**
https://habr.com/ru/company/vk/blog/516214/

**проект Natasha**
https://habr.com/ru/post/516098/
https://habr.com/ru/post/349864/

**Текстовые корпуса**
библиотека проекта Natasha https://natasha.github.io/corus/
открытый датасет Lenta.ru https://github.com/yutkin/Lenta.Ru-News-Dataset
корпус Taiga https://tatianashavrina.github.io/taiga_site/
датасет РИА Новости https://github.com/RossiyaSegodnya/ria_news_dataset

**Navec - коллекция предобученных эмбеддингов проекта Natasha**
https://github.com/natasha/navec

**РуСентиЛекс – электронный словарь экспрессивно окрашенной лексики**
https://www.labinform.ru/pub/rusentilex/index.htm


**Данные:**
Корпус собирается из материалов сайта ICT.Moscow: описаний мероприятий в календаре или новостных материалов.
Объем корпуса составляет 200 текстов. Тексты загружаются с помощью парсера или предоставляются редакцией в формате CSV.
Препроцессинг: токенизация, удаление пунктуации, удаление стоп-слов.

**Лингвистический компонент**

**Входные данные:** файл CSV, содержащий таблицу с текстами, подлежащими анализу (колонки: заголовок, дата, теги, текст), запрашивается пользовательский ввод - одно или несколько ключевых слов для поиска соответствий.

**Выходные данные:** файл CSV, содержащий таблицу с текстами, соответствующие заданным ключевым словам; В каждом тексте специальными символами или маркерами выделяется искомая информация. (Колонки: заголовок, дата, тема, оригинальные теги, текст)

**Тестирование:** все публикации на сайте ICT.Moscow сопровождаются тегами и заголовками. Таким образом, пользователь имеет возможность сравнить соответствие оригинальных тегов, заголовков и выделенных системой ключевых слов (возможно оценить это соответствие с помощью Word2Vec).

**Использование технических навыков:** сбор собственного корпуса, препроцессинг текстов, построение частотных списков, сравнение элементов списков и выделение ключевых слов по метрике TF-IDF, проверка наличия заданных лексем в частотном списке и в списке ключевых слов, а также в соответствующих заголовках и тегах, оценка соответствия выделенных ключевых слов тегам и заголовкам (с помощью Word2Vec).


**Порядок работы:**

- Разработка парсера (загрузчика текстов) или подбор текстов на сайте ICT.Moscow
- Создание текстового корпуса (мероприятия или новости)
- Разработка блока выделения ключевых слов текста на основе частотного анализа с помощью метрики TF-IDF
- Разработка блока выделения темы или фильтрующего блока на основе модели Word2Vec
- Отладка приложения, разработка интерфейса
- Тестирование приложения в работе с календарем или новостной лентой

