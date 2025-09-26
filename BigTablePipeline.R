library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(sf)
library(hms)
library(DBI)
library(RPostgres)
dotenv::load_dot_env()

dbicon <- dbConnect(
  RPostgres::Postgres(),
  dbname = "ivnis",
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

# Функция для добавления пробела после ".", если пробела нет
add_space_after_dot = function(text) {
  gsub("(\\.)([^ ])", "\\1 \\2", text)
}

# Замена множества пробелов подряд на один
remove_multispaces = function(text) {
  gsub("\\s+", " ", text)
}

remove_dots = function(text) {
  gsub("\\.", "", text)
}

remove_coma = function(text) {
  gsub(",", "", text)
}

remove_more_less_signs = function(text) {
  gsub("<", "", text)
  gsub(">", "", text)
}

fix_river_pattern = function(text) {
  # Заменяем "Б (русло)" на "Б, русло"
  # Регулярное выражение:
  # ([А-Яа-яA-Za-z]) - любая буква (русская или английская)
  # \\s*\\(русло\\) - пробелы (если есть) + "(русло)"
  gsub("([А-Яа-яA-Za-z])\\s*\\(русло\\) ", "\\1, русло, ", text)
}

process_brackets = function(text) {
  # Шаг 1: Добавляем запятую перед открывающей скобкой, если её нет
  text <- gsub("([^,])\\s*\\(", "\\1, (", text)
  
  # Шаг 2: Удаляем все скобки
  text <- gsub("[\\(\\)]", "", text)
  
  # Шаг 3: Удаляем возможные двойные запятые и лишние пробелы
  text <- gsub("\\s*,\\.*", ", ", text)  # Заменяем множественные запятые на одну
  text <- gsub("\\s+", " ", text)         # Удаляем лишние пробелы
  text <- gsub("^\\s+|\\s+$", "", text)   # Удаляем пробелы в начале/конце
  
  # Шаг 4: Удаляем запятую в конце строки, если она есть
  text <- gsub(",\\s*$", "", text)
  
  return(text)
}


big_table <- read_excel(
  input_file,
  skip = skip_lines,
  na = na_strings,
  col_types = c('text', 'text', 'date', 'date', rep('guess', 133)))

big_table = big_table |> 
  mutate(`Название створа` = `Название створа` |> 
           add_space_after_dot() |> 
           remove_multispaces() |> fix_river_pattern() |> process_brackets()) |> 
  mutate(`Координаты` = `Координаты` |> remove_coma() |> remove_multispaces())

# Remove rows where Координаты is NA
big_table = big_table |> 
  filter(!is.na(`Координаты`))


stations_table = big_table[, c('Название створа', 'Координаты')]

names(stations_table) = c('name_station','coords')

stations_table = stations_table |> 
  distinct(name_station, .keep_all = TRUE) |> distinct(coords, .keep_all = TRUE) |> separate(coords, into = c("Y", "X"), sep = " ")

# Add PK and ID columns
stations_table = stations_table |> 
  mutate(
    PK = row_number(),  # Create a primary key (unique row identifier)
    ID_station = row_number()  # Create an ID column (unique row identifier)
  ) |> 
  select(PK, ID_station, everything())


samples_table = big_table

# Add PK and ID columns
samples_table = samples_table |> 
  mutate(
    PK = row_number(),  # Create a primary key (unique row identifier)
    ID_sample = row_number(),  # Create an ID column (unique row identifier)
    ID_condition = row_number()
  ) |> 
  select(PK, ID_sample, ID_condition, everything())


sample_conditions = samples_table |> 
  select(PK, ID_condition, everything()[15:ncol(samples_table)])

samples_table = samples_table |> 
  select(1:14) |> separate(`Координаты`, into = c("Y", "X"), sep = " ")


samples_table = samples_table |> 
  left_join(stations_table[, c('name_station', 'ID_station')], by = c('Название створа' = 'name_station')) %>% 
  rows_patch(
    left_join(
      filter(., is.na(ID_station)) %>% select(-ID_station),
      stations_table %>% select(X, Y, ID_station),
      by = c("X" = "X", "Y" = "Y")
    )
  )


samples_table = samples_table |> select(-X, -Y, -`Название створа`)


values_table = sample_conditions |> 
  select(PK, ID_condition, everything()[10:ncol(sample_conditions)])

values_table = values_table |> select(2:(ncol(values_table)), -`Примечания`)

sample_conditions = sample_conditions |> 
  select(1:9, `Примечания`)


# Таблица с основными значениями (все остальные столбцы)
table_main = values_table %>%
  select(ID_condition, `рН лаб погр. 0,20`, !matches("погр")) |> select(!matches("ПДК"))

# Таблица с погрешностями (столбцы, содержащие "погр" в названии)
table_error = values_table %>%
  select(ID_condition, -`рН лаб погр. 0,20`, matches("погр")) |> select(!matches("ПДК"))


table_pdk <- values_table %>%
  select(ID_condition, matches("ПДК")) |> select(!matches("погр"))


table_main = table_main |> 
  mutate(across(2:ncol(table_main), as.character))

table_error = table_error |> 
  mutate(across(2:ncol(table_error), as.character))

table_pdk = table_pdk |> 
  mutate(across(2:ncol(table_pdk), as.character))

table_main = table_main |> pivot_longer(cols = 2:ncol(table_main), names_to = "value_type", values_to = "value")
table_error = table_error |> pivot_longer(cols = 2:ncol(table_error), names_to = "error_type", values_to = "error")
table_pdk = table_pdk |> pivot_longer(cols = 2:ncol(table_pdk), names_to = "pdk_type", values_to = "PDK")

# Создаем очищенную копию столбца
table_main = table_main %>% 
  mutate(
    value_tech = value_type %>% 
      str_replace_all("Са,", "Ca,") |> 
      str_remove_all("мг/л|погр\\.|мгО2/л|мгО/л") %>%  # Удаляем "мг/л" и "погр."
      str_remove_all("\\s|,|\\.") |>              # Удаляем пробелы и запятые
      str_replace_all("Сr", "Cr") |> 
      str_remove_all("фильтрград|мг-экв/дм3") |> 
      str_remove_all("-3мгРО4-3/лфильтр")
    
  )

# Создаем очищенную копию столбца
table_error = table_error %>% 
  mutate(
    error_tech = error_type %>% 
      str_replace_all("К,", "K,") |> 
      str_replace_all("Са,", "Ca,") |> 
      str_replace_all("ХПK", "ХПК") |> 
      str_remove_all("мг/л|погр\\.|мгО2/л|погр") %>%  # Удаляем "мг/л" и "погр."
      str_remove_all("\\s|,|\\.")             # Удаляем пробелы и запятые
  )

values_table = table_main |> 
  full_join(table_error, by = c('value_tech' = 'error_tech', 'ID_condition' = 'ID_condition')) |> 
  mutate(PK = row_number()) |> select(PK, everything())

# Обработка данных
table_pdk <- table_pdk %>%
  distinct(pdk_type, .keep_all = TRUE) |> 
  mutate(
    pdk_type = pdk_type |> str_remove_all(", ПДК|ПДК ")
  ) |> 
  mutate(
    # Извлекаем название (все до "ПДК")
    PDK_name = str_extract(pdk_type, "^.*?(?=,? \\d)"),
    # Извлекаем значение ПДК (число с запятой/точкой)
    PDK_max = case_when(
      str_detect(pdk_type, "кислород") ~ NA_character_,
      TRUE ~ str_extract(pdk_type, "\\d+[,\\.]\\d+")),
    # Извлекаем единицы измерения (мг/л или ...)
    unit = case_when(
      str_detect(pdk_type, "мг/л") ~ "мг/л",
      str_detect(pdk_type, "\\.\\.\\.") ~ "undefined",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(PDK_min = case_when(
    str_detect(pdk_type, "кислород") ~ str_extract(pdk_type, "\\d+[,\\.]\\d+"),
    TRUE ~ NA_character_)) |> 
  # Заменяем запятые на точки в числах
  mutate(PDK_min = as.numeric(str_replace(PDK_min, ",", ".")),
         PDK_max = as.numeric(str_replace(PDK_max, ",", "."))) %>%
  # Выбираем нужные столбцы
  select(PDK_name, PDK_min, PDK_max, unit)

table_pdk = table_pdk |> 
  mutate(
    pdk_tech = PDK_name %>% 
      str_remove_all("\\(высшая, I, II кат.\\)|\\(высшая, I категория\\)|\\(II категория\\)|-3") |> 
      str_remove_all("мг/л|погр\\.|мгО2/л|мгО/л") %>%
      str_remove_all("\\s|,|\\.") |>   
      str_replace_all("Сr", "Cr") |> str_replace_all("Fe", "Feраств") |> 
      str_replace_all("Раствкислород", "КислородВинклер")
  )



values_types = values_table["value_type"] |> distinct(value_type, .keep_all = TRUE)

values_types = values_types |> 
  mutate(
    PK = row_number(),
    ID_value = row_number(),
    value_type = value_type,
    unit = NA,
    min_enable = NA,
    max_enable = NA,
    group = NA
  )

values_types = values_types |>
  mutate(
    group = case_when(
      row_number() <= 12 ~ "Зондирование",
      row_number() <= 57 ~ "Главные ионы",
      TRUE ~ "Металлы"  # Default for rows beyond 25
    )
  )

values_table = values_table |> 
  left_join(values_types[, c('value_type', 'ID_value')], by = c('value_type' = 'value_type')) 

table_pdk = table_pdk |> 
  left_join(values_table[, c('value_tech', 'ID_value')], by = c('pdk_tech' = 'value_tech')) |> 
  distinct(PDK_name, .keep_all = TRUE) |> 
  mutate(PK = row_number()) |> 
  select(PK, ID_value, PDK_name, PDK_min, PDK_max, unit)


values_table = values_table |> 
  select(PK, ID_condition, ID_value, value, error)


stations = stations_table |> 
  mutate(X = as.numeric(X),
         Y = as.numeric(Y),
         geometry = st_as_sfc(paste0("POINT(", X, " ", Y, ")")))

samples = samples_table |> 
  mutate(person = `Пробоотборщик`,
         editor = `Занес в систему`,
         date = as.Date(`Дата`),
         time = as_hms(format(`Время, час,мин`, "%H:%M:00")),
         equipment = as.character(`Приборы`),
         cipher = `Шифр пробы`,
         number = as.integer(`№ пробы, присв. ИЛ`),
         horiz = `Горизонт отбора, Н, м`,
         volume = `V/масса, литр/кг\r\n`
  ) |> 
  select(PK, ID_sample, ID_condition, person, editor, date, time, ID_station, equipment, cipher, number, horiz, volume)

sample_conditions = sample_conditions |> 
  mutate(temp_air = `Температура воздуха, С`,
         wind = `Ветер, м/с`,
         humidity = `Влажность, %`,
         pressure = `Давление, hРа`,
         el_conduct = as.numeric(`Электропр_поле, мкСм/см`),
         el_conduct_25 = `Электропр_(25), мкСм/см`,
         el_conduct_25_pogr = `Электропр_(25), погр.`,
         notes = `Примечания`
  ) |> 
  select(PK, ID_condition, temp_air, wind, humidity, pressure, el_conduct, el_conduct_25, el_conduct_25_pogr, notes)

values = values_table |> 
  mutate(value = as.numeric(value |> remove_multispaces() |> remove_more_less_signs()),
         error = as.numeric(error |> remove_multispaces() |> remove_more_less_signs())
  ) |> 
  select(PK, ID_condition, ID_value, value, error)

values_types = values_types |> 
  mutate(
    unit = case_when(
      str_detect(value_type, "мг/л|мгО2/л|мгРО4-3/л|мгО/л") ~ "мг/л",
      str_detect(value_type, ", С") ~ "С",
      str_detect(value_type, " %") ~ "%",
      str_detect(value_type, ", мм") ~ "мм",
      str_detect(value_type, "рН") ~ "pH",
      str_detect(value_type, "мг-экв/дм3") ~ "мг-экв/дм3",
      str_detect(value_type, "град") ~ "град",
      TRUE ~ NA_character_
    ),
    min_enable = as.numeric(min_enable),
    max_enable = as.numeric(max_enable)
  ) |> 
  mutate(value_type = value_type |> 
           str_replace_all("Са,", "Ca,") |> 
           str_remove_all(", мг/л|, мгО2/л|, мгО/л") %>%  # Удаляем "мг/л" и "погр."
           str_replace_all("Сr", "Cr") |> 
           str_remove_all(", град|мг-экв/дм3") |> 
           str_remove_all(", мгРО4-3/л") |> 
           str_replace_all("Жестк.,", "Жёсткость")
  ) |> 
  select(PK, ID_value, value_type, unit, min_enable, max_enable, group)

DBI::dbWriteTable(dbicon, "stations", stations, create = FALSE, append = TRUE)
DBI::dbWriteTable(dbicon, "samples", samples, create = FALSE, append = TRUE)
DBI::dbWriteTable(dbicon, "sample_conditions", sample_conditions, create = FALSE, append = TRUE)
DBI::dbWriteTable(dbicon, "values", values, create = FALSE, append = TRUE)
DBI::dbWriteTable(dbicon, "values_types", values_types, create = FALSE, append = TRUE)
DBI::dbWriteTable(dbicon, "table_pdk", table_pdk, create = FALSE, append = TRUE)