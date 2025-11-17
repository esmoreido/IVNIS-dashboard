library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
# library(leaflet)
library(dygraphs)
library(dotenv)
library(htmltools)
library(RPostgreSQL)
library(DT)
library(xts)
library(readxl)
# library(terra)
library(dplyr)
library(tidyr)
library(lubridate)
dotenv::load_dot_env()

getDbStats <- function(con) {
  # Проверяем соединение с БД
  if (!DBI::dbIsValid(con)) {
    stop("Соединение с базой данных не активно")
  }
  
  tryCatch({
    # Получаем размер базы данных (в МБ)
    db_size <- if (inherits(con, "PqConnection")) {  # PqConnection
      # Для PostgreSQL
      size_query <- "SELECT pg_size_pretty(pg_database_size(current_database())) as size"
      result <- dbGetQuery(con, size_query)
      if (nrow(result) > 0 && !is.na(result$size[1])) {
        result$size[1]
      } else {
        "N/A (не удалось получить размер)"
      }
    } else {
      "N/A (используется RPostgres?)"
    }
    
    # Получаем количество уникальных записей в основной таблице
    record_count <- tryCatch({
      count <- dbGetQuery(con, "SELECT COUNT(DISTINCT \"PK\") as count FROM values")$count
      format(count, big.mark = " ", scientific = FALSE)
    }, error = function(e) {
      "N/A"
    })
    
    # Возвращаем список с двумя значениями
    list(db_size, record_count)
    
  }, error = function(e) {
    # В случае ошибки возвращаем значения по умолчанию
    list("N/A", "N/A")
  })
}


# Функция для получения списка станций в виде UI элемента
get_station_list_ui <- function() {
  # Подключаемся к БД и выполняем запрос
  conn <- dbConnect(
    RPostgres::Postgres(),
    dbname = "ivnis",
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS")
  )
  
  tryCatch({
    stations <- dbGetQuery(conn, "SELECT name_station FROM stations")
    
    # Создаем UI элемент - выпадающий список с множественным выбором
    renderUI({
      selectInput(
        inputId = "ui_stations",
        label = "Выберите станции:",
        choices = stations$name_station, 
        width = "300px",
        multiple = TRUE,
        selectize = TRUE
      )
    })
  }, finally = {
    dbDisconnect(conn)
  })
}

# Функция для получения списка переменных для графиков
get_plot_vars <- function() {
  conn <- dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS")
  )
  
  tryCatch({
    vars <- dbGetQuery(conn, "SELECT value_type FROM public.values_types")
    
    renderUI({
      selectizeInput(
        inputId = "pick_var",
        label = "Выберите показатели:",
        choices = vars$value_type,
        multiple = TRUE,
        options = list(placeholder = 'Выберите один или несколько показателей')
      )  # Добавлена закрывающая скобка для selectizeInput
    })
  }, finally = {
    dbDisconnect(conn)
  })
}

