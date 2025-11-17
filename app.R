library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
# library(leaflet)
library(dotenv)
library(dygraphs)
library(htmltools)
library(RPostgreSQL)
library(DT)
library(xts)
library(readxl)
# library(terra)
library(dplyr)
library(tidyr)
library(lubridate)
source('funs.R')
dotenv::load_dot_env()

stl <- "display:inline-block; vertical-align:top"

ui <- dashboardPage(skin = 'red', 
                    dashboardHeader(title = "IvankovoNIS"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Обзор", tabName = "dashboard", icon = icon("map-location-dot")), # fontawesome.com
                        menuItem("Загрузка", tabName = "upload", icon = icon("arrow-up-from-bracket")),
                        menuItem("Просмотр", tabName = "explore", icon = icon("chart-line"))
                        # menuItem("Обслуживание", tabName = "edit", icon = icon("cog"))
                        # tags$div(class = 'sticky_footer', 
                        #          tags$a(href="href=\"https://www.iwp.ru/about/structure/otdel-gidrologii-rechnykh-basseynov/laboratoriya-gidrologii-navodneniy/\"", 
                        #                 HTML("&copy; 2023-24 Лаборатория 
                        #                 гидроинформатики
                        #                 ИВП РАН"))
                        # )
                      )
                    ),
                    dashboardBody(
                      includeCSS("www/ivnis.css"),
                      tabItems(
                        # дэшборд ----
                        tabItem("dashboard",
                                fluidRow(
                                  valueBoxOutput("num_stat"),
                                  valueBoxOutput("dbsize"),
                                  valueBoxOutput("users")
                                ),
                                fluidRow(
                                  box(title = 'Карта расположения станций и постов', 
                                      solidHeader = TRUE, height = 'auto',
                                      htmlOutput('stations_iframe_map')),
                                  box(title = "Перечень станций", solidHeader = TRUE,
                                      dataTableOutput("st_table"))
                                ),
                        ),
                        tabItem("upload",
                                tabsetPanel(
                                  # метеостанции ----
                                  tabPanel(title = "Загрузка протокола измерений", icon = icon("pen-to-square"),
                                           fluidPage(
                                             shinyjs::useShinyjs(),
                                             shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                                             titlePanel("Загрузка файлов с метеостанций"),
                                             wellPanel(id = 'inputFile', 
                                                       div(style = stl, fileInput("file1", "Выбрать файл",
                                                                                  multiple = FALSE, width = '350px',
                                                                                  accept = c("text/csv",".xls",
                                                                                             "text/comma-separated-values,text/plain",
                                                                                             ".csv",
                                                                                             ".xlsx"),
                                                                                  buttonLabel = "Выбрать...",
                                                                                  placeholder = "Файл не выбран")),
                                                       div(style = stl, uiOutput('ui_st_import')),
                                                       # checkboxInput(inputId = "amdate", 
                                                       #               label = "Формат даты и времени мм/дд/гг 12 ч.", 
                                                       #               value = F
                                                       # ),
                                                       # checkboxInput(inputId = "amunit", 
                                                       #               label = "Неметрические единицы (°F, in.)", 
                                                       #               value = F
                                                       # ), 
                                                       # checkboxInput(inputId = "pres_mm", 
                                                       #               label = "мБар в мм.рт.ст.", 
                                                       #               value = F
                                                       # ),
                                                       # checkboxInput(inputId = "soil_data", 
                                                       #               label = "Есть температура и влажность почвы", 
                                                       #               value = F
                                                       # ),
                                                       # Horizontal line 
                                                       tags$hr(),
                                                       
                                                       # Insert button 
                                                       actionButton("insert_df", "Загрузить", icon = icon("arrow-up-from-bracket")),
                                                       actionButton("reset1", "Очистить",icon = icon("broom")),
                                                       h2(textOutput("qry", inline = T))
                                             ),
                                             wellPanel(id = 'mainTable', style = "overflow-y:scroll; max-height: 600px",
                                                       # Вывод таблицы и результатов добавления ----
                                                       div(h3(textOutput('weather_warning'))),
                                                       div(dataTableOutput("contents"), style = "font-size:80%")
                                             )
                                             
                                             
                                           )),
                                  # гидропосты ----
                                  tabPanel(
                                    title = "Загрузка единой таблицы измерений", icon = icon("water"),
                                    fluidPage(
                                      shinyjs::useShinyjs(),
                                      shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                                      titlePanel("Загрузка единой таблицы измерений"),
                                      wellPanel(id = 'inputFileHobo',
                                                div(style = stl, 
                                                    fileInput("file2", "Выбрать файл",
                                                              multiple = FALSE, width = '350px',
                                                              accept = c("text/csv",".xls",
                                                                         "text/comma-separated-values,text/plain",
                                                                         ".csv",
                                                                         ".xlsx"),
                                                              buttonLabel = "Выбрать...",
                                                              placeholder = "Файл не выбран")),
                                                div(style = stl, 
                                                    uiOutput('ui_hobo_import')),
                                                # div(radioButtons(inputId = "hobo_xls",
                                                #                  label = "Формат файла", inline = T,
                                                #                  choices = c('txt/csv'='2', 'excel'='1'))),
                                                # Horizontal line 
                                                tags$hr(),
                                                
                                                # Insert button 
                                                actionButton("insert_hobo_df", "Загрузить", icon = icon("arrow-up-from-bracket")),
                                                actionButton("reset4", "Очистить", icon = icon("broom")),
                                                h2(textOutput("qry_hobo", inline = T))
                                      ),
                                      wellPanel(id = 'mainTable', 
                                                h3(textOutput('hobo_warning')),
                                                # Вывод таблицы и результатов добавления c HOBO ----
                                                div(style = "overflow-y:scroll; max-height: 600px",
                                                    DTOutput("contents_hobo"), 
                                                    style = "font-size:80%")
                                      )
                                      
                                      
                                    )
                                  ))
                        ),
                        # просмотр ----
                        tabItem("explore",
                                fluidPage(
                                  shinyjs::useShinyjs(),
                                  shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                                  
                                  # Заголовок
                                  titlePanel("Просмотр данных"),
                                  wellPanel(
                                    div(style = stl, uiOutput('ui_stations')),
                                    div(style = stl, uiOutput('ui_var')),
                                    div(style = stl, selectInput('aggregate', 'Осреднение', width = '250px',
                                                                 choices = c('Без осреднения'='none', '1 месяц'='1 month', 
                                                                             '2 месяца'='2 months', '3 месяца'='3 months',
                                                                             '6 месяцев'='6 months', '1 год'='1 year'))),
                                    div(style = "display:block;", 
                                        actionButton("plot_graph", "Создать", icon = icon("chart-line")),
                                        downloadButton('download',"Скачать таблицу"),
                                        actionButton("reset2", "Очистить", icon = icon("broom")))
                                  ),
                                  wellPanel(
                                    # div(style='overflow: scroll; max-height: 600px', 
                                    #     plotOutput('plotdata')),
                                    div(style='overflow: scroll', 
                                        htmlOutput('plotdata', width='100%')),
                                    div(dataTableOutput("datatable"), style = "font-size:80%; overflow-y:scroll; max-height: 600px")
                                  )
                                )
                        )
                      )
                    )
)

server <- function(input, output, session) {
  # source('source/helpers_funs.R', local = T, encoding = 'UTF-8')
  
  # Перезагрузка приложения с кнопки ----
  observeEvent(c(input$reset1,input$reset2,input$reset3, input$reset4), {
    shinyjs::js$refresh_page()
  }, ignoreNULL = T, ignoreInit = T)
  
  # Соединение с БД ----
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "ivnis",
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS")
  )
  
  # Обработчик кнопки "Загрузить"
  observeEvent(input$insert_hobo_df, {
    # Проверяем, что файл загружен
    req(input$file2)
    
    tryCatch({
      # Показываем индикатор загрузки
      showModal(modalDialog("Идет обработка данных...", footer = NULL))
      
      # # Создаем временное окружение для выполнения скрипта
      # script_env <- new.env()
      # 
      # # Передаем параметры в окружение скрипта
      # script_env$input_file <- input$file2$datapath
      # script_env$skip_lines <- 1
      # script_env$na_strings <- '-'
      # 
      # # Выполняем скрипт в созданном окружении
      # source("BigTablePipeline.R", local = script_env)
      # 
      # # Получаем результат из окружения
      # big_table <- script_env$big_table
      # 
      # # Далее можно использовать big_table в вашем приложении
      # # Например, сохранить в реактивное значение:
      # values$big_table <- big_table
      
      # Закрываем индикатор загрузки
      removeModal()
      
      showNotification("Данные успешно загружены", type = "message")
      
    }, error = function(e) {
      removeModal()
      showNotification(paste("Ошибка при загрузке:", e$message), type = "error")
    })
  })
  
  # Таблица станций ----
  stations_df <- reactive({
    q <- gsub("[\r\n\t]", "", 
              "SELECT name_station, \"X\", \"Y\" FROM stations")
    pts <- dbGetQuery(con, q)
    return(pts)
  })
  
  # Верхние картинки ----
  output$num_stat <- renderValueBox({
    valueBox(
      value = nrow(stations_df()),
      subtitle = "Количество станций в БД",
      icon = icon("area-chart"),
      # color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
    )
  })
  
  output$dbsize <- renderValueBox({
    valueBox(
      value = getDbStats(con)[[1]],
      subtitle = "Текущий объём БД",
      icon = icon("download")
    )
  })
  
  output$users <- renderValueBox({
    valueBox(
      value = getDbStats(con)[[2]],
      "Уникальных записей в БД",
      icon = icon("users")
    )
  })
  
  
  # Таблица с перечнем станций ----
  output$st_table <- renderDT(stations_df(), 
                              colnames = c('Название', 'Долгота, °', 
                                           'Широта, °'),
                              options = list(pageLength = 10,
                                             language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json")))
  
  # Карта расположения станций (пока не работает на сервере) ----

  
  output$stations_iframe_map <- renderUI({
    tags$iframe(src ="https://wshydro.nextgis.com/resource/663/display/tiny?angle=0&zoom=8&styles=656%2C728%2C708%2C672&linkMainMap=true&events=false&panel=none&controls=&panels=&base=basemap_0&lon=36.0354&lat=56.8787", 
                style="overflow:hidden;height:500px;width:700px", height="500", width="700")
  })
  
  
  # Панель загрузки данных ----
  
  # Метеоданные ----
  
  # Получение из БД списка метеостанций для добавления в таблицу ----
  
  # Таблица данных с метеостанции ----
  input_df <- reactive({
    req(input$file1)
    output$weather_warning <- validate(need(tools::file_ext(input$file1$datapath) %in% c("csv", "txt", "asc", "xls", "xlsx"), 
                                            "Пожалуйста, загрузите текстовый файл (txt, csv, asc, xls, xlsx)"),
                                       # need(input$station_id != '', 'Выберите название станции!')
    )
    
    tryCatch({
      # Чтение файла в зависимости от расширения
      ext <- tools::file_ext(input$file1$datapath)
      
      if (ext == "csv") {
        df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      }
      else if (ext == "xls") {
        df <- read_xls(input$file1$datapath)
      }
      else if (ext == "xlsx") {
        df <- read_xlsx(input$file1$datapath)
      }
      else { # Для txt и asc
        df <- read.delim(input$file1$datapath, stringsAsFactors = FALSE)
      }
      
      # Дополнительная проверка, что данные были прочитаны
      validate(
        need(is.data.frame(df) && ncol(df) > 0, 
             "Не удалось прочитать данные из файла")
      )
      
      return(df)
      
    }, error = function(e) {
      # В случае ошибки возвращаем пустой data.frame
      return(data.frame())
    })
  })
  
  # Вывод таблицы с загруженным файлом для просмотра ----
  output$contents <- renderDT(
    input_df(), 
    options = list(pageLength = 10, 
                   language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
  )
  
  # Гидроданные ----
  # Получение из БД списка логгеров для добавления в таблицу ----
  
  # Таблица данных с логгера ----
  input_df_hobo <- reactive({
    req(input$file2)
    
    output$hobo_warning <-   validate(
      need(tools::file_ext(input$file2$datapath) %in% c("csv", "txt", "asc", "xls", "xlsx"), 
           "Пожалуйста, загрузите текстовый файл (txt, csv, asc, xls, xlsx)"
      )
                                    # need(input$station_hobo_id != '', 'Выберите название станции!')
    )
    
    tryCatch({
      # Чтение файла в зависимости от расширения
      ext <- tools::file_ext(input$file2$datapath)
      
      if(ext == "csv") {
        df <- read.csv(input$file2$datapath, stringsAsFactors = FALSE)
      }
      else if (ext == "xls") {
        df <- read_xls(input$file1$datapath)
      }
      else if (ext == "xlsx") {
        df <- read_xlsx(input$file1$datapath)
      }
      else { # Для txt и asc
        df <- read.delim(input$file2$datapath, stringsAsFactors = FALSE)
      }
      
      # Дополнительная проверка, что данные были прочитаны
      validate(
        need(is.data.frame(df) && ncol(df) > 0, 
             "Не удалось прочитать данные из файла")
      )
      
      return(df)
      
    }, error = function(e) {
      # В случае ошибки возвращаем пустой data.frame
      return(data.frame())
    })
  })
  
  # Вывод таблицы с загруженным файлом HOBO для просмотра ----
  output$contents_hobo <- renderDT(
    input_df_hobo(), 
    options = list(pageLength = 10,
                   language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
  )
  
  # Для панели графики ---- 
  # Получение из БД списка станций для графики ----
  output$ui_stations <- get_station_list_ui()
  
  # Получение из БД списка переменных ----
  output$ui_var <- get_plot_vars()
  
    
  plot_df <- reactive({
    req(input$ui_stations, input$pick_var, input$aggregate)
    
    q <- paste0("
      SELECT 
        stations.name_station, 
        values_types.value_type, 
        samples.date, 
        values.value
      FROM values
      JOIN values_types ON values.\"ID_value\" = values_types.\"ID_value\"
      JOIN samples ON values.\"ID_condition\" = samples.\"ID_condition\"
      JOIN stations ON samples.\"ID_station\" = stations.\"ID_station\"
      WHERE values_types.value_type IN ('", paste0(input$pick_var, collapse = "', '"), "')
      AND stations.name_station IN ('", paste0(input$ui_stations, collapse = "', '"), "')
      AND values.value IS NOT NULL
      AND trim(values.value::text) != ''
      ORDER BY samples.date")
    
    tryCatch({
      df <- dbGetQuery(con, q)
      
      if(nrow(df) == 0) {
        showNotification("Нет данных для выбранных параметров", type = "warning")
        return(NULL)
      }
      
      # Преобразуем value в числа (если есть запятые как десятичные разделители)
      df$value <- as.numeric(gsub(",", ".", df$value))
      
      # Дополнительная фильтрация NA значений после преобразования
      df <- df %>% filter(!is.na(value))
      
      df$datetime <- as.POSIXct(df$date)
      
      # Аггрегация данных
      if(input$aggregate != 'none'){
        df <- df %>%
          group_by(name_station, value_type, 
                   datetime = lubridate::floor_date(datetime, input$aggregate)) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')
      }
      
      return(df)
      
    }, error = function(e) {
      showNotification(paste("Ошибка при загрузке данных:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # График ----
  # В server части обновите блок построения графиков:
  observeEvent(input$plot_graph, {
    output$plotdata <- renderUI({
      req(input$pick_var, input$ui_stations)
      withProgress(message = "Построение графиков...", value = 0, {
        df <- plot_df()
        if(is.null(df) || nrow(df) == 0) {
          return(div("Нет данных для отображения", style = "color:red; padding:20px;"))
        }
        
        # Создаем отдельный график для каждого показателя
        plot_list <- lapply(input$pick_var, function(current_var) {
          # Фильтруем данные по текущему показателю
          var_data <- df %>% 
            filter(value_type == current_var) %>%
            filter(name_station %in% input$ui_stations)
          
          if(nrow(var_data) == 0) return(NULL)
          
          # Создаем временной ряд для каждой станции
          station_data <- lapply(input$ui_stations, function(station) {
            station_df <- var_data %>% 
              filter(name_station == station) %>%
              select(datetime, value)
            
            if(nrow(station_df) > 0) {
              ts <- xts::as.xts(station_df$value, order.by = station_df$datetime)
              colnames(ts) <- station
              return(ts)
            }
            return(NULL)
          })
          
          # Удаляем NULL элементы
          station_data <- station_data[!sapply(station_data, is.null)]
          
          if(length(station_data) == 0) return(NULL)
          
          # Объединяем все станции для текущего показателя
          combined_ts <- do.call(cbind, station_data)
          
          # Генерируем уникальные цвета для станций
          colors <- RColorBrewer::brewer.pal(max(3, length(station_data)), "Set1")[1:length(station_data)]
          
          incProgress(1/length(input$pick_var), 
                      detail = paste("Показатель:", current_var))
          
          # Строим график
          dygraph(combined_ts, main = current_var, width = '100%') %>%
            dyRangeSelector() %>%
            dyAxis("x", label = "Дата") %>%
            dyAxis("y", label = current_var) %>%
            dyOptions(
              colors = colors,
              drawPoints = TRUE,
              pointSize = 3,
              strokeWidth = 2,
              connectSeparatedPoints = TRUE
            ) %>%
            dyLegend(
              show = "always",
              hideOnMouseOut = FALSE,
              labelsSeparateLines = TRUE
            ) %>%
            dyHighlight(
              highlightCircleSize = 4,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE
            )
        })
        
        # Удаляем NULL элементы
        plot_list <- plot_list[!sapply(plot_list, is.null)]
        
        if(length(plot_list) == 0) {
          return(div("Нет данных для построения графиков", style = "color:red; padding:20px;"))
        }
        
        htmltools::tagList(plot_list)
      })
    })
    
    # Вывод таблицы
    output$datatable <- renderDataTable({
      df <- plot_df()
      if(is.null(df)) return(NULL)
      
      # Преобразуем в широкий формат
      wide_df <- df %>%
        pivot_wider(
          id_cols = datetime,
          names_from = c(name_station, value_type),
          names_sep = " - ",
          values_from = value
        ) %>%
        # Форматируем дату (без времени)
        mutate(datetime = format(as.Date(datetime), "%Y-%m-%d")) %>%
        arrange(desc(datetime)) %>%
        mutate(across(where(is.numeric), ~round(., 3)))
      
      datatable(wide_df,
                options = list(
                  pageLength = 100,
                  scrollX = TRUE,
                  language = list(
                    url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"
                  )
                ),
                rownames = FALSE) %>%
        formatStyle(columns = 1:ncol(wide_df), fontSize = '90%')
    })
  })
  
  # Файл для скачивания ----
  output$download <- downloadHandler(
    filename = function(){"ivankovo_output.csv"}, 
    content = function(fname){
      write.table(plot_df(), 
                  fname, sep = ";", quote = F, row.names = F, na = '-')
    }
  )
  
  # Разрыв соединения с БД ----
  session$onSessionEnded(function() {
    #   lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
    dbDisconnect(con)
    print('Disconnected')
  })
}

shinyApp(ui, server)