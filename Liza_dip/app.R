library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(patchwork)
library(plotly)

# Define UI for data upload app ----
ui <- navbarPage("Тайфун",
    tabPanel("Ввод данных",             
    
    # App title ----
    titlePanel("Загрузка файлов"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("files_to_tidy", "Файлы для очистки",
                      multiple = TRUE,
                      accept = c(".res", ".txt")),
            
            # Horizontal line ----
            tags$hr(), 
            
            fileInput("good_observations_file", "Файл для расчета скорости",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".txt")),
            
            # Horizontal line ----
            tags$hr(), 
            
            fileInput("garm_analisys_file", "Файл для гармонического анализа",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".txt"))

            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
             
            
            
            
        )
        
    )
), 

tabPanel("Анализ локатора", 
         
         
             
             fluidRow(
                 
                 column(1),
                 
                 column(6,
                 
                 sliderInput("good_obs_dates", "Выбор даты:", 
                             min = as.Date("01-01-2020", format = "%d-%m-%Y"), 
                             max = Sys.Date(), 
                             value = as.Date("01-01-2020", format = "%d-%m-%Y"), 
                             step = 1, 
                             timeFormat = "%d-%m-%Y"), 
                 
                 
                 ),
                 
                 column(3, 
                        
                        selectInput("good_obs_dist", "Выбор направления:", 
                              c(1:8))
                        
                 )
                 
             ), 
         
            tags$hr(),
         
             
             fluidRow(
                 column(1),
                 column(3, plotlyOutput("quantity_by_direct")), 
                column(3,plotlyOutput("hist_of_dist")), 
                column(5,plotlyOutput("quantity_in_hour")) 
                
                ),
         
                    fluidRow(
                        column(1),
                        column(10,plotlyOutput("quantity_by_direct_and_hours")), 
                        
                        ), 
                        
         ),





tabPanel("Расчет скорости", 
         
         fluidRow(
             
             column(1),
             
             column(6,
                    
                    sliderInput("good_obs_dates", "Выбор даты:", 
                                min = as.Date("01-01-2020", format = "%d-%m-%Y"), 
                                max = Sys.Date(), 
                                value = as.Date("01-01-2020", format = "%d-%m-%Y"), 
                                step = 1, 
                                timeFormat = "%d-%m-%Y"), 
                    
                    
             )),
         
         tags$hr(),
         
         fluidRow(
             column(1),
             column(5, plotlyOutput("U_plot")), 
             column(5,plotlyOutput("Y_plot"))
         )
         
),
         
         
tabPanel("Гармонический анализ U"), 
tabPanel("Гармонический анализ V")
)

# Define server logic to read selected file ----
server <- function(input, output) {


    mydata <- reactive({
        
        req(input$files_to_tidy)
        
        inFile <- input$files_to_tidy
        
        tbl <- read_table2(inFile$datapath)
        
        return(tbl)
    })

    output$quantity_in_hour <- renderPlotly({
        
        data <- mydata()
        
        data <- 
            data %>%
            mutate(mytext = paste("Час: ", hour, "\n", 
                                  "Количество: ", Quantity_in_hour))
        
        gg <- ggplot(data, aes(x = hour, y = Quantity_in_hour, text = mytext)) + 
            geom_bar(stat="identity",fill="steelblue") +
            ggtitle("Численность метеоров по часам")+
            #geom_text(aes(label=Quantity_in_hour), vjust=-0.3, size=3.5)+
            theme_classic()+
            xlab("Час")+ylab("Количество")+
            scale_x_continuous(breaks = seq(from = 0, to = 23,by = 1))
        
        ggplotly(gg, tooltip = "mytext")
    })


    good_observation <- reactive({
        
        req(input$good_observations_file)
        
        inFile <- input$good_observations_file
        
        tbl <- read_table2(inFile$datapath)
        
        return(tbl)
        
    })
    
    output$quantity_by_direct <- renderPlotly({
        
        data <- good_observation()
        
        data <- 
            data %>% 
            group_by(`"direct"`) %>% 
            summarise(col = n()) %>% 
            mutate(mytext = paste("Направление: ", `"direct"`, "\n", 
                                  "Количество: ", col))
        

       gg <- ggplot(data = data, aes(x = factor(`"direct"`), y = col, text = mytext)) + 
            geom_bar(stat="identity",fill="steelblue", width=0.7) +
            ggtitle("Численность метеоров по направлениям")+
            theme_classic()+
            xlab("Направление")+ylab("Количество")
       
       ggplotly(gg, tooltip = "mytext")
        
    })
    
    output$quantity_by_direct_and_hours <- renderPlotly({
        
        data <- good_observation()
        
        Quantity_in_derection_by_hour <- 
            data %>%
            mutate(hour = floor(as.numeric(`"time"`)/3600)) %>%
            group_by(hour, `"direct"`) %>%
            summarise(Quantity_in_derection = n()) %>% 
            mutate(mytext = paste("Направление: ", `"direct"`, "\n", 
                                "Час: ", hour, "\n", 
                                "Количество: ", Quantity_in_derection))
        

        gg <- ggplot(data = Quantity_in_derection_by_hour , aes(x = hour, y = Quantity_in_derection, 
                                                fill = as.factor(`"direct"`), text = mytext)) +
            geom_col(color = 1, size = 0.2, position = 'dodge2') +
            ggtitle("Численность метеоров по часам и направлениям")+
            theme_classic()+
            xlab("Час")+ylab("Количество")+
            theme(legend.title = element_text( size=10, face="bold"))+
            theme(legend.position ="top")+
            scale_fill_discrete(name = "Направления")+
            scale_x_continuous(breaks = c(0:23))
        
        ggplotly(gg, tooltip = "mytext") %>%
            layout(legend = list(orientation = "h", x = 0.6, y = 1.15, title=list(text='<b> Направления </b>')))
        
    })
    
    
    output$hist_of_dist <- renderPlotly({
        
        data <- good_observation()

        data <- 
            data %>%
            rename("Дальность" = `"dist"`)
        
        gg <- ggplot(data = data, aes(x = `Дальность`)) +
            geom_histogram(binwidth = 10, colour = "black", 
                           fill = "greenyellow") +
            ggtitle("Численность метеоров по дальности")+
            theme_classic()+
            xlab("Дальность")+ylab("Количество")+
            theme(legend.title = element_text( size=10, face="bold"))+
            theme(legend.position ="top")+
            scale_fill_discrete(name = "Направления")+
            scale_x_continuous(breaks = seq(from = floor(min(data$`Дальность`)), 
                                            to = ceiling(max(data$`Дальность`)), by = 20))+
            scale_y_continuous(breaks = seq(from = 0, to = 100,by = 5))
        
        ggplotly(gg)
        
    })
    
    output$U_plot <- renderPlotly({
        
        data <- mydata()
        
        data <- 
            data %>%
            mutate(row_num = 1:nrow(data)) %>%
            mutate(mytext = paste("Измерение: ", row_num, "\n", 
                                "Скорость: ", U))
        
        
        gg <- ggplot(data = data, aes(x = row_num, y = data$U, text = mytext, group = 1)) +
            geom_line() + geom_point() + 
            ggtitle("Зональная составляющая скорости")+
            theme_classic()+
            xlab("Час")+ylab("Скорость")
        
        ggplotly(gg, tooltip = "mytext")
        
    })
    
    output$Y_plot <- renderPlotly({
        
        data <- mydata()
        
        data <- 
            data %>%
            mutate(row_num = 1:nrow(data)) %>%
            mutate(mytext = paste("Измерение: ", row_num, "\n", 
                            "Скорость: ", Y))
        
        gg <- ggplot(data = data, aes(x = row_num, y = data$Y, text = mytext, group = 1)) +
            geom_line() + geom_point() + 
            ggtitle("Меридианальная составляющая скорости")+
            theme_classic()+
            xlab("Час")+ylab("Скорость")
        
        
        ggplotly(gg, tooltip = "mytext")
        
    })
    
    

    
}

# Create Shiny app ----
shinyApp(ui, server)