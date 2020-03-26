
library(shiny)
library(shinyWidgets)
library(ggplot2)


ui <- fluidPage(

    titlePanel("Análise das lavouras"),
    hr(),
    
    column(
        width = 3,
        fluidPage(
            uiOutput(outputId = "input_ui1"),
            uiOutput(outputId = "input_ui2"),
            uiOutput(outputId = "input_ui3"),
            uiOutput(outputId = "input_ui4"),
            uiOutput(outputId = "input_ui5"),
            uiOutput(outputId = "input_ui6")
        )
    ),
    
    column(
        width = 9,
        fluidPage(
            tabsetPanel(selected = "tab2",
                        tabPanel("tab1",
                                 plotOutput(outputId = "plot1")
                        ),
                        tabPanel("tab2",
                                 tableOutput(outputId = "table1"))
            )
        )
    )
    
)

server <- function(input, output) {
    

# Data --------------------------------------------------------------------
    load("~/lavouras_app/data_raw/Lavouras_CENSO2017.RData", )
    
    lavouras <- lavouras %>% mutate(uf = as.character(UF1))

    output$input_ui1 <- renderUI({
        var_input1 <- lavouras %>% select(Nomes_prod) %>% distinct() %>% pull()
        
        pickerInput(
            inputId = "input_server1", 
            label = "Nomes_prod", 
            choices = var_input1, 
            selected = var_input1[1], 
            multiple = T
        )
    })
    
    output$input_ui2 <- renderUI({
        var_input2 <- lavouras %>% filter(Nomes_prod %in% input$input_server1) %>% select(uf) %>% distinct() %>% pull()
        
        pickerInput(
            inputId = "input_server2", 
            label = "uf", 
            choices = var_input2, 
            selected = var_input2[1], 
            multiple = T,
            options = shinyWidgets::pickerOptions(
                actionsBox = T,
                deselectAllText = "Nenhum", 
                selectAllText = "Todos"
            )
        )
    })
    
    output$input_ui3 <- renderUI({
        var_input3 <- lavouras %>% filter(Nomes_prod %in% input$input_server1, uf %in% input$input_server2) %>% select(Meso) %>% distinct() %>% pull()
        
        pickerInput(
            inputId = "input_server3", 
            label = "Meso", 
            choices = var_input3, 
            selected = var_input3[1], 
            multiple = T
        )
    })
    
    output$input_ui4 <- renderUI({
        var_input4 <- lavouras %>% filter(Nomes_prod %in% input$input_server1, uf %in% input$input_server2, Meso %in% input$input_server3) %>% select(Micro) %>% distinct() %>% pull()
        
        pickerInput(
            inputId = "input_server4", 
            label = "Micro", 
            choices = var_input4, 
            selected = var_input4[1], 
            multiple = T
        )
    })
    
    output$input_ui5 <- renderUI({
        var_input5 <- lavouras %>% filter(Nomes_prod %in% input$input_server1, uf %in% input$input_server2, Meso %in% input$input_server3, Micro %in% input$input_server4) %>% select(Tipo_lav) %>% distinct() %>% pull()
        
        pickerInput(
            inputId = "input_server5", 
            label = "Tipo_lav", 
            choices = var_input5, 
            selected = var_input5[1], 
            multiple = T
        )
    })
    
    output$input_ui6 <- renderUI({
        var_input6 <- lavouras %>% filter(Nomes_prod %in% input$input_server1, uf %in% input$input_server2, Meso %in% input$input_server3, Micro %in% input$input_server4, Tipo_lav %in% input$input_server5) %>% select(Gr_areatotal) %>% distinct() %>% pull()
        
        pickerInput(
            inputId = "input_server6", 
            label = "Gr_areatotal", 
            choices = var_input6, 
            selected = var_input6[1], 
            multiple = T
        )
    })
    
    
    output$plot1 <- renderPlot({
        a <- lavouras %>% filter(Nomes_prod %in% input$input_server1, uf %in% input$input_server2, Meso %in% input$input_server3, Micro %in% input$input_server4, Tipo_lav %in% input$input_server5, Gr_areatotal %in% input$input_server6) %>% select(Areat) %>% pull()
        
        hist(a)
        # ggplot(lavouras, aes(x=lavouras$Volume)) + 
        #     geom_histogram(aes(y=..density..),
        #                    binwidth=.5,
        #                    colour="black", fill="white") +
        #     geom_density(alpha=.2, fill="#FF6666")
    })
    
    output$table1 <- renderTable(
        lavouras %>% filter(Nomes_prod %in% input$input_server1, uf %in% input$input_server2, Meso %in% input$input_server3, Micro %in% input$input_server4, Tipo_lav %in% input$input_server5, Gr_areatotal %in% input$input_server6)
    )
}

shinyApp(ui = ui, server = server)
