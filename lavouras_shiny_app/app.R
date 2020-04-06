#shinyApp()
#install.packages("rsconnect")
library(rsconnect)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)

#https://www.curso-r.com/material/shiny/
# setwd('C:/Users/Arlei/OneDrive/Arlei-Carliton-Dimitri/SHYNI')  

ui <- fluidPage(
    
    titlePanel("Análise das lavouras do CENSO Agropecuário 2017"),
    hr(),
    
    fluidRow(
        column(width = 2,
               br(),
               uiOutput(outputId = "input_ui1"),
               uiOutput(outputId = "input_ui2"),
               uiOutput(outputId = "input_ui3"),
               uiOutput(outputId = "input_ui4"),
               uiOutput(outputId = "input_ui5"),
               uiOutput(outputId = "input_ui6"),
               uiOutput(outputId = "input_ui7")
        ),
        
        column(width = 10,
            tabsetPanel(selected = "Tab 1",
                        tabPanel("Tab1",
                                 tableOutput(outputId = "table1")),
                        tabPanel("Graf. Densidade Nacional",
                                 plotOutput(outputId = "plot1")),                  
                        tabPanel("Graf. Densidade Estadual",
                                 plotOutput(outputId = "plot2")),
                        tabPanel("Graf. Boxplot Mesoregional",
                                 plotOutput(outputId = "plot3")),
                        tabPanel("Graf. Dispersao",
                                 plotOutput(outputId = "plot4")),
                        tabPanel("Graf. Boxplot por grupo de area",
                                 plotOutput(outputId = "plot5")
                        )
            )
        )
    )
    
)

server <- function(input, output) {
    
    
    # Data --------------------------------------------------------------------
    # load("C:/Users/Arlei/OneDrive/Arlei-Carliton-Dimitri/SHYNI/Lavouras_CENSO2017.RData")
    load("~/lavouras_app/data_raw/Lavouras_CENSO2017.RData")
    
    
    output$input_ui1 <- renderUI({
        var_input1 <- lavouras %>% select(Tipo_lav) %>% distinct() %>% pull()
        
        pickerInput(
            inputId = "input_server1", 
            label = "Tipo_lav", 
            choices = var_input1, 
            selected = var_input1[1], 
            multiple = T
        )
    })
    
    output$input_ui2 <- renderUI({
        var_input2 <- lavouras %>% filter(Tipo_lav %in%  input$input_server1) %>% arrange(desc(Areat)) %>% 
            select(Nomes_prod1) %>% distinct() %>% pull()
        
        pickerInput(
            inputId = "input_server2", 
            label = "Nome dos produtos", 
            choices = var_input2,   
            selected = var_input2[1],
            multiple = T
        )
    })
    
    output$input_ui3 <- renderUI({
        var_input3 <- lavouras %>% filter(Tipo_lav %in%  input$input_server1, Nomes_prod1 %in% input$input_server2) %>% arrange(desc(Areat)) %>% select(UF1) %>% distinct() %>% pull()
        
        pickerInput(
            inputId = "input_server3", 
            label = "UF1", 
            choices = var_input3,    # choices = cv_today_100[order(-cv_today_100$cases),]$country  
            selected = var_input3[1], 
            multiple = T,
            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!")
        )
    })
    
    output$input_ui4 <- renderUI({
        var_input4 <- lavouras %>% filter(Tipo_lav %in%  input$input_server1, Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3) %>% arrange(desc(Areat)) %>% select(Nome_Meso) %>% distinct() %>% pull()
        
        pickerInput(
            inputId = "input_server4", 
            label = "Nome_Meso", 
            choices = var_input4, 
            selected = var_input4[1], 
            multiple = T,
            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!")
        )
    })
    
    output$input_ui5 <- renderUI({
        var_input5 <- lavouras %>% filter(Tipo_lav %in%  input$input_server1, Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3, Nome_Meso %in% input$input_server4) %>% arrange(desc(Areat))%>% select(Nome_Micro) %>% distinct() %>% pull()
        
        pickerInput(
            inputId = "input_server5", 
            label = "Nome_Micro", 
            choices = var_input5, 
            selected = var_input5[1], 
            multiple = T,
            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!")
        )
    })
    
    
    output$input_ui6 <- renderUI({
        var_input6 <- lavouras %>% filter(Tipo_lav %in%  input$input_server1, Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3, Nome_Meso %in% input$input_server4, Nome_Micro %in% input$input_server5) %>% select(Gr_areatotal) %>% distinct() %>% pull()
        #arrange(desc(Area))  ##coloca em ordem decrescente
        
        pickerInput(
            inputId = "input_server6", 
            label = "Gr_areatotal", 
            choices = var_input6, 
            selected = var_input6[1], 
            multiple = T,
            options = list(`actions-box` = TRUE, `none-selected-text` = "Selecionar Tudo")
            
        )
    })
    
    output$input_ui7 <- renderUI({
        var_input7_max <- lavouras %>% filter(Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3, Rendm > 0 ) %>% select(Areat) %>% max()
        var_input7_min <- lavouras %>% filter(Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3, Rendm > 0 ) %>% select(Areat) %>% min()
        sliderInput(
            inputId = "input_server7", 
            label = "slider minimax xlim", 
            min = var_input7_min, 
            max = var_input7_max, 
            value = c(var_input7_min, var_input7_max), 
            step = 100
        )
    })
    
    
    
    output$table1 <- renderTable(
        lavouras %>%  filter(Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3, Nome_Meso %in% input$input_server4, Nome_Micro %in% input$input_server5, Gr_areatotal %in% input$input_server6) %>%
            select(Nomes_prod1, UF1, Nome_Meso, Nome_Micro, Nome_mun, Gr_areatotal, Nr_estab, Areat, Volume, Rendm) %>%
            arrange(desc(Areat))
    )
    
    output$plot1 <- renderPlot({
        lavouras %>% filter(Nomes_prod1 %in% input$input_server2, Rendm > 0 ) %>% select(Rendm) %>%    
            ggplot() +  geom_density (aes(Rendm)) + 
            ggtitle('Rendimento Nacional da lavoura selecionada') 
        
    })
    
    output$plot2 <- renderPlot({
        var1 <- lavouras %>% filter(Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3, Rendm > 0 ) %>% select(UF1, Rendm)    
        var2 <- lavouras %>% filter(Nomes_prod1 %in% input$input_server2, Rendm > 0 ) %>% select(Rendm)    
        ggplot() +  geom_density (aes(var1$Rendm)) + geom_density (aes(var2$Rendm)) +
            ggtitle('Rendimento do da lavouras em cada estado') #+
            # facet_wrap(~input$input_server3)
    })
    
    output$plot3 <- renderPlot({
        lavouras %>% filter(Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3, Rendm > 0 ) %>% select(Nome_Meso, Rendm) %>%    
            ggplot() + geom_boxplot(aes(x = input$input_server3, y = Rendm)) +
            ggtitle('Rendimento do PRODUTO nas Meso regioes selecionada')  +
            facet_wrap(~ Nome_Meso)
        
    })
    output$plot4 <- renderPlot({
        lavouras %>% filter(Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3, Rendm > 0 ) %>% select(Areat, Rendm) %>%    
            ggplot() +  geom_point(aes(x = Areat, y = Rendm), color = "blue") +
            xlim(input$input_server7[1], input$input_server7[2])+
            ggtitle('Relação entre Area colhida e Rendimento da lavoura nos Estados selecionados')+
            theme_minimal()
    })
    output$plot5 <- renderPlot({
        lavouras %>% filter(Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3, Rendm > 0 ) %>% group_by(Gr_areatotal) %>% select(Rendm) %>%    
            ggplot() +  geom_boxplot(aes(x = Gr_areatotal, y = Rendm, fill = Gr_areatotal)) +
            ggtitle('Comportamento da produtivida por tamanho da propriedade  nos Estados selecionados') +
            scale_x_discrete(limits = c("ha0_10","ha10_20", "ha20_50", "ha50_100", "ha200_500", "ha500_1000", "ha1000_mais", "Total"))
    })    
    
    
}

shinyApp(ui = ui, server = server)