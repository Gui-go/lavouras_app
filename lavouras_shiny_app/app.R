library(rsconnect)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(shinydashboard)

#https://www.curso-r.com/material/shiny/
# setwd('C:/Users/Arlei/OneDrive/Arlei-Carliton-Dimitri/SHYNI')  

ui <- dashboardPage(
    title = "Aplicação das lavouras", 
    skin = "blue",
    header = dashboardHeader(
        title = "Cabeçalho", 
        titleWidth = 300, 
        disable = F
    ), 
    sidebar = dashboardSidebar(
        disable = F, 
        width = 300, 
        collapsed = F,
        
        sidebarMenu(
            menuItem(
                text = "Início", 
                icon = icon("asterisk"), 
                badgeLabel = "novo", 
                badgeColor = "green", 
                tabName = "menu1", 
                selected = T
            ),
            menuItem(
                text = "Aba2", 
                icon = icon("gear"), 
                badgeLabel = "dica", 
                badgeColor = "red", 
                tabName = "menu2", 
                selected = F
            ),
            menuItem(
                text = "Disperão em violino", 
                tabName = "menuVS"
            ),
            menuItem(
                text = "Sobre", 
                tabName = "menu3", 
                selected = F,
                
                menuSubItem(
                    text = "Sub Item um", 
                    tabName = "submenu1"
                ),
                menuSubItem(
                    text = "Sub Item dois", 
                    tabName = "submenu2"
                )
            )
        )
    ), 
    body =  dashboardBody(
        tabItems(
            tabItem(
                tabName = "menu1",
                fluidPage(
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
                                                    plotOutput(outputId = "plot1",height="700px")),                  
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
            ),
            tabItem(
                tabName = "menuVS",
                plotOutput(outputId = "plotVS", width = "1500px", height = "740px")
            ),
            tabItem(
                tabName = "menu2",
                fluidPage(
                    h3("Abas são muito úteis pra que possamos dividir vizualizações em abas diferentes. Quando cada uma das vizualizações possui muitos detalhes, variáveis e inputs diferentes uns dos outros, acaba ficando um pouco confuso. Possível, mas confuso. Então, ao concentrar vizualizações semelhantes na mesma aba, tudo se torna mais simples e intuitivo."),
                    br(),
                    h3("Pra aproveitar melhor o espaço da tela, basta utilizar o argumento height dentro da função plotOutput, ou seja qual for o output desejável. No caso do plot2, por exemplo, utilizei height='800px'. Infelizmente, não é possível definir como 100%, ou alguma medida relativa. Sendo assim, é possível que a Dashboard tenha aparencia diferente em tamanhos de tela distintos..."),
                    br(),
                    h3("Além disso, me parece mais agradável aos olhos utilizar theme_minimal ou theme_classic nos gráficos. Mas isso, claro, é apenas questão de gosto.")
                )
            ),
            tabItem(
                tabName = "submenu1",
                h1("Subitem1...")
            ),
            tabItem(
                tabName = "submenu2",
                h3("submenu2")
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
            ggtitle('Rendimento Nacional da lavoura selecionada') +
            theme_dark()
    })
    
    output$plot2 <- renderPlot({
        var1 <- lavouras %>% filter(Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3, Rendm > 0 ) %>% select(UF1, Rendm)    
        var2 <- lavouras %>% filter(Nomes_prod1 %in% input$input_server2, Rendm > 0 ) %>% select(Rendm)    
        ggplot() +  geom_density (aes(var1$Rendm)) + geom_density (aes(var2$Rendm)) +
            ggtitle('Rendimento do da lavouras em cada estado') +
            theme_classic()
    })
    
    output$plot3 <- renderPlot({
        lavouras %>% filter(Nomes_prod1 %in% input$input_server2, UF1 %in% input$input_server3, Rendm > 0 ) %>% select(Nome_Meso, Rendm) %>%    
            ggplot() + geom_boxplot(aes(x = input$input_server3, y = Rendm)) +
            ggtitle('Rendimento do PRODUTO nas Meso regioes selecionada')  +
            facet_wrap(~ Nome_Meso)+
            theme_void()
        
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
    output$plotVS <- renderPlot({
        ggplot(mtcars, aes(mpg, factor(cyl))) +
            geom_violin()+
            theme_classic()
    })
    
    
}

shinyApp(ui = ui, server = server)