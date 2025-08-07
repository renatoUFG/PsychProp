# PsychProp é um aplicativo Shiny para análise de propriedades psicométricas de 
# escalas tipo Likert
# Autor: Renato Rodrigues Silva
# Licenciado sob a GNU General Public License v3.0

# Este programa é software livre: você pode redistribuí-lo e/ou
# modificá-lo sob os termos da Licença Pública Geral GNU publicada
# pela Free Software Foundation, na versão 3 da Licença, ou
# (a seu critério) qualquer versão posterior.

# Este programa é distribuído na expectativa de que seja útil,
# mas SEM NENHUMA GARANTIA; nem mesmo a garantia implícita de
# COMERCIABILIDADE OU ADEQUAÇÃO A UM DETERMINADO PROPÓSITO.
# Veja a Licença Pública Geral GNU para mais detalhes.

# Você deve ter recebido uma cópia da Licença Pública Geral GNU
# junto com este programa. Caso contrário, veja <https://www.gnu.org/licenses/>.

library(shiny)
library(shinyWidgets)
library(shinythemes)

# UI ---------------------------------------------------------------------
ui <- navbarPage(
  title = "Análises Psicométricas",
  theme = shinytheme("flatly"),
  
  tabPanel("Importar Dados",
           sidebarLayout(
             sidebarPanel(
               fileInput("arquivo", "Escolha um arquivo CSV ou Excel", 
                         accept = c(".csv", ".xlsx")),
               checkboxInput("cabecalho", "Usar cabeçalho", TRUE),
               selectInput("sep", "Separador", 
                           choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), 
                           selected = ";")
             ),
             mainPanel(
               DT::dataTableOutput("tabela_dados")
             )
           )
  ),
  
  tabPanel("Estatística Descritiva",
           fluidPage(
             actionButton("rodar_resumo", "Rodar Resumo Estatístico"),
             tableOutput("resumo"),
             h1(''),
             actionButton("rodar_correlacao", "Rodar Matriz de Correlação"),
             plotOutput("correlacao"),
             h1(''),
             downloadButton("downloadCorr", "Exportar Matriz de Correlação ")
           )
  ),
  
  tabPanel("Análise Paralela",
           fluidPage(
             h4('Análise Paralela com pacote psych'),
             numericInput("n.iter", "Número de Iterações", 
                          value = 100, min = 50, max=500),
             actionButton("rodar_ap_psych", "Rodar Análise Paralela"),
             verbatimTextOutput("analise_paralela_psych"),
             h4('Análise Paralela com pacote EFA.MRFA'),
             numericInput("Ndatsets", "Número de Iterações", 
                          value = 100, min = 50, max=500),
             actionButton("rodar_ap_mrfa", "Rodar Análise Paralela"),
             verbatimTextOutput("analise_paralela_mrfa")
           )
  ),
  
  tabPanel("Análise Fatorial Exploratória (EFA)",
           fluidPage(
             numericInput("n_fatores", "Número de fatores", 
                          value = 2, min = 1),
             actionButton("rodar_efa", "Rodar EFA"),
             verbatimTextOutput("resultado_efa")
           )
  ),
  
  tabPanel("Análise Fatorial Confirmatória (CFA)",
           fluidPage(
             textAreaInput("modelo_cfa", "Modelo Lavaan", 
                           placeholder = "Ex: F1 =~ x1 + x2 + x3", rows = 15),
             actionButton("rodar_cfa", "Rodar CFA"),
             verbatimTextOutput("resultado_cfa")
           )
  ),
  
  tabPanel("Teoria de Resposta ao Item (TRI)",
           fluidPage(
             actionButton("rodar_tri", "Rodar TRI"),
             verbatimTextOutput("resultado_tri"),
             numericInput("n_itens", "Número de itens", 
                          value = 10, min = 1),
             actionButton("rodar_icc", "Curvas Características dos Itens"),
             plotOutput('curvas_icc'),
             h1(''),
             downloadButton("downloadICC", "Exportar Curva ICC"),
             h1(''),
             numericInput("n_itens", "Número de itens", 
                          value = 10, min = 1),
             actionButton("rodar_iic", "Curvas de Informação dos Itens"),
             plotOutput('curvas_iic'),
             h1(''),
             downloadButton("downloadIIC", "Exportar Curva IIC"),
             h1(''),
             actionButton("rodar_tic", "Curva de Informação do Teste"),
             plotOutput('curvas_tic'),
             h1(''),
             downloadButton("downloadTIC", "Exportar Curva TIC"),
           )
  ),
  
  tabPanel("Referências",
     fluidPage(
       h6('Chalmers, R. P. (2012). mirt: A multidimensional 
          item response theory package for the R environment. 
          Journal of Statistical Software, 48(6), 
          1-29. doi:10.18637/jss.v048.i06. '),
       h6('Navarro-Gonzalez D, Lorenzo-Seva U (2021). EFA.MRFA:
          Dimensionality Assessment Using Minimum Rank Factor Analysis. 
          R package version 1.1.2, https://CRAN.R-project.org/package=EFA.MRFA.'),
       h6('OpenAI. ChatGPT, Mar 27 version, OpenAI, 2025, https://chat.openai.com/.'),
       h6('R Core Team (2024). R: A Language and Environment for Statistical Computing. 
          R Foundation for Statistical Computing, Vienna, Austria. 
          URL: https://www.R-project.org/.'),
       h6('Schloerke B, Cook D, Larmarange J, Briatte F, Marbach M, Thoen E, 
           Elberg A, Crowley J (2025). GGally: Extension to ggplot2. 
           R package version 2.3.0, https://ggobi.github.io/ggally/.'),
       h6('Xie Y, Cheng J, Tan X (2025). DT: A Wrapper of the JavaScript 
           Library DataTables. R package version 0.33.3, 
           https://github.com/rstudio/dt.'),
       h6('Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, 
           Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, 
           Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, 
           Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). 
           “Welcome to the tidyverse.” Journal of Open Source Software, 
           4(43), 1686. doi:10.21105/joss.01686.'),
       h6('William Revelle (2025). psych: Procedures for Psychological, 
            Psychometric, and Personality Research. Northwestern University, 
            Evanston, Illinois. R package version 2.5.6, 
            https://CRAN.R-project.org/package=psych.'),
       h6('Yves Rosseel (2012). lavaan: An R Package for Structural Equation 
           Modeling. Journal of Statistical Software, 48(2), 1-36. URL 
           http://www.jstatsoft.org/v48/i02/'),
       em('Esse software foi escrito com auxílio de inteligência artificial 
          (OpenAI, 2025).')
        
             
     )
           
  )
  
  
  
)

# Server -----------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reativo: ler dados
  dados <- reactive({
    req(input$arquivo)
    ext <- tools::file_ext(input$arquivo$name)
    if (ext == "csv") {
      read.csv(input$arquivo$datapath, header = input$cabecalho, sep = input$sep)
    } else if (ext == "xlsx") {
      readxl::read_excel(input$arquivo$datapath)
    } else {
      validate("Formato não suportado")
    }
  })
  
  # Exibir dados
  output$tabela_dados <- DT::renderDataTable({
    req(dados())
    DT::datatable(dados())
  })
  
  
  
  # Estatística descritiva
  observeEvent(input$rodar_resumo,{
    output$resumo = renderTable({
      req(dados())
      dados_limpos = na.omit(dados())
      
      medias = apply(dados_limpos, 2, mean)
      desvios = apply(dados_limpos, 2, sd)
      skewness = apply(dados_limpos, 2, moments::skewness)
      kurtosis = apply(dados_limpos, 2, moments::kurtosis)
    
      resumo = rbind(
        "Média" = round(medias, 2),
        "Desvio Padrão" = round(desvios, 2),
        "Assimetria" = round(skewness, 2),
        "Curtose"   = round(kurtosis, 2)
      )
    
      return(as.data.frame(resumo))
    }, rownames = TRUE)
  })    
    
  observeEvent(input$rodar_correlacao,{
    output$correlacao <- renderPlot({
      req(dados())
      n = ncol(dados())
      polychoric_cor <- function(data, mapping, ...) {
      # Extrai as variáveis
      x <- GGally::eval_data_col(data, mapping$x)
      y <- GGally::eval_data_col(data, mapping$y)
      
      # Calcula as correlações policoricas
      corP <- tryCatch({
        psych::polychoric(cbind(x, y))$rho[1, 2]
      }, error = function(e) NA)
      
      # Cria o gráfico com correlações
      ggplot2::ggplot(data = data, mapping = mapping) +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = paste("r =", round(corP, 2)),
                          size = 4) +
        ggplot2::theme_void() 
    }
    
      GGally::ggpairs(dados(),
                    upper = list(continuous = GGally::wrap(polychoric_cor)), 
                    lower = list(continuous = "blankDiag"), 
                    diag = list(continuous = GGally::wrap("barDiag"))) 
    
    
    })
})

  
  output$downloadCorr <- downloadHandler(
    filename = function() {
      paste0("corr_plot_", Sys.Date(), ".png") # Or .pdf, .jpeg, etc.
    },
    content = function(file) {
      req(dados())
      n = ncol(dados())
      polychoric_cor <- function(data, mapping, ...) {
        # Extrai as variáveis
        x <- GGally::eval_data_col(data, mapping$x)
        y <- GGally::eval_data_col(data, mapping$y)
        
        # Calcula as correlações policoricas
        corP <- tryCatch({
          psych::polychoric(cbind(x, y))$rho[1, 2]
        }, error = function(e) NA)
        
        # Cria o gráfico com correlações
        ggplot2::ggplot(data = data, mapping = mapping) +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = paste("r =", round(corP, 2)),
                            size = 4) +
          ggplot2::theme_void() 
       }
      
        corr_plot = GGally::ggpairs(dados(),
                      upper = list(continuous = GGally::wrap(polychoric_cor)), 
                      lower = list(continuous = "blankDiag"), 
                      diag = list(continuous = GGally::wrap("barDiag"))) 
      
        # Save the plot to the specified file
        ggplot2::ggsave(file, plot = corr_plot, device = "png") # Or pdf(), jpeg(), etc.
    }
  )
  

  # Análise Paralela com psych
  
  observeEvent(input$rodar_ap_psych,{
    output$analise_paralela_psych <- renderPrint({
      req(dados())
      dados_limpos <- na.omit(dados())
      psych::fa.parallel(dados_limpos,
                         fm='minres',
                         fa='fa',
                         cor='poly',
                         n.iter = input$n.iter,
                         SMC=TRUE,
                         quant=0.95,
                         plot = FALSE)
    })
  })
  
  # Análise Paralela com EFA.MRFA
  
  observeEvent(input$rodar_ap_mrfa,{
    output$analise_paralela_mrfa <- renderPrint({
      req(dados())
      dados_limpos <- na.omit(dados())
      EFA.MRFA::parallelMRFA(dados_limpos, 
                   corr = "Polychoric",
                   Ndatsets = input$Ndatsets, 
                   percent = 95,
                   graph=FALSE)
      
    })
  })
  

  # EFA
  observeEvent(input$rodar_efa, {
    output$resultado_efa <- renderPrint({
      req(dados())
      dados_limpos = na.omit(dados()) 
      dados_ord = dplyr::mutate_all( dados_limpos,
        function(x){ factor(x, ordered=TRUE)}) 
      fit  = lavaan::efa(data = dados_ord,
                       estimator = "WLSMV",
                       nfactors = 1:input$n_fatores)
      summary(fit, fit.measures=TRUE,standardized= TRUE)
    })
  })
  
  # CFA
  observeEvent(input$rodar_cfa, {
    output$resultado_cfa <- renderPrint({
      req(dados())
      req(input$modelo_cfa)
      dados_limpos = na.omit(dados()) 
      dados_ord = dplyr::mutate_all( dados_limpos,
         function(x){ factor(x, ordered=TRUE)})
      model_cfa = input$modelo_cfa
      #covidBS  =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10
      fit <- lavaan::cfa(model = model_cfa, data = dados_ord)
      lavaan::summary(fit, fit.measures = TRUE, standardized = TRUE)
    })
  })
  
  # TRI
  
  ##Ajuste do modelo
  observeEvent(input$rodar_tri, {
    output$resultado_tri <- renderPrint({
      req(dados())
      dados_limpos = na.omit(dados())
      fit = mirt::mirt(dados_limpos, 1, 
           verbose = FALSE,
           itemtype = 'graded', 
           SE = TRUE)
      list_res = list('Qualidade do Ajuste do Modelo' = NA,
                      'Qualidade do Ajuste dos Itens' = NA,
                      'Estimativas dos Parâmetros' = NA)
      list_res$'Qualidade do Ajuste do Modelo' = mirt::M2(fit, type = "C2", calcNULL = FALSE)
      list_res$'Qualidade do Ajuste dos Itens' = mirt::itemfit(fit)
      list_res$'Estimativas dos Parâmetros' = mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)
      return(list_res)
    })
  })  
  
  ##Plotar ICC
  
  observeEvent(input$rodar_icc, {   
    req(dados())
    dados_limpos <- na.omit(dados())
    
    # Ajuste o modelo
    fit <- mirt::mirt(dados_limpos, 1, 
                      verbose = FALSE,
                      itemtype = 'graded', 
                      SE = TRUE)
    
    # Armazene o modelo para uso posterior
    output$curvas_icc <- renderPlot({
      mirt::plot(fit, 
                 type = 'trace', 
                 which.item = 1:input$n_itens,
                 facet_items = TRUE,
                 as.table = TRUE,
                 auto.key = list(points = FALSE, lines = TRUE, 
                                 columns = 4, space = 'top', cex = .8),
                 theta_lim = c(-3, 3),
                 main = "")
    })
    
    output$downloadICC <- downloadHandler(
      filename = function() {
        paste0("plot_ICC_", Sys.Date(), ".png")
      },
      content = function(file) {
        png(file, width = 800, height = 600)
        print(
          mirt::plot(fit, 
                     type = 'trace', 
                     which.item = 1:input$n_itens,
                     facet_items = TRUE,
                     as.table = TRUE,
                     auto.key = list(points = FALSE, lines = TRUE, 
                                     columns = 4, space = 'top', cex = .8),
                     theta_lim = c(-3, 3),
                     main = "")
        )
        dev.off()
      }
    )
  })

  
  ##Plotar IIC
  
  # Observe o botão e ajuste o modelo
  observeEvent(input$rodar_iic, {   
    req(dados())
    dados_limpos <- na.omit(dados())
    
    # Ajuste o modelo
    fit <- mirt::mirt(dados_limpos, 1, 
                      verbose = FALSE,
                      itemtype = 'graded', 
                      SE = TRUE)
    
    # Armazene o modelo para uso posterior
    output$curvas_iic <- renderPlot({
      mirt::plot(fit, 
                 type = 'infotrace', 
                 which.item = 1:input$n_itens,
                 facet_items = TRUE,
                 as.table = TRUE,
                 auto.key = list(points = FALSE, lines = TRUE, 
                                 columns = 4, space = 'top', cex = .8),
                 theta_lim = c(-3, 3),
                 main = "")
    
    })
    
    output$downloadIIC <- downloadHandler(
      filename = function() {
        paste0("plot_IIC_", Sys.Date(), ".png")
      },
      content = function(file) {
        png(file, width = 800, height = 600)
        print(
          mirt::plot(fit, 
                     type = 'infotrace', 
                     which.item = 1:input$n_itens,
                     facet_items = TRUE,
                     as.table = TRUE,
                     auto.key = list(points = FALSE, lines = TRUE, 
                                     columns = 4, space = 'top', cex = .8),
                     theta_lim = c(-3, 3),
                     main = "")
        )
        dev.off()
      }
    )
  })
  
  
  ##Plotar TIC
  
  # Observe o botão e ajuste o modelo
  observeEvent(input$rodar_tic, {   
    req(dados())
    dados_limpos <- na.omit(dados())
    
    # Ajuste o modelo
    fit <- mirt::mirt(dados_limpos, 1, 
                      verbose = FALSE,
                      itemtype = 'graded', 
                      SE = TRUE)
    
    # Armazene o modelo para uso posterior
    output$curvas_tic <- renderPlot({
      mirt::plot(fit, type = 'info', 
                 theta_lim = c(-3, 3), main = "")
    })
    
    output$downloadTIC <- downloadHandler(
      filename = function() {
        paste0("plot_TIC_", Sys.Date(), ".png")
      },
      content = function(file) {
        png(file, width = 800, height = 600)
        print(
          mirt::plot(fit, type = 'info', 
                     theta_lim = c(-3, 3), main = "")
        )
        dev.off()
      }
    )
  })
  
}

# Run App ---------------------------------------------------------------
shinyApp(ui = ui, server = server)
