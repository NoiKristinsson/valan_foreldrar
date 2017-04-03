library(shiny)
library(stringr)
library(openxlsx)
library(zoo)
temp <- read.xlsx("listi.xlsx")
template <- read.csv("data/templ.csv")
template <- template[NULL,]

shorten <- function(x){
        if (str_count(x, " ") == 1) {
                paste(word(x,1), collapse = " ")   
        } else {
                paste(word(x,1:2), collapse = " ")   
        }
}


ui = fluidPage(
        titlePanel("Lagfæring á skrám úr Völunni"),
        sidebarLayout(
                sidebarPanel(
                        fileInput('file1', 'Veljið .xlsx skrá'),
                        downloadButton('downloadEmail', 'Sækja lista fyrir outlook'),
                        downloadButton('downloadData', 'Sækja lista yfir foreldra'),
                        p(),
                        p("Athugasemdir eða hugmyndir má senda á:"),
                        p("noi.kristinsson hja reykjavik.is")
                ),
                mainPanel(
                        h3("Mikilvægt"),
                        h4("Opnið skjalið úr völunni í Excel og vistið sem .xlsx"),
                        h4("Annars mun þetta ekki virka."),
                        br(),
                        p(strong("Outlook"), "mun gefa CSV skrá sem hægt er að importa inn í outlook"),
                        p(strong("Listi yfir foreldra"), "er hugsað sem skjal fyrir foreldrafélagið.")
                        #dataTableOutput('contents')
                )
        )
)


server = function(input, output, session){
        dataf.fixed <- reactive({
                inFile <- input$file1
                if (is.null(inFile)) return(NULL)
                dataf <- read.xlsx(inFile$datapath)
                
                colnames(dataf) <- dataf[2,]
                dataf <- dataf[-nrow(dataf),]
                dataf <- dataf[-1:-2,]
                dataf$kennitalabarns <- as.character(0000000000) 
                dataf <- dataf[,c(1,11,2:10)]
                dataf$kennitalabarns <- ifelse(is.na(dataf$Aðstandandi), dataf$Kennitala, NA)
                bil <- " // "
                
                dataf$kennitalabarns <- na.locf(dataf$kennitalabarns)
                dataf$Barn <- na.locf(dataf$Barn)
                dataf$Deild <- na.locf(dataf$Deild)
                dataf <- dataf[!is.na(dataf$Aðstandandi),]
                dataf$Kennitala <- sprintf("%010s", as.character(dataf$Kennitala))
                dataf$kennitalabarns <- sprintf("%010s", as.character(dataf$kennitalabarns))
                
                dataf
                })
        
        outlook <- reactive({
                print(1)
                ##Stytti nöfnin
                dataf <- as.data.frame(dataf.fixed())
                dataf$Barn <- lapply(dataf$Barn, shorten)
                dataf$Aðstandandi <- lapply(dataf$Aðstandandi, shorten)
                
                print(2)
                email.datab <- data.frame(matrix("", nrow = nrow(dataf), ncol= ncol(template)))
                email.datab2 <- template
                
                email.datab <- rbind(email.datab, email.datab2) 
                colnames(email.datab) <- colnames(template)
                
                print(3)
                email.datab$First.Name <- paste0(dataf$Barn, bil, dataf$Aðstandandi)       
                email.datab$E.mail.Address <- paste0(dataf$Netfang) 
                
                email.datab
                })
               
        foreldrar.ready <- reactive({
                #### listi fyrir foreldrafélag með upplýsingum
                
                ## búa til gagnagrunninn og þær upplýsingar sem eiga að vera í honum.
                ## Nafn barns, kennitala þess, nafn foreldris og kennitala þess. Email foreldris.
                ##
                
                f.foreldraf <- as.data.frame(matrix("", nrow = nrow(dataf.fixed()), ncol=5))
                colnames(f.foreldraf) <- c("Nafn barns", "Kennitala barns", "Nafn aðstandanda", "Kennitala aðstandanda", "Vefpóstur")
                f.foreldraf$`Nafn barns` <- paste0(dataf.fixed()$Barn)
                f.foreldraf$`Kennitala barns` <- paste0(dataf.fixed()$kennitalabarns)
                f.foreldraf$`Nafn aðstandanda` <- paste0(dataf.fixed()$Aðstandandi)
                f.foreldraf$`Kennitala aðstandanda` <- paste0(dataf.fixed()$Kennitala)
                f.foreldraf$Vefpóstur <- paste0(dataf.fixed()$Netfang)
                f.foreldraf
             
                    })
        
        
        #output$contents <- renderDataTable({
        #        
        #        foreldrar.ready()
        #               })
        
        output$downloadEmail <- downloadHandler(
                filename = function() { paste0("fyrir_outlook", ".csv") },
                content = function(file) {
                        write.csv(outlook(), file)
                        }
        )
        output$downloadData <- downloadHandler(
                filename = function() { paste0("fyrir_foreldrafelag", ".xlsx") },
                content = function(file) {
                        write.xlsx(foreldrar.ready(), file)
                }
        )
}

shinyApp(ui = ui, server = server) # this launches your app