library(shiny)
library(stringr)
library(openxlsx)
library(zoo)

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
                        fileInput('file1', 'Veljið .xls skrá'),
                        downloadButton('downloadEmail', 'Sækja lista fyrir outlook'),
                        downloadButton('downloadData', 'Sækja lista yfir foreldra'),
                        p(),
                        p("Athugasemdir eða hugmyndir má senda á:"),
                        p("noi.kristinsson hja reykjavik.is")
                ),
                mainPanel(
                        h3("Ath"),
                        h4("Þetta notast við skránna: Skýrslur -> listar -> Aðstandendur barna"),
                        h4("Þegar þú sækir skrá opnast nýr gluggi, tómur. Það má loka honum."),
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
                dataf <- as.data.frame(readHTMLTable(inFile$datapath, encoding = "UTF-8"))
                
                dataf[dataf == ""] <- NA
               
                dataf <- dataf[,c(6,8:10,15)]
                dataf$kennitalabarns <- as.character(0000000000)
                dataf <- dataf[,c(1,6,2:5)]
                colnames(dataf) <- c("Barn", "kennitalabarns", "Deild", "Kennitala", "Aðstandandi", "Netfang")
                dataf[,1] <- as.character(dataf[,1])
                dataf[,3] <- as.character(dataf[,3])
                dataf[,5] <- as.character(dataf[,5])
                dataf[,6] <- as.character(dataf[,6])
                dataf[,4] <- as.character(dataf[,4])
                
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
                email.datab$E.mail.Address <- dataf$Netfang
                email.datab$Department <- dataf$Deild
                
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