
library(shiny)
library(dplyr)
#login-------------
library(shinyauthr)

users <-read.csv("user_table.csv",sep=";")

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
    user = as.character(users$user),
    password = sapply(as.character(users$password),
                      sodium::password_store),
    permissions = as.character(users$permission),
    name = as.character(users$name)
)
#




#1---------------------------
outputDir <- "responses"
outputDir_Admin <- "settings"
    
    saveData <- function(data,user) {
        data <- t(data)
        # Create a unique file name
        #fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
        fileName <- sprintf("%s_%s.csv", data[[1]], "FlowKupa1")
        # Write the file to the local system
        write.csv(
            x = data,
            file = file.path(outputDir, fileName), 
            row.names = FALSE, quote = TRUE )
    }
    
    saveData_Admin <- function(data,user) {
        data <- t(data)
        # Create a unique file name
        #fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
        fileName <- sprintf("%s_%s.csv", "FlowKupa1", "settings")
        # Write the file to the local system
        write.csv(
            x = data,
            file = file.path(outputDir_Admin, fileName), 
            row.names = FALSE, quote = TRUE )
    }
    
    loadData <- function() {
        # Read all the files into a list
        files <- list.files(outputDir, full.names = TRUE)
        data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
        # Concatenate all data together into one data.frame
        data <- do.call(rbind, data)
        data
    }
    
    loadData_Admin <- function() {
        # Read all the files into a list
        files <- list.files(outputDir_Admin, full.names = TRUE,pattern = "^[FlowKupa1]")
        data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
        # Concatenate all data together into one data.frame
        data <- do.call(rbind, data)
        data
    }
    
    
    #2------------
    # Define the fields we want to save from the form
    fields <- c("user","name","sex","cat",
                "B1", "B2","B3","B4","B5",
                "B6", "B7","B8","B9","B10",
                "B11", "B12","B13","B14","B15",
                "B16", "B17","B18","B19","B20",
                "B21", "B22","B23","B24","B25",
                "B26", "B27","B28","B29","B30",
                "B31", "B32","B33","B34","B35",
                "B36", "B37","B38","B39","B40")
    #fields_admin <- c("verseny","boulderek")
    
    
    
    #app--------------
    ui <- fluidPage(
        titlePanel(title = span(img(src = "flowlogo.png", height = 35) ),"1. Flow seasons 2024"),
        # logout button
        div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
        
        # login section
        shinyauthr::loginUI(id = "login"),
        
        # Sidebar to show user info after login
        uiOutput("sidebarpanel"),
        
        # Info after login
        #fluidPage("Ha beléptél add meg az eredményeid."),
        
        tableOutput("user_table")
        
    )
    
    server <- function(input, output, session) {
        
        credentials <- shinyauthr::loginServer(
            id = "login",
            data = user_base,
            user_col = user,
            pwd_col = password,
            sodium_hashed = TRUE,
            log_out = reactive(logout_init())
        )
        
        # Logout to hide
        logout_init <- shinyauthr::logoutServer(
            id = "logout",
            active = reactive(credentials()$user_auth)
        )
        
        # UI
        output$sidebarpanel <- renderUI({
            # Show only when authenticated
            req(credentials()$user_auth)
            if(credentials()$info[[3]]=="admin"){
                fluidPage(
                    tags$hr(),
                    strong("Admin felület"),br(),
                    #textInput("verseny","Verseny neve","FlowKupa1"),
                    radioButtons("stop","Adatbevitel leállítása",choices = c("Live","Stop"),selected = "Live"),
                    fluidRow(
                        checkboxGroupInput("boulderek","Boulderek",choices = fields[5:44],selected = fields[5:44],
                                           inline = T)
                    ),
                    
                    actionButton("submit", "Submit",class = "btn btn-primary"),tags$hr(),
                    strong("Settings"),br(),
                    DT::dataTableOutput("settings", width = 300), tags$hr(),
                    strong("Eredmények (Felnőtt)"),br(),
                    "Results (adult)",
                    DT::dataTableOutput("results1", width = 300), tags$hr(),
                    strong("Eredmények (Gyermek)"),br(),
                    "Results (Children)",
                    DT::dataTableOutput("results2", width = 300), tags$hr(),
                    strong("Megmászások és Zónák"),br(),
                    "Tops & zones",
                    DT::dataTableOutput("responses", width = 300), tags$hr(),
                    strong("Aktuális Pontok boulderenként: Felnőtt (Top=1000/topok száma, Zóna=500/zónák száma)"),br(),
                    "Actual points for boulders: adult (Top=1000/number of tops, Zone=500/number of zones)",
                    DT::dataTableOutput("points1", width = 300), tags$hr(),
                    strong("Aktuális Pontok boulderenként: Gyermek (Top=1000/topok száma, Zóna=500/zónák száma)"),br(),
                    "Actual points for boulders: Children (Top=1000/number of tops, Zone=500/number of zones)",
                    DT::dataTableOutput("points2", width = 300), tags$hr()
                )
            }else{
                fluidPage(
                    tags$hr(),
                    strong("A név amit mindenki láthat az eredményeidnél (nem kötelező)"),br(),
                    "The name which is visible to everyone (optional)",
                    textInput("name", "Name", ""),
                    radioButtons("sex","Sex",choices = c("Man","Woman"),selected = "Man",inline = T),
                    radioButtons("cat","Category",
                                 choiceNames = c("Lelkes","Kattant","Szikkadt","Masters","Veterán", "U10", "U12", "U14", "U16"),
                                 choiceValues =c("lelkes","kattant","szikkadt","masters","veteran", "U10", "U12", "U14", "U16"),
                                 selected = "k",inline = T),
                    column(3,
                           (if("B1" %in% settings()){
                               radioButtons("B1", "Bolulder 01", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B2" %in% settings()){
                               radioButtons("B2", "Bolulder 02", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B3" %in% settings()){
                               radioButtons("B3", "Bolulder 03", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B4" %in% settings()){
                               radioButtons("B4", "Bolulder 04", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B5" %in% settings()){
                               radioButtons("B5", "Bolulder 05", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B6" %in% settings()){
                               radioButtons("B6", "Bolulder 06", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B7" %in% settings()){
                               radioButtons("B7", "Bolulder 07", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B8" %in% settings()){
                               radioButtons("B8", "Bolulder 08", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B9" %in% settings()){
                               radioButtons("B9", "Bolulder 09", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B10" %in% settings()){
                               radioButtons("B10", "Bolulder 10", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           })
                           ),
                    column(3,
                           (if("B11" %in% settings()){
                               radioButtons("B11", "Bolulder 11", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B12" %in% settings()){
                               radioButtons("B12", "Bolulder 12", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B13" %in% settings()){
                               radioButtons("B13", "Bolulder 13", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B14" %in% settings()){
                               radioButtons("B14", "Bolulder 14", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B15" %in% settings()){
                               radioButtons("B15", "Bolulder 15", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B16" %in% settings()){
                               radioButtons("B16", "Bolulder 16", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B17" %in% settings()){
                               radioButtons("B17", "Bolulder 17", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B18" %in% settings()){
                               radioButtons("B18", "Bolulder 18", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B19" %in% settings()){
                               radioButtons("B19", "Bolulder 19", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B20" %in% settings()){
                               radioButtons("B20", "Bolulder 20", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           })
                           ),
                    column(3,
                           (if("B21" %in% settings()){
                               radioButtons("B21", "Bolulder 21", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B22" %in% settings()){
                               radioButtons("B22", "Bolulder 22", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B23" %in% settings()){
                               radioButtons("B23", "Bolulder 23", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B24" %in% settings()){
                               radioButtons("B24", "Bolulder 24", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B25" %in% settings()){
                               radioButtons("B25", "Bolulder 25", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B26" %in% settings()){
                               radioButtons("B26", "Bolulder 26", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B27" %in% settings()){
                               radioButtons("B27", "Bolulder 27", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B28" %in% settings()){
                               radioButtons("B28", "Bolulder 28", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B29" %in% settings()){
                               radioButtons("B29", "Bolulder 29", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B30" %in% settings()){
                               radioButtons("B30", "Bolulder 30", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           })
                           ),
                    column(3,
                           (if("B31" %in% settings()){
                               radioButtons("B31", "Bolulder 31", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B32" %in% settings()){
                               radioButtons("B32", "Bolulder 32", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B33" %in% settings()){
                               radioButtons("B33", "Bolulder 33", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B34" %in% settings()){
                               radioButtons("B34", "Bolulder 34", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B35" %in% settings()){
                               radioButtons("B35", "Bolulder 35", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B36" %in% settings()){
                               radioButtons("B36", "Bolulder 36", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B37" %in% settings()){
                               radioButtons("B37", "Bolulder 37", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B38" %in% settings()){
                               radioButtons("B38", "Bolulder 38", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B39" %in% settings()){
                               radioButtons("B39", "Bolulder 39", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }),
                           (if("B40" %in% settings()){
                               radioButtons("B40", "Bolulder 40", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           })
                           ),
                    actionButton("submit", "Submit",class = "btn btn-primary"),
                    (if("Stop" %in% settings()){
                        strong("A verseny lezárult, a módosításokat már nem mentheted el.")
                    }),
                    tags$hr(),
                    strong("Összesen:"),br(),
                    "Summary",
                    DT::dataTableOutput("summary", width = 300), tags$hr(),
                    # strong("Eredmények (Felnőtt)"),br(),
                    # "Results (adult)",
                    # DT::dataTableOutput("results1", width = 300), tags$hr(),
                    # strong("Eredmények (Gyermek)"),br(),
                    # "Results (Children)",
                    # DT::dataTableOutput("results2", width = 300), tags$hr(),
                    # strong("Megmászások és Zónák"),br(),
                    # "Tops & zones",
                    # DT::dataTableOutput("responses", width = 300), tags$hr(),
                    strong("Aktuális Pontok boulderenként: Felnőtt (Top=1000/topok száma, Zóna=500/zónák száma)"),br(),
                    "Actual points for boulders: adult (Top=1000/number of tops, Zone=500/number of zones)",
                    DT::dataTableOutput("points1", width = 300), tags$hr(),
                    strong("Aktuális Pontok boulderenként: Gyermek (Top=1000/topok száma, Zóna=500/zónák száma)"),br(),
                    "Actual points for boulders: Children (Top=1000/number of tops, Zone=500/number of zones)",
                    DT::dataTableOutput("points2", width = 300), tags$hr()
                )
            }
            
        })
        
        formData <- reactive({
            if(credentials()$info[[3]]=="admin"){
                data <- c(input$stop,input$boulderek)
                
            }else{
                data <- sapply(fields, function(x) input[[x]])
                data$user<-credentials()$info[[1]]
            }
            data
        })
        
        # When the Submit button is clicked, save the form data
        settings<-reactive({
            input$submit
            loadData_Admin()
        })
        
        observeEvent(input$submit, {
            if(credentials()$info[[3]]=="admin"){
                saveData_Admin(formData())
            }else{
                if(settings()[[1]]=="Live"){
                    saveData(formData())
                }
                
            }
            
        })
        
        
        userdata<-reactive({
            input$submit
            user0<-credentials()$info[[1]]
            if(is.null(user0)){loadData()}else{
                loadData() %>% filter(user==user0)
            }
        })

        observe({
            updateRadioButtons(session,"stop","Adatbevitel leállítása",choices = c("Live","Stop"),selected = settings()[1])
            updateCheckboxGroupInput(session,"boulderek",choices = fields[5:44],selected = settings()[-1],inline = T)
            updateTextInput(session,"name","Name",userdata()$name)
            updateRadioButtons(session,"sex","Sex",choices = c("Man","Woman"),selected = userdata()$sex,inline = T)
            updateRadioButtons(session,"cat","Category",
                               choiceNames = c("Lelkes","Kattant","Szikkadt","Masters","Veterán", "U10", "U12", "U14", "U16"),
                               choiceValues =c("lelkes","kattant","szikkadt","masters","veteran", "U10", "U12", "U14", "U16"),
                               selected = userdata()$cat,inline = T)
            updateRadioButtons(session,"B1", "Bolulder 01", choices = c("None","Zone","Top"),selected = userdata()$B1,inline = T)
            updateRadioButtons(session,"B2", "Bolulder 02", choices = c("None","Zone","Top"),selected = userdata()$B2,inline = T)
            updateRadioButtons(session,"B3", "Bolulder 03", choices = c("None","Zone","Top"),selected = userdata()$B3,inline = T)
            updateRadioButtons(session,"B4", "Bolulder 04", choices = c("None","Zone","Top"),selected = userdata()$B4,inline = T)
            updateRadioButtons(session,"B5", "Bolulder 05", choices = c("None","Zone","Top"),selected = userdata()$B5,inline = T)
            updateRadioButtons(session,"B6", "Bolulder 06", choices = c("None","Zone","Top"),selected = userdata()$B6,inline = T)
            updateRadioButtons(session,"B7", "Bolulder 07", choices = c("None","Zone","Top"),selected = userdata()$B7,inline = T)
            updateRadioButtons(session,"B8", "Bolulder 08", choices = c("None","Zone","Top"),selected = userdata()$B8,inline = T)
            updateRadioButtons(session,"B9", "Bolulder 09", choices = c("None","Zone","Top"),selected = userdata()$B9,inline = T)
            updateRadioButtons(session,"B10", "Bolulder 10", choices = c("None","Zone","Top"),selected = userdata()$B10,inline = T)
            updateRadioButtons(session,"B11", "Bolulder 11", choices = c("None","Zone","Top"),selected = userdata()$B11,inline = T)
            updateRadioButtons(session,"B12", "Bolulder 12", choices = c("None","Zone","Top"),selected = userdata()$B12,inline = T)
            updateRadioButtons(session,"B13", "Bolulder 13", choices = c("None","Zone","Top"),selected = userdata()$B13,inline = T)
            updateRadioButtons(session,"B14", "Bolulder 14", choices = c("None","Zone","Top"),selected = userdata()$B14,inline = T)
            updateRadioButtons(session,"B15", "Bolulder 15", choices = c("None","Zone","Top"),selected = userdata()$B15,inline = T)
            updateRadioButtons(session,"B16", "Bolulder 16", choices = c("None","Zone","Top"),selected = userdata()$B16,inline = T)
            updateRadioButtons(session,"B17", "Bolulder 17", choices = c("None","Zone","Top"),selected = userdata()$B17,inline = T)
            updateRadioButtons(session,"B18", "Bolulder 18", choices = c("None","Zone","Top"),selected = userdata()$B18,inline = T)
            updateRadioButtons(session,"B19", "Bolulder 19", choices = c("None","Zone","Top"),selected = userdata()$B19,inline = T)
            updateRadioButtons(session,"B20", "Bolulder 20", choices = c("None","Zone","Top"),selected = userdata()$B20,inline = T)
            updateRadioButtons(session,"B21", "Bolulder 21", choices = c("None","Zone","Top"),selected = userdata()$B21,inline = T)
            updateRadioButtons(session,"B22", "Bolulder 22", choices = c("None","Zone","Top"),selected = userdata()$B22,inline = T)
            updateRadioButtons(session,"B23", "Bolulder 23", choices = c("None","Zone","Top"),selected = userdata()$B23,inline = T)
            updateRadioButtons(session,"B24", "Bolulder 24", choices = c("None","Zone","Top"),selected = userdata()$B24,inline = T)
            updateRadioButtons(session,"B25", "Bolulder 25", choices = c("None","Zone","Top"),selected = userdata()$B25,inline = T)
            updateRadioButtons(session,"B26", "Bolulder 26", choices = c("None","Zone","Top"),selected = userdata()$B26,inline = T)
            updateRadioButtons(session,"B27", "Bolulder 27", choices = c("None","Zone","Top"),selected = userdata()$B27,inline = T)
            updateRadioButtons(session,"B28", "Bolulder 28", choices = c("None","Zone","Top"),selected = userdata()$B28,inline = T)
            updateRadioButtons(session,"B29", "Bolulder 29", choices = c("None","Zone","Top"),selected = userdata()$B29,inline = T)
            updateRadioButtons(session,"B30", "Bolulder 30", choices = c("None","Zone","Top"),selected = userdata()$B30,inline = T)
            updateRadioButtons(session,"B31", "Bolulder 31", choices = c("None","Zone","Top"),selected = userdata()$B31,inline = T)
            updateRadioButtons(session,"B32", "Bolulder 32", choices = c("None","Zone","Top"),selected = userdata()$B32,inline = T)
            updateRadioButtons(session,"B33", "Bolulder 33", choices = c("None","Zone","Top"),selected = userdata()$B33,inline = T)
            updateRadioButtons(session,"B34", "Bolulder 34", choices = c("None","Zone","Top"),selected = userdata()$B34,inline = T)
            updateRadioButtons(session,"B35", "Bolulder 35", choices = c("None","Zone","Top"),selected = userdata()$B35,inline = T)
            updateRadioButtons(session,"B36", "Bolulder 36", choices = c("None","Zone","Top"),selected = userdata()$B36,inline = T)
            updateRadioButtons(session,"B37", "Bolulder 37", choices = c("None","Zone","Top"),selected = userdata()$B37,inline = T)
            updateRadioButtons(session,"B38", "Bolulder 38", choices = c("None","Zone","Top"),selected = userdata()$B38,inline = T)
            updateRadioButtons(session,"B39", "Bolulder 39", choices = c("None","Zone","Top"),selected = userdata()$B39,inline = T)
            updateRadioButtons(session,"B40", "Bolulder 40", choices = c("None","Zone","Top"),selected = userdata()$B40,inline = T)
            

        })
        
        # Show the previous responses
        # (update with current response when Submit is clicked)
        output$summary <- DT::renderDataTable({
            input$submit
            user0<-credentials()$info[[1]][1]
            table<- loadData() %>% filter(user==user0)
            if(dim(table)[1]==0){
                    table<- data.frame(summary=c("Top","Zone"),result=c(0,0))
                }else{
                    cn<- (t(settings()[1,-1]))
                    table<- 
                        loadData() %>% filter(user==user0)  %>% 
                        select(all_of(as.character(cn))) %>% t() %>% as.data.frame() %>%
                        group_by(V1) %>% summarise(n=n())
                    colnames(table) <- c("summary","result")
                } 
            table
        })
        
        output$settings <- DT::renderDataTable({
            input$submit
            loadData_Admin() 
        })
        
        output$responses <- DT::renderDataTable({
            input$submit
            cn<- (t(settings()[1,-1]))
            DT::datatable( loadData() %>% select(user,name,sex,cat,all_of(as.character(cn))),
                       extensions = "Buttons", 
                       options = list(paging = TRUE,
                                      #scrollX=TRUE, 
                                      searching = TRUE,
                                      ordering = TRUE,
                                      dom = 'Blfrtip',
                                      buttons = c('copy', 'csv', 'excel'),
                                      pageLength=10, 
                                      lengthMenu=c(10,50,100) )
            )
            
        })
        
        output$points1 <- DT::renderDataTable({
            input$submit
            cn<- (t(settings()[1,-1]))
            
            data2<-loadData() %>% mutate(cat2=cat) %>%
              filter(!as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2) %>% 
                select(all_of(as.character(cn))) 
            tops<-as.data.frame(ifelse(data2=="Top",1,0)) %>% colSums()
            zones<-as.data.frame(ifelse(data2=="Top",1,
                                        ifelse(data2=="Zone",1,0))) %>% colSums()
            tops<-1000/ifelse(tops==0,1,tops)
            zones<-500/ifelse(zones==0,1,zones)
            points<-rbind("Top"=tops,"Zone"=zones)
            DT::datatable(
                points,
                extensions = "Buttons", 
                options = list(paging = TRUE,
                               #scrollX=TRUE, 
                               searching = TRUE,
                               ordering = TRUE,
                               dom = 'Blfrtip',
                               buttons = c('copy', 'csv', 'excel'),
                               pageLength=5, 
                               lengthMenu=c(3,5,10) )
            )
            
        })
        output$points2 <- DT::renderDataTable({
          input$submit
          cn<- (t(settings()[1,-1]))
          
          data2<-loadData() %>% mutate(cat2=cat) %>%
            filter(as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2) %>%
            select(all_of(as.character(cn))) 
          tops<-as.data.frame(ifelse(data2=="Top",1,0)) %>% colSums()
          zones<-as.data.frame(ifelse(data2=="Top",1,
                                      ifelse(data2=="Zone",1,0))) %>% colSums()
          tops<-1000/ifelse(tops==0,1,tops)
          zones<-500/ifelse(zones==0,1,zones)
          points<-rbind("Top"=tops,"Zone"=zones)
          DT::datatable(
            points,
            extensions = "Buttons", 
            options = list(paging = TRUE,
                           #scrollX=TRUE, 
                           searching = TRUE,
                           ordering = TRUE,
                           dom = 'Blfrtip',
                           buttons = c('copy', 'csv', 'excel'),
                           pageLength=5, 
                           lengthMenu=c(3,5,10) )
          )
          
        })
        output$results1 <- DT::renderDataTable({
            input$submit
            cn<- (t(settings()[1,-1]))
            
            data3<- loadData() %>% 
                select(user,name,sex,cat,all_of(as.character(cn)))%>% mutate(cat2=cat) %>%
              filter(!as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2)
            data2<-loadData() %>% mutate(cat2=cat) %>%
              filter(!as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2) %>%
              select(all_of(as.character(cn)))
            tops<-as.data.frame(ifelse(data2=="Top",1,0)) %>% colSums()
            zones<-as.data.frame(ifelse(data2=="Top",1,
                                        ifelse(data2=="Zone",1,0))) %>% colSums()
            tops<-1000/ifelse(tops==0,1,tops)
            zones<-500/ifelse(zones==0,1,zones)
            points<-rbind("Top"=tops,"Zone"=zones)
            data4 <- as.data.frame(t(ifelse(t(data2)=="Top",tops,
                                            ifelse(t(data2)=="Zone",zones,0)))) 
            data4<- cbind((data3 %>% select(user,name,sex,cat)),
                          #SUM=(data4 %>% rowSums()),
                          (data4 %>% rowwise %>%
                                 mutate(!! "SUM_top10" := sum(head(sort(c_across(starts_with("B")), decreasing = TRUE), 10))) %>% 
                                 ungroup %>% select(SUM_top10)),
                          data4 ) %>% arrange(sex,cat,desc(SUM_top10))
            DT::datatable(data4,
                          extensions = "Buttons", 
                          options = list(paging = TRUE,
                                         #scrollX=TRUE, 
                                         searching = TRUE,
                                         ordering = TRUE,
                                         dom = 'Blfrtip',
                                         buttons = c('copy', 'csv', 'excel'),
                                         pageLength=10, 
                                         lengthMenu=c(10,50,100) ))  %>% 
                DT::formatStyle( 'SUM_top10', backgroundColor = 'yellow' )
        })
        
        output$results2 <- DT::renderDataTable({
          input$submit
          cn<- (t(settings()[1,-1]))
          
          data3<- loadData() %>% 
            select(user,name,sex,cat,all_of(as.character(cn)))%>%mutate(cat2=cat) %>%
            filter(as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2)
          data2<-loadData() %>% mutate(cat2=cat) %>%
            filter(as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2) %>%
            select(all_of(as.character(cn)))
          tops<-as.data.frame(ifelse(data2=="Top",1,0)) %>% colSums()
          zones<-as.data.frame(ifelse(data2=="Top",1,
                                      ifelse(data2=="Zone",1,0))) %>% colSums()
          tops<-1000/ifelse(tops==0,1,tops)
          zones<-500/ifelse(zones==0,1,zones)
          points<-rbind("Top"=tops,"Zone"=zones)
          data4 <- as.data.frame(t(ifelse(t(data2)=="Top",tops,
                                          ifelse(t(data2)=="Zone",zones,0)))) 
          data4<- cbind((data3 %>% select(user,name,sex,cat)),
                        #SUM=(data4 %>% rowSums()),
                        (data4 %>% rowwise %>%
                           mutate(!! "SUM_top10" := sum(head(sort(c_across(starts_with("B")), decreasing = TRUE), 10))) %>% 
                           ungroup %>% select(SUM_top10)),
                        data4 ) %>% arrange(sex,cat,desc(SUM_top10))
          DT::datatable(data4,
                        extensions = "Buttons", 
                        options = list(paging = TRUE,
                                       #scrollX=TRUE, 
                                       searching = TRUE,
                                       ordering = TRUE,
                                       dom = 'Blfrtip',
                                       buttons = c('copy', 'csv', 'excel'),
                                       pageLength=10, 
                                       lengthMenu=c(10,50,100) ))  %>% 
            DT::formatStyle( 'SUM_top10', backgroundColor = 'yellow' )
        })
        
        output$user_table <- renderTable({
            # use req to only render results when credentials()$user_auth is TRUE
            req(credentials()$user_auth)
            credentials()$info[1,1]
        })
    }
    

# Run the application 
shinyApp(ui = ui, server = server)



