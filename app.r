library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(shinyjs)


source("plots.R")

years = getYears()

semesterCat = function(x)
{
  switch(x,
         "Winter" = 0,
         "Spring" = 1,
         "Summer" = 2,
         "Fall" = 3)
}

ui <-fluidPage(includeCSS("www\\theme.CSS"),
               dashboardPage(skin="yellow",
                             dashboardHeader(title="ELAC",titleWidth="300px"),
                             dashboardSidebar(width="300px",collapsed=TRUE,
                                              
                                              sidebarMenu(id="tabs",
                                                          menuItem("Tutorial",tabName="tutorial",icon=icon("info-circle")),
                                                          menuItem("Scheduling App",tabName="main_app",selected=T)
                                              )
                             ),
                             
                             dashboardBody(shinyjs::useShinyjs(),
                                           tabItems(tabItem(tabName = "main_app",
                                                            
                                                            
                                                            #---------------------------------------Body--------------------------------------------------------------------                     
                                                            fluidRow(
                                                              # div( id="title1" , h3(" Time Frame")),
                                                              box(title=NULL, width=12,div(id="time_frame",box(solidHeader = T, width=12,title="Time Frame",status="warning",
                                                                                                               column(3,selectInput(inputId = "year",label="Academic Year",
                                                                                                                                    choices=years)),
                                                                                                               column(2,radioButtons(inputId = "semester","Semester",
                                                                                                                                     choices=c(" "),inline=TRUE))
                                                              )),
                                                              column(3,box(solidHeader = T,status="warning",width=12,title="Program",selectInput(inputId = "programs",label=NULL,c(" ")))),
                                                              column(3, box(solidHeader = T,status="warning",width=12,title= "Department",selectInput(inputId = "departments",label=NULL,choices=c(" ")))),
                                                              column(3,box(solidHeader = T,status="warning",width=12,title="Discipline",selectInput(inputId = "disciplines", label=NULL,choices=c(" ")))),
                                                              column(3,div(id="course_box",box(solidHeader = T,status="warning",width=12,title="Courses",selectInput(inputId = "courses",label = NULL,choices=c(" "))))),
                                                              column(width=1,offset=11,actionButton(inputId = "submit",label="Enter")),
                                                              column(12,p(id = "element", HTML("&#9658; Advanced"))),
                                                              column(12, div(id="advanced_box",box(status="info",width=12,
                                                                                                   title=NULL,
                                                                                                   collapsed=F,
                                                                                                   column(12,selectInput(inputId="filter_option", label="Filter By:", choices=c("Meeting Day","Start Period"),
                                                                                                                         width="40%")),
                                                                                                   column(4,selectInput(inputId = "week_filter",label="Weeks Available",choices=c("Select Weeks"), width="100%")),
                                                                                                   
                                                                                                   column(12,selectInput(inputId = "meeting_day", 
                                                                                                                         label = "Meeting Day", choices = c("Select Day"), width="40%")),
                                                                                                   column(12,selectInput(inputId = "start_period", 
                                                                                                                         label = "Start Period", choices = c("Select Time"),width="40%")
                                                                                                   ),
                                                                                                   column(width=1,actionButton(inputId = "submit_advanced",label="Enter"))
                                                                                                   
                                                              )))
                                                              ),br(),br(),
                                                              
                                                              column(12,div(id="box_graph",box(width="100%",
                                                                                               column(12,downloadButton("download_excel", "Download CSV")),br(),br(),
                                                                                               shinydashboard::valueBoxOutput(outputId ="box_time",3 ),
                                                                                               shinydashboard::valueBoxOutput(outputId ="box_program",4 ),
                                                                                               shinydashboard::valueBoxOutput(outputId = "box_class",5),
                                                                                               tabsetPanel(id="location",
                                                                                                           #------------------------------ ELAC -------------------------------------------------------
                                                                                                           tabPanel(title="Main Campus",value="main_campus",br(),
                                                                                                                    column(width=1,div(class="time_down",dropdownButton(size="sm",right=F,width=300,
                                                                                                                                                                        tooltip = tooltipOptions(title = "Time Legend"),
                                                                                                                                                                        icon=icon("info"),label="Time Description",
                                                                                                                                                                        tags$div(HTML('<h5>*Relative to start time of class</h5>
                                                                                                                                                                                      <h5>Early Morning (Before 10:00AM) </br>
                                                                                                                                                                                      </br>Morning (10:00AM-12:00PM)</br>
                                                                                                                                                                                      </br> College Hour (12:00PM-1:30PM)</br>
                                                                                                                                                                                      </br> Early Afternoon (1:30PM-3:30PM)</br>
                                                                                                                                                                                      </br>Afternoon (3:30PM-6:00PM)</br>
                                                                                                                                                                                      </br>Evening (After 6:00PM) </h5>'))))
                                                                                                                           
                                                                                                                                                                        ),
                                                                                                                    column(11,div(class="rank_text",uiOutput("title_main"))),br(),
                                                                                                                    column(width=2,offset=10,materialSwitch(inputId = "limit", 
                                                                                                                                                            label = "Enrollment Limit", status = "primary", 
                                                                                                                                                            right = TRUE)),
                                                                                                                    
                                                                                                                    uiOutput("box1"),
                                                                                                                    uiOutput("box2"),
                                                                                                                    uiOutput("box3"),
                                                                                                                    uiOutput("box4"),
                                                                                                                    uiOutput("box5"),
                                                                                                                    column(12,div(class="rank",h3("Average Enrollment Ranking"))),
                                                                                                                    column(12,selectInput(inputId = "weeks_chosen",choices=NULL,label=NULL, width="25%")),
                                                                                                                    column(12,tableOutput("table"))),
                                                                                                           
                                                                                                           #----------------------SOUTH GATE -------------------------------------------------------
                                                                                                           tabPanel(title="SGEC",value="SGEC",
                                                                                                                    column(width=1,div(class="time_down",dropdownButton(size="sm",right=T,width=300,
                                                                                                                                                                        tooltip = tooltipOptions(title = "Time Legend"),
                                                                                                                                                                        icon=icon("info"),label="Time Description",
                                                                                                                                                                        tags$div(HTML('<h5>*Relative to start time of class</h5>
                                                                                                                                                                                      <h5>Early Morning (Before 10:00AM) </br>
                                                                                                                                                                                      </br>Morning (10:00AM-12:00PM)</br>
                                                                                                                                                                                      </br> College Hour (12:00PM-1:30PM)</br>
                                                                                                                                                                                      </br> Early Afternoon (1:30PM-3:30PM)</br>
                                                                                                                                                                                      </br>Afternoon (3:30PM-6:00PM)</br>
                                                                                                                                                                                      </br>Evening (After 6:00PM) </h5>'))))
                                                                                                                           
                                                                                                                                                                        ),
                                                                                                                    column(11,div(class="rank_text",uiOutput("title_sgec"))),
                                                                                                                    column(width=2,offset=10,materialSwitch(inputId = "limit_s", 
                                                                                                                                                            label = "Enrollment Limit", status = "primary", 
                                                                                                                                                            right = TRUE)),
                                                                                                                    uiOutput("box1_s"),
                                                                                                                    uiOutput("box2_s"),
                                                                                                                    uiOutput("box3_s"),
                                                                                                                    uiOutput("box4_s"),
                                                                                                                    uiOutput("box5_S"),
                                                                                                                    column(12,div(class="rank",h3("Average Enrollment Ranking"))),
                                                                                                                    column(12,selectizeInput(inputId = "weeks_chosen_s",choices=NULL,label=NULL, width="25%")),
                                                                                                                    column(12,tableOutput("table_s"))
                                                                                                                                                                        ))))
                                                                     
                                                                                                                    ))#fluidRow     
                                                                                                                    ),#tabitem-main_app
                                                    tabItem(tabName ="tutorial",
                                                            includeHTML("tutorial.txt"))
                                                                                                                    )#tabitems
                                                                                                           )#dashboard body
                                                            ))#dashboardPage,fluidPage

server <- shinyServer(function(input, output, session) {
  advanced_toggle <- reactiveVal(FALSE)
  data=reactiveVal(value=NULL)
  data_excel=reactiveVal(value=NULL)
  discipline_data=reactiveVal(value=NULL)
  
  options(warn = -1)
  
  observeEvent(input$filter_option,{
    if(input$filter_option =="Meeting Day"){
      shinyjs::hide("start_period")
      shinyjs::show("meeting_day")
    }else{
      shinyjs::hide("meeting_day")
      shinyjs::show("start_period")
    }
  })
  
  
  #will cause toggle to turn on oberve bellw will display hidden box
  onclick("element", {
    updateTextInput(session,inputId = "element")
    advanced_toggle(!advanced_toggle())
    
  })
  
  #display or show hidden box, if toggled will also hide enter button and course input
  observe(if(advanced_toggle()==F){#dont want to see advanced
    #weird number below is a character code that is an arrow
    html("element","&#9658; Advanced")
    shinyjs::hide("advanced_box")
    shinyjs::show("course_box")
    shinyjs::show("submit")
  }
  else{
    html("element","&#9660; Advanced")
    shinyjs::show("advanced_box")
    shinyjs::hide("course_box")
    shinyjs::hide("submit")
    discipline_data(getCourseEnrollmentData(input$year, semesterCat(input$semester),input$programs, 
                                            input$departments, input$disciplines, course = "None"))
    if(!is.null(discipline_data())){
      temp<<-unique(discipline_data()$Meeting_Days)
      
      
      updateSelectInput(session,inputId = "week_filter", choices=c("Select Weeks",weeks_session(isolate(input$semester),data=discipline_data())),label = "Weeks Available")
    }
    
  })
  
  #when weeks is choosen then dayys and times will also be updated
  observe(if(!is.null(input$week_filter) && input$week_filter!="Select Weeks"){
    
    
    days_available= get_filter(data=discipline_data(), semester = input$semester, week= input$week_filter, selection= "Meeting Days")
    start_period_available=get_filter(data=discipline_data(), semester = input$semester, week= input$week_filter, selection= "Start Period")
    
    updateSelectInput(session, inputId = "meeting_day", label = "Meeting Day", choices = c("Select Day",as.character(days_available)))
    updateSelectInput(session,inputId = "start_period",label = "Start Period", choices = c("Select Time",as.character(start_period_available)))
  }else{
    updateSelectInput(session, inputId = "meeting_day", label = "Meeting Day", choices = c("Select Day"))
    updateSelectInput(session,inputId = "start_period",label = "Start Period", choices = c("Select Time"))
  })
  
  chosen_department=reactiveVal(NULL)
  chosen_discipline=reactiveVal(NULL)
  chosen_course=reactiveVal(NULL)
  
  observeEvent(input$departments,{
    chosen_department(input$departments)
  })
  observeEvent(input$disciplines,{
    chosen_discipline(input$disciplines)
  })
  observeEvent(input$courses,{
    chosen_course(input$courses)
  })
  
  elac_normal<-reactiveValues(graph1=NULL,graph2=NULL,graph3=NULL,graph4=NULL,graph5=NULL)
  elac_enroll_limit<-reactiveValues(graph1=NULL,graph2=NULL,graph3=NULL,graph4=NULL,graph5=NULL)
  
  sgec_normal<-reactiveValues(graph1=NULL,graph2=NULL,graph3=NULL,graph4=NULL,graph5=NULL)
  sgec_enroll_limit<-reactiveValues(graph1=NULL,graph2=NULL,graph3=NULL,graph4=NULL,graph5=NULL)
  
  
  
  semesters = reactive({
    if(!is.null(input$year))
    {
      sem = getSemesters(input$year)
      
      sem = factor(sem, levels = c("Summer","Fall","Winter","Spring"),ordered=TRUE)
      sem = sem[order(sem)]
      semesters = as.character(sem)
    }
  })
  
  programs = reactive({
    if(!is.null(input$year) & !is.null(input$semester))
    {
      programs = getPrograms(input$year,semesterCat( input$semester))
      
      programs = factor(programs, levels = c("Not part of a program", setdiff(levels(factor(programs)),"Not part of a program")), ordered = TRUE)
      
      programs = programs[order(programs)]
      programs = as.character(programs)
    }
  })
  
  departments = reactive({
    if(!is.null(input$semester) & !is.null(input$year) & !is.null(input$programs))
    {
      getDepartments(input$year, semesterCat(input$semester),input$programs)
    }
  })
  
  disciplines = reactive({
    if(!is.null(input$departments) & !is.null(input$semester) & !is.null(input$year) & !is.null(input$programs))
    {
      getDisciplines(input$year, semesterCat(input$semester),input$programs,input$departments)
    }
  })
  
  courses = reactive({
    if(!is.null(input$semester) & !is.null(input$year) & !is.null(input$departments) & !is.null(input$disciplines))
    {
      getCourses(input$year, semesterCat(input$semester), input$departments, input$disciplines,input$programs)
    }
  })
  
  
  mainCampusPlot = reactive({
    if(!is.null(input$course))
    {
      g = getSchedulingPlots(course_enrollment_data(), input$year,input$semester,0)
      grid.draw(g)
    }
  })
  
  sgecPlot = reactive({
    if(!is.null(input$course))
    {
      g = getSchedulingPlots(course_enrollment_data(), input$year,input$semester,1)
      grid.draw(g)
    }
  })
  
  observeEvent( input$year,{
    if(!is.null(semesters())){
      updateRadioButtons(session=session,inputId = "semester",label = "Semester", choices = semesters())
    }
  })
  
  observe(if(!is.null(input$year) && !is.null(semesters()) && !is.null(programs())){
    updateSelectInput(session=session,inputId = "programs",label="Program",choices=programs())
  })
  
  observe(if(!is.null( departments())){
    if(!is.null(isolate(chosen_department())) && isolate(chosen_department()) %in% departments()){
      updateSelectInput(session=session,inputId="departments",label="Department",choices = departments(),selected=chosen_department())
    }else{
      # isolate(chosen_department(departments()[1]))
      updateSelectInput(session=session,inputId="departments",label="Department",choices = departments(),selected=departments()[1])
    }
    
  })
  
  observe(if(!is.null(disciplines())){
    
    if(!is.null(isolate(chosen_discipline())) && isolate(chosen_discipline()) %in% disciplines()){
      updateSelectInput(session=session,inputId="disciplines",label="Disciplines",choices = disciplines(),selected=chosen_discipline())
    }else{
      updateSelectInput(session=session,inputId="disciplines",label="Disciplines",choices = disciplines(),selected=disciplines()[1])
    }
    
  })
  
  observe(if(!is.null(courses())){
    
    if(!is.null(isolate(chosen_course())) && isolate(chosen_course()) %in% courses()){
      updateSelectInput(session=session,inputId="courses",label="Courses",choices = courses(), selected=chosen_course())
    }
    else{
      updateSelectInput(session=session,inputId="courses",label="Courses",choices = courses(),selected=courses()[1])
    }
    
  })
  
  #when you submit without advanced is entered
  observeEvent(input$submit,{
    
    if( isolate(input$departments)!="Select Department" && isolate(input$disciplines)!="Select Discipline" &&
        isolate(input$courses)!="Select Course"){
      shinyjs::show("weeks_chosen")
      shinyjs::show("weeks_chosen_s")
      #-----------------------Reset boxes --------------------------------------
      shinyjs::reset("box1")
      output$box2<-renderUI({NULL})
      output$box3<-renderUI({NULL})
      output$box4<-renderUI({NULL})
      output$box5<-renderUI({NULL})
      
      #-----------------Title change add "(Predictions)" if needed------------------
      if(isolate(input$year)==earliest_projection_year()){
        output$title_sgec<-renderUI({
          h3("Course Schedule (Predictions)")
        })
        output$title_main<-renderUI({
          h3("Course Schedule (Predictions)")
        })
      }
      else{
        output$title_sgec<-renderUI({
          h3("Course Schedule")
        })
        output$title_main<-renderUI({
          h3("Course Schedule")
        })
      }
      
      #--------------------------------VALUE BOXES-------------------------------
      
      year_=adjust_year(isolate(input$semester),isolate(input$year))
      
      
      output$box_time<-renderValueBox(valueBox(value=tags$p(HTML((paste(isolate(input$year),br(), isolate(input$semester),"(",year_,")")))
                                                            ,style="font-size:90%"),
                                               subtitle="Time Frame",icon=icon("clock-o"),color="aqua"))
      
      output$box_class<-renderValueBox(valueBox(value=tags$p(HTML(paste(isolate(input$departments),isolate(input$disciplines),
                                                                        isolate(input$courses), sep="<br/>")),style="font-size:80%"),
                                                subtitle="Department,Discipline, Course",icon=icon("building"),color="aqua"))
      
      output$box_program<-renderValueBox(valueBox(value=HTML(paste(isolate(input$programs), sep="<br/>")),
                                                  subtitle="Program",icon=icon("users"),color="aqua"))
      
      #-------------------------------DATA-----------------------------------------------------
      data(getCourseEnrollmentData(isolate(input$year), isolate(semesterCat(input$semester)),isolate(input$programs), 
                                   isolate(input$departments), isolate(input$disciplines), isolate(input$courses)))
      excel_data<-data()
      #include academic_year,semester, program,department,discipline,course
      if(!is.null(excel_data)){
        
        excel_data$Academic_Year<- isolate(input$year)
        excel_data$Semester <-isolate(input$semester)
        excel_data$Program <-isolate(input$programs)
        excel_data$Department<-isolate(input$departments)
        excel_data$Discipline<- isolate(input$disciplines)
        excel_data=excel_data[c(11:15,6,1:5,7:10)]
        colnames(excel_data)[9]<-"Location"
        if(nrow(excel_data[excel_data$Location==0,])> 0)
          excel_data[excel_data$Location==0,]$Location<-"Main Campus"
        if(nrow(excel_data[excel_data$Location==1,])>0)
          excel_data[excel_data$Location==1,]$Location<-"SGEC"
        if(isolate(input$semester)!="Summer")
          excel_data$Summer_Session<-NULL
        
        data_excel(excel_data)
      }
      
      #---------------------------TABLE--------------------------------------------------------
      
      if(nrow(data())!=0){
        weeks_available<-weeks_session(isolate(input$semester))
        
        updateSelectizeInput(session=session,inputId = "weeks_chosen",choices=weeks_available,label=NULL)
        
        #---------------------------GRAPHS--------------------------------------------------------
        #-------------------------------RENDER BOXES WHERE GRAPHS LIE---------------------------------------------------
        #----------box 1----------------------------------
        
        length_1=box_length(weeks_available[1])
        #create graph   
        
        #use for enrollment limit toggle 
        elac_enroll_limit$graph1<-get_graph(12,weeks_available[1],SGEC=F,Enrollment_Limit=T)
        
        #normal 80 limit
        elac_normal$graph1<-get_graph(12,weeks_available[1],SGEC=F)
        
        if(isolate(input$limit)==T){
          output$graph1<-renderPlotly({
            isolate(elac_enroll_limit$graph1)
          })
        }
        else{
          output$graph1<-renderPlotly({
            isolate(elac_normal$graph1)
          })
        }
        
        
        
        output$box1<-renderUI({
          box(title = weeks_available[1],solidHeader = T,
              width=12,collapsible = T,status="danger",plotlyOutput("graph1"))
        })
        
        if(length_1==6){
          output$box1<-renderUI({
            box(title = weeks_available[1],solidHeader = T,
                width=length_1,collapsible = T,status="danger",plotlyOutput("graph1",height=300))
          })
        }
        else{
          output$box1<-renderUI({
            box(title = weeks_available[1],solidHeader = T,
                width=length_1,collapsible = T,status="danger",plotlyOutput("graph1"))
          })
        }
        
        #----------box 2--------------------------------
        if(length(weeks_available)>=2){
          #decide if need to create cart by adding random days and times
          length_2=box_length(weeks_available[2])
          #use for enrollment limit toggle 
          elac_normal$graph2<-get_graph(length_2,weeks_available[2],SGEC=F,Enrollment_Limit=F)
          
          elac_enroll_limit$graph2<- get_graph(length_2,weeks_available[2],SGEC=F,Enrollment_Limit=T)
          
          if(isolate(input$limit)==T){
            output$graph2<-renderPlotly({
              isolate(elac_enroll_limit$graph2)
            })
          }
          else{
            #create graph
            output$graph2<-renderPlotly({
              isolate(elac_normal$graph2)
            })
          }
          
          
          
          #create box 
          if(length_2==6){
            output$box2<-renderUI({
              box(title = weeks_available[2],solidHeader = T,
                  width=length_2,collapsible = T,status="danger",plotlyOutput("graph2",height=300))
            })
          }
          else{
            output$box2<-renderUI({
              box(title = weeks_available[2],solidHeader = T,
                  width=length_2,collapsible = T,status="danger",plotlyOutput("graph2"))
            })
          }
        }
        #----------box 3--------------------------------
        if(length(weeks_available)>=3){
          length_3=box_length(weeks_available[3]) 
          
          #use for enrollment limit toggle 
          elac_normal$graph3<-get_graph(length_3,weeks_available[3],SGEC=F,Enrollment_Limit=F)
          
          elac_enroll_limit$graph3<-get_graph(length_3,weeks_available[3],SGEC=F,Enrollment_Limit=T)
          
          if(isolate(input$limit)==T){
            output$graph3<-renderPlotly({
              isolate(elac_enroll_limit$graph3)
            })
          }
          else{
            #create graph
            output$graph3<-renderPlotly({
              isolate(elac_normal$graph3)
            })
          }
          
          
          
          
          if(length_3==6){
            output$box3<-renderUI({
              box(title = weeks_available[3],solidHeader = T,
                  width=length_3,collapsible = T,status="danger",plotlyOutput("graph3",height=300))
            })
          }
          else{
            output$box3<-renderUI({
              box(title = weeks_available[3],solidHeader = T,
                  width=length_3,collapsible = T,status="danger",plotlyOutput("graph3"))
            })
          }
        }
        #----------box 4--------------------------------
        if(length(weeks_available)>=4){
          length_4=box_length(weeks_available[4]) 
          
          elac_normal$graph4<-get_graph(length_4,weeks_available[4],SGEC=F,Enrollment_Limit=F)
          
          #use for enrollment limit toggle 
          elac_enroll_limit$graph4<-get_graph(length_4,weeks_available[4],SGEC=F,Enrollment_Limit=T)
          
          if(isolate(input$limit)==T){
            output$graph4<-renderPlotly({
              isolate(elac_enroll_limit$graph4)
            })
          }
          else{
            #create graph
            output$graph4<-renderPlotly({
              isolate(elac_normal$graph4)
            })
          }
          
          if(length_4==6){
            output$box4<-renderUI({
              box(title = weeks_available[4],solidHeader = T,
                  width=length_4,collapsible = T,status="danger",plotlyOutput("graph4",height=300))
            })
          }
          else{
            output$box4<-renderUI({
              box(title = weeks_available[4],solidHeader = T,
                  width=length_4,collapsible = T,status="danger",plotlyOutput("graph4"))
            })
          }
        }
        #----------box 5--------------------------------
        if(length(weeks_available)>=5){
          length_5=box_length(weeks_available[5]) 
          elac_normal$graph5<-get_graph(length_5,weeks_available[5],SGEC=F,Enrollment_Limit=F)
          
          #use for enrollment limit toggle 
          elac_enroll_limit$graph5<-get_graph(length_5,weeks_available[5],SGEC=F,Enrollment_Limit=T)
          
          if(isolate(input$limit)==T){
            output$graph5<-renderPlotly({
              isolate(elac_enroll_limit$graph5)
            })
          }
          else{
            #create graph
            output$graph5<-renderPlotly({
              isolate(elac_normal$graph5)
            })
          }
          
          
          if(length_5==6){
            output$box5<-renderUI({
              box(title = weeks_available[5],solidHeader = T,
                  width=length_5,collapsible = T,status="danger",plotlyOutput("graph5",height=300))
            })
          }
          else{
            output$box5<-renderUI({
              box(title = weeks_available[5],solidHeader = T,
                  width=length_5,collapsible = T,status="danger",plotlyOutput("graph5"))
            })
          }
        }
        
        #Rankings table
        output$table<-renderTable(get_rankings(weeks_available[1],Sgec=F),bordered =TRUE,rownames=TRUE, width=1000)#main campus
        #--------------------------------SOUTH GATE------------------------------------------------
        
        
        weeks_available_s<-weeks_session(isolate(input$semester),ELAC=F)
        
        if(!is.null(weeks_available_s)){
          #------------------------------Table------------------------------------------------------
          updateSelectizeInput(session=session,inputId = "weeks_chosen_s",choices=weeks_available_s,label=NULL)
          
          #------------------------------Graphs-----------------------------
          #----------------box 1---------------------------------------------
          length=box_length(weeks_available_s[1],SGEC=T)  
          
          sgec_normal$graph1=get_graph(length=length,week=weeks_available_s[1])
          sgec_enroll_limit$graph1=get_graph(length=length,week=weeks_available_s[1],Enrollment_Limit=T)
          
          if(isolate(input$limit_s)==T){
            output$graph1_s<-renderPlotly({
              isolate(sgec_enroll_limit$graph1)
            })
          }
          else{
            output$graph1_s<-renderPlotly({
              isolate(sgec_normal$graph1)
            })
          }
          
          if(length==6){
            output$box1_s<-renderUI({
              box(title = weeks_available_s[1],solidHeader = T,
                  width=length,collapsible = T,status="danger",plotlyOutput("graph1_s",height=300))
            })
          }
          else{
            output$box1_s<-renderUI({
              box(title = weeks_available_s[1],solidHeader = T,
                  width=length,collapsible = T,status="danger",plotlyOutput("graph1_s"))
            })
          }
          
          #----------------box 2-----------------------------------------
          if(length(weeks_available_s)>=2){
            #decide if need to create cart by adding random days and times
            length=box_length(weeks_available_s[2],SGEC=T)
            
            sgec_normal$graph2=get_graph(length,weeks_available_s[2],SGEC=T)
            sgec_enroll_limit$graph2=get_graph(length,weeks_available_s[2],SGEC=T,Enrollment_Limit=T)
            
            if(isolate(input$limit_s)==T){
              output$graph2_s<-renderPlotly({
                isolate(sgec_enroll_limit$graph2)
              })
            }
            else{
              #create graph
              output$graph2_s<-renderPlotly({
                isolate(sgec_normal$graph2)
              })
            }
            
            #create box 
            if(length==6){
              output$box2_s<-renderUI({
                box(title = weeks_available_s[2],solidHeader = T,
                    width=length,collapsible = T,status="danger",plotlyOutput("graph2_s",height=300))
              })
            }
            else{
              output$box2_s<-renderUI({
                box(title = weeks_available_s[2],solidHeader = T,
                    width=length,collapsible = T,status="danger",plotlyOutput("graph2_s"))
              })
            }
          }
          #----------------box 3------------------------------------------
          if(length(weeks_available_s)>=3){
            #decide if need to create cart by adding random days and times
            length=box_length(weeks_available_s[3],SGEC=T)
            
            sgec_normal$graph3=get_graph(length,weeks_available_s[3],SGEC=T)
            sgec_enroll_limit$graph3=get_graph(length,weeks_available_s[3],SGEC=T,Enrollment_Limit=T)
            
            if(isolate(input$limit_s)==T){
              output$graph3_s<-renderPlotly({
                isolate(sgec_enroll_limit$graph3)
              })
            }
            else{
              #create graph
              output$graph3_s<-renderPlotly({
                isolate(sgec_normal$graph3)
              })
            }
            
            #create box 
            if(length==6){
              output$box3_s<-renderUI({
                box(title = weeks_available_s[3],solidHeader = T,
                    width=length,collapsible = T,status="danger",plotlyOutput("graph3_s",height=300))
              })
            }
            else{
              output$box3_s<-renderUI({
                box(title = weeks_available_s[3],solidHeader = T,
                    width=length,collapsible = T,status="danger",plotlyOutput("graph3_s"))
              })
            }
          }
          #----------------box 4-----------------------------------------
          if(length(weeks_available_s)>=4){
            #decide if need to create cart by adding random days and times
            length=box_length(weeks_available_s[4],SGEC=T)
            
            sgec_normal$graph4=get_graph(length,weeks_available_s[4],SGEC=T)
            sgec_enroll_limit$graph4=get_graph(length,weeks_available_s[4],SGEC=T,Enrollment_Limit=T)
            
            if(isolate(input$limit_s)==T){
              output$graph4_s<-renderPlotly({
                isolate(sgec_enroll_limit$graph4)
              })
            }
            else{
              #create graph
              output$graph4_s<-renderPlotly({
                isolate(sgec_normal$graph4)
              })
            }
            
            #create box 
            if(length==6){
              output$box4_s<-renderUI({
                box(title = weeks_available_s[4],solidHeader = T,
                    width=length,collapsible = T,status="danger",plotlyOutput("graph4_s",height=300))
              })
            }
            else{
              output$box4_s<-renderUI({
                box(title = weeks_available_s[4],solidHeader = T,
                    width=length,collapsible = T,status="danger",plotlyOutput("graph4_s"))
              })
            }
          }
          #----------------box 5--------------------------------------
          if(length(weeks_available_s)>=5){
            #decide if need to create cart by adding random days and times
            length=box_length(weeks_available_s[5],SGEC=T)
            
            sgec_normal$graph5=get_graph(length,weeks_available_s[5],SGEC=T)
            sgec_enroll_limit$graph5=get_graph(length,weeks_available_s[5],SGEC=T,Enrollment_Limit=T)
            
            if(isolate(input$limit_s)==T){
              output$graph5_s<-renderPlotly({
                isolate(sgec_enroll_limit$graph5)
              })
            }
            else{
              #create graph
              output$graph5_s<-renderPlotly({
                isolate(sgec_normal$graph5)
              })
            }
            
            #create box 
            if(length==6){
              output$box5_s<-renderUI({
                box(title = weeks_available_s[5],solidHeader = T,
                    width=length,collapsible = T,status="danger",plotlyOutput("graph5_s",height=300))
              })
            }
            else{
              output$box5_s<-renderUI({
                box(title = weeks_available_s[5],solidHeader = T,
                    width=length,collapsible = T,status="danger",plotlyOutput("graph5_s"))
              })
            }
          }
          
          
          #Rankings table
          output$table_s<-renderTable(get_rankings(week=weeks_available_s[1],Sgec=T),bordered =TRUE,rownames=TRUE, width=1000)#main campus
        }
      }
      else{
        showNotification("No data for this course selection",type="error")
      }
      
      if(nrow((data()[data()$SGEC==1,]))==0){
        updateTabsetPanel(session, "location", selected = "main_campus")
      }
    }
    else{
      showNotification("Department, Discipline, or Course were not selected",type="error", duration = 10)
    }
  })
  
  #--------------------------------------------------------------------Submission with advanced settings-----------------------------------------------------
  #Note can never run both enters because never are both displayed at once
  observeEvent(input$submit_advanced,{
    #hide weeks dropdown before table
    shinyjs::hide("weeks_chosen")
    shinyjs::hide("weeks_chosen_s")
    
    chose_filter=F
    filter_string=NULL
    actual_filter=NULL
    if(isolate(input$filter_option)=="Meeting Day"){
      filter_string="Meeting Days"
      actual_filter=isolate(input$meeting_day)
      if(isolate(input$meeting_day)!="Select Day")
        chose_filter=T
    }else{
      filter_string="Start Period"
      actual_filter=isolate(input$start_period)
      if(isolate(input$start_period)!="Select Time")
        chose_filter=T
    }
    
    
    if( isolate(input$departments)!="Select Department" && isolate(input$disciplines)!="Select Discipline" &&
        isolate(input$week_filter)!="Select Week" && chose_filter){
      #-----------------------Reset boxes --------------------------------------
      shinyjs::reset("box1")
      output$box2<-renderUI({NULL})
      output$box3<-renderUI({NULL})
      output$box4<-renderUI({NULL})
      output$box5<-renderUI({NULL})
      
      #-----------------Title change add "(Predictions)" if needed------------------
      if(isolate(input$year)==earliest_projection_year()){
        output$title_sgec<-renderUI({
          h3("Course Schedule (Predictions)")
        })
        output$title_main<-renderUI({
          h3("Course Schedule (Predictions)")
        })
      }
      else{
        output$title_sgec<-renderUI({
          h3("Course Schedule")
        })
        output$title_main<-renderUI({
          h3("Course Schedule")
        })
      }
      #--------------------------------VALUE BOXES-------------------------------
      
      year_=adjust_year(isolate(input$semester),isolate(input$year))
      
      
      output$box_time<-renderValueBox(valueBox(value=tags$p(HTML((paste(isolate(input$year),br(), isolate(input$semester),"(",year_,")")))
                                                            ,style="font-size:90%"),
                                               subtitle="Time Frame",icon=icon("clock-o"),color="aqua"))
      
      output$box_class<-renderValueBox(valueBox(value=tags$p(HTML(paste(isolate(input$departments),isolate(input$disciplines)
                                                                        , sep="<br/>")),style="font-size:80%"),
                                                subtitle="Department,Discipline",icon=icon("building"),color="aqua"))
      
      output$box_program<-renderValueBox(valueBox(value=HTML(paste(isolate(input$programs), sep="<br/>")),
                                                  subtitle="Program",icon=icon("users"),color="aqua"))
      if(nrow(discipline_data())!=0){
        if(filter_string=="Start Period")
          filter_chosen<-paste(isolate(input$week_filter)," (", isolate(input$start_period),")")
        else{
          filter_chosen<-paste(isolate(input$week_filter)," (", isolate(input$meeting_day),")")
        }
        #-------------------------------RENDER Graph and table for main campus--------------------------------------------------
        
        if(isolate(input$filter_option=="Meeting Day")){
          #use for enrollment limit toggle 
          elac_enroll_limit$graph1<-isolate(get_advaced_graph(data=discipline_data(),week=input$week_filter,
                                                              filter=input$filter_option, filter_option=input$meeting_day, SGEC=F,Enrollment_Limit=T))
          
          #normal 80 limit
          elac_normal$graph1<-isolate(get_advaced_graph(data=discipline_data(),week=input$week_filter,
                                                        filter=input$filter_option, filter_option=input$meeting_day, SGEC=F,Enrollment_Limit=F))
        }
        else{
          #use for enrollment limit toggle 
          elac_enroll_limit$graph1<-isolate(get_advaced_graph(data=discipline_data(),week=input$week_filter,
                                                              filter=input$filter_option, filter_option=input$start_period, SGEC=F,Enrollment_Limit=T))
          
          #normal 80 limit
          elac_normal$graph1<-isolate(get_advaced_graph(data=discipline_data(),week=input$week_filter,
                                                        filter=input$filter_option, filter_option=input$start_period, SGEC=F,Enrollment_Limit=F))
        }
        
        if(isolate(input$limit)==T){
          output$graph1<-renderPlotly({
            isolate(elac_enroll_limit$graph1)
          })
        }
        else{
          output$graph1<-renderPlotly({
            isolate(elac_normal$graph1)
          })
        }
        output$box1<-renderUI({
          box(title = filter_chosen ,solidHeader = T,
              width=12,collapsible = T,status="danger",plotlyOutput("graph1"))
        })
        
        #Rankings table
        output$table<-renderTable(isolate(get_rankings(input$week_filter,Sgec=F, data=discipline_data(),advanced=filter_string,filter_option=actual_filter)),bordered =TRUE,rownames=TRUE, width=1000)#main campus
        excel_temp<-discipline_data()
        # if(isolate(input$filter_option)=="Meeting Day"){
        #   print(input$meeting_day)
        #   print(input$week_filter)
        #   excel_temp<-excel_temp[excel_temp$Meeting_Days==isolate(input$meeting_day) && 
        #                            excel_temp$Weeks==isolate(input$week_filter),]
        # }
        # else{
        #   excel_temp<-excel_temp[excel_temp$Start_Period==isolate(input$start_period) && 
        #                            excel_temp$Weeks==isolate(input$week_filter),]
        # }
        
        if(nrow(excel_temp)!=0){
          
          excel_temp$Academic_Year<-isolate(input$year)
          excel_temp$Semester<-isolate(input$semester)
          excel_temp$Department<-isolate(input$departments)
          excel_temp$Discipline<-isolate(input$disciplines)
          colnames(excel_temp)[3]<-"Location"
          if(nrow(excel_temp[excel_temp$Location==0,])> 0)
            excel_temp[excel_temp$Location==0,]$Location<-"Main Campus"
          if(nrow(excel_temp[excel_temp$Location==1,])>0)
            excel_temp[excel_temp$Location==1,]$Location<-"SGEC"
          
          excel_temp<-excel_temp[c(11:14,6,1:5,7:10)]
          
          if(isolate(input$semester)!="Summer")
            excel_temp$Summer_Session<-NULL
          
          
        }
        data_excel(excel_temp)
        
        #---------------------------------------------------------SOUTH GATE CAMPUS----------------------------------------------------------------------------------
        #if south gate is available
        if(isolate(south_gate_available(data=discipline_data(),semester=input$semester,week=input$week_filter, selection=input$filter_option, filter_type=actual_filter))){
          shinyjs::show(selector = "#location li a[data-value=SGEC]")
          
          sgec_normal$graph1<-isolate(get_advaced_graph(data=discipline_data(),week=input$week_filter,
                                                        filter=input$filter_option, filter_option=actual_filter, SGEC=T,Enrollment_Limit=F))
          sgec_enroll_limit$graph1=isolate(get_advaced_graph(data=discipline_data(),week=input$week_filter,
                                                             filter=input$filter_option, filter_option=actual_filter, SGEC=T,Enrollment_Limit=T))
          
          if(isolate(input$limit_s)==T){
            output$graph1_s<-renderPlotly({
              isolate(sgec_enroll_limit$graph1)
            })
          }
          else{
            output$graph1_s<-renderPlotly({
              isolate(sgec_normal$graph1)
            })
          }
          
          output$box1_s<-renderUI({
            box(title = filter_chosen ,solidHeader = T,
                width=12,collapsible = T,status="danger",plotlyOutput("graph1_s"))
          })
          output$table_s<-renderTable(get_rankings(isolate(input$week_filter),Sgec=T, data=discipline_data(),advanced=filter_string,filter_option=actual_filter),bordered =TRUE,rownames=TRUE, width=1000)#main campus
          
          
        }
        else{#no options for south gate
          updateTabsetPanel(session, "location", selected = "main_campus")
          shinyjs::hide(selector = "#location li a[data-value=SGEC]")
          
        }
      }  
      
      
      
    }else{
      showNotification(paste("You did not select Department, Discipline, Week, or ",filter_string),duration=10,type="error")
    }
  })
  
  
  #remove the SGEC tab if no southgate campus class available
  observe(if(!is.null(data())){
    
    if(nrow((data()[data()$SGEC==1,]))>0){
      shinyjs::show(selector = "#location li a[data-value=SGEC]")
      
    }
    else{
      shinyjs::hide(selector = "#location li a[data-value=SGEC]")
    }
  })
  
  observe(if(input$weeks_chosen!=""){
    
    output$table<-renderTable(get_rankings(isolate(input$weeks_chosen),Sgec=F),bordered =TRUE,rownames=TRUE, width=1000)#main campus
    
  })
  
  observe(if(input$limit==T){
    #----ELAC---------
    #----------box 1----------------------------------
    
    output$graph1<-renderPlotly({
      isolate(elac_enroll_limit$graph1)
    })
    
    #----------box 2--------------------------------
    if(isolate(!is.null(elac_enroll_limit$graph2))){
      
      output$graph2<-renderPlotly({
        isolate(elac_enroll_limit$graph2)
      })
      
    }
    #----------box 3--------------------------------
    if(isolate(!is.null(elac_enroll_limit$graph3))){
      
      #create graph
      output$graph3<-renderPlotly({
        isolate(elac_enroll_limit$graph3)
      })
      
    }
    #----------box 4--------------------------------
    if(isolate(!is.null(elac_enroll_limit$graph4))){
      
      #create graph
      output$graph4<-renderPlotly({
        isolate(elac_enroll_limit$graph4)
      })
      
    }
    #----------box 5--------------------------------
    if(isolate(!is.null(elac_enroll_limit$graph5))){
      
      #create graph
      output$graph5<-renderPlotly({
        isolate(elac_enroll_limit$graph5)
      })
      
      
    }
    
    
  }
  else{
    #-------ELAC-----------
    #----------box 1----------------------------------
    
    output$graph1<-renderPlotly({
      isolate(elac_normal$graph1)
    })
    
    #----------box 2--------------------------------
    if(isolate(!is.null(elac_normal$graph2))){
      
      output$graph2<-renderPlotly({
        isolate(elac_normal$graph2)
      })
      
    }
    #----------box 3--------------------------------
    if(isolate(!is.null(elac_normal$graph3))){
      
      #create graph
      output$graph3<-renderPlotly({
        isolate(elac_normal$graph3)
      })
      
    }
    #----------box 4--------------------------------
    if(isolate(!is.null(elac_normal$graph4))){
      
      #create graph
      output$graph4<-renderPlotly({
        isolate(elac_normal$graph4)
      })
      
    }
    #----------box 5--------------------------------
    if(isolate(!is.null(elac_normal$graph5))){
      
      #create graph
      output$graph5<-renderPlotly({
        isolate(elac_normal$graph5)
      }) 
    }
    
  })
  
  observe(if(input$limit_s==T){
    
    #========sgec===============
    #----------box 1----------------------------------
    
    output$graph1_s<-renderPlotly({
      isolate(sgec_enroll_limit$graph1)
    })
    
    #----------box 2--------------------------------
    if(isolate(!is.null(sgec_enroll_limit$graph2))){
      
      output$graph2_s<-renderPlotly({
        isolate(sgec_enroll_limit$graph2)
      })
      
    }
    #----------box 3--------------------------------
    if(isolate(!is.null(sgec_enroll_limit$graph3))){
      
      #create graph
      output$graph3_s<-renderPlotly({
        isolate(sgec_enroll_limit$graph3)
      })
      
    }
    #----------box 4--------------------------------
    if(isolate(!is.null(sgec_enroll_limit$graph4))){
      
      #create graph
      output$graph4_s<-renderPlotly({
        isolate(sgec_enroll_limit$graph4)
      })
      
    }
    #----------box 5--------------------------------
    if(isolate(!is.null(sgec_enroll_limit$graph5))){
      
      #create graph
      output$graph5_s<-renderPlotly({
        isolate(sgec_enroll_limit$graph5)
      })
      
      
    } 
  }
  else{
    
    #=========sgec============
    #----------box 1----------------------------------
    
    output$graph1_s<-renderPlotly({
      isolate(sgec_normal$graph1)
    })
    
    #----------box 2--------------------------------
    if(isolate(!is.null(sgec_normal$graph2))){
      
      output$graph2_s<-renderPlotly({
        isolate(sgec_normal$graph2)
      })
      
    }
    #----------box 3--------------------------------
    if(isolate(!is.null(sgec_normal$graph3))){
      
      #create graph
      output$graph3_s<-renderPlotly({
        isolate(sgec_normal$graph3)
      })
      
    }
    #----------box 4--------------------------------
    if(isolate(!is.null(sgec_normal$graph4))){
      
      #create graph
      output$graph4_s<-renderPlotly({
        isolate(sgec_normal$graph4)
      })
      
    }
    #----------box 5--------------------------------
    if(isolate(!is.null(sgec_normal$graph5))){
      
      #create graph
      output$graph5_s<-renderPlotly({
        isolate(sgec_normal$graph5)
      })
      
      
    }   
  })
  
  #-------Dowanload Excel spreeadsheet---------------------
  # Downloadable csv of selected dataset ----
  output$download_excel <- downloadHandler(
    filename = function() {
      paste(input$year,"-",input$semester,"-",input$programs,"-",input$departments,
            "-",input$disciplines,"-",input$courses, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_excel(), file, row.names = FALSE)
    }
  )
})

shinyApp(ui = ui, server = server)
