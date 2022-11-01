library(shinydashboard)
library(DT)
library(readr)
library(ijtiff)
library(raster)
library(imager)
library(shiny)
library(shinyWidgets)
library(RMySQL)
library(DBI)
makereactivetrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

dbtrigger <- makereactivetrigger()
con <- dbConnect(MySQL(), user = 'root', password = '1234',
                 dbname = 'R_project', host = 'localhost')

#dbExecute(con, 'CREATE TABLE student_details12
   #       (name varchar(10), fathername varchar(10));')



ui<-dashboardPage(skin="red",
                  dashboardHeader(title="Student Admission"),
                  dashboardSidebar(
                    width=200,
                    sidebarMenu(
                      menuItem(h4("Home") , tabName = "Home", icon = icon("house-user")),
                      menuItem(h4("About") , tabName = "About", icon = icon("plane")),
                      menuItem(h4("Details") , tabName = "Details", icon = icon("train")),
                      menuItem(h4("Students Marks"), tabName = "StudentsMarks", icon = icon("plane")),
                      menuItem(h4("Eligibility"), tabName = "Eligibility", icon = icon("book")),
                      menuItem(h4("Placement Criteria"), tabName = "PlacementCriteria", icon = icon("bell")),

                      menuItem(h4("Applications Received"), tabName = "ApplicationsReceived", icon = icon("plane")),
                      menuItem(h4("Placement"), tabName = "Placement", icon = icon("book"))
                      ,
                      menuItem(h4("Feedback"), tabName = "Feedback", icon = icon("train"))
                      
                      
                    )),
                  dashboardBody(
                    
                    
                    tabItems(
                      tabItem("Home",
                              img(src="rosi.png",height=800,width=1500)
                      ),
                      tabItem("About",
                              h1("WELCOME TO OUR  PLACEMENT MANAGEMENT SYSTEM"),
                              h4("The college had its origin as an engineering college which started in 1968 by Thomas. It is a co-educational college imparting education to more than 10000 students. The college celebrated its Silver Jubilee in the year 1993 and its Golden Jubilee in the year 2020."),
                              h4("This college is located in Ooty with excellent infrastructure, talented faculties and good facilities."),
                              h1(),
                              h2("Description:"),
                              h4("Courses: BE/Btech,Integrated MSc,Commerce and Management,Science and Tech"),
                              h4("Affiliated to : Anna University"),
                              h4("Website: http://www.studentmanagement.edu.in"),
                              h4("Email: studentmanagement@gmail.com"),
                              h1(),
                              h2("Telephone:"),
                              h4("9253679815/044-22871974"),
                              h2("Location:"),
                              h4("No.34, Dr.Gokulan street"),
                              h4("Radha Nagar"),
                              h4("Ooty-643001"),
                              h2("Categories:"),
                              h4("Ooty College"),
                              h4("Mettupalayam College"),
                              h4("Ooty engineering college"),
                              h4("Ooty University"),
                              h2("Board Members:"),
                              h3("Founder:"),h4("Rosina Rosinz"),
                              h3("Correspondent:"),h4("Amir Suhail"),
                              h3("Principal:"),h4("Riya Thomas")
                              
                              
                      ),
                      tabItem("Details",
                              fluidPage(
                                textInput("name", 'Enter your Name:', ""),
                                textInput("dateofbirth", 'Enter your Date of Birth:', ""),
                                selectInput("gender", 'Select Gender:',c("Male","Female")),
                                textInput("fathername", 'Enter your Father Name:', ""),
                                textInput("mothername", 'Enter your Mother Name:', ""),
                                selectInput("std", 'Select Course:',c("Science & tech","Communication & Managemnet","Data Science","Artificial Intelligence","Software Systems","Decision and computing science","Others")),
                                selectInput("religion", 'Select religion:',c("Hindu", "Muslim", "Christian", "Others")),
                                textInput("phonenumber", 'Enter Parents phone number:', ""),
                                textAreaInput("address", 'Enter your Address:', ""),
                                textInput("txt2", 'Enter your Place of Birth:', ""),
                                actionButton('writetodb', 'SUBMIT'),
                              )
                              
                      ),
                      
                      tabItem("StudentsMarks",
                              fluidPage(
                                h1("Enter your 10th marks:"),
                                textInput("tamil", 'Enter Tamil mark:', ""),
                                textInput("english", 'Enter English mark:', ""),
                                textInput("maths", 'Enter Maths mark:', ""),
                                textInput("science", 'Enter Science mark:', ""),
                                textInput("social", 'Enter Social mark:', ""),
                                textInput("percentage", 'Enter your 10th percentage:', ""),
                                selectInput("Board", 'Select Board:',c("Central","Others")),
                                h2("Enter your 12th marks :"),
                                h3("Select your 12th stream :"),
                                selectInput("Stream", 'Select Stream:',c("Commerce","Science","Arts")),
                                textInput("tamil", 'Enter Tamil mark:', ""),
                                textInput("english", 'Enter English mark::', ""),
                                textInput("maths", 'Enter Maths mark:', ""),
                                textInput("phy", 'Enter Physics mark:', ""),
                                textInput("chem", 'Enter Chemistry mark:', ""),
                                textInput("comp",'Enter Commerce /science/Arts mark : ',""),
                                textInput("percentage", 'Enter your 12th percentage:', ""),
                                selectInput("Board", 'Select Board:',c("Central","Others")),
                                actionButton('writetodb', 'SAVE'),
                              )
                                
                              ),
                      tabItem("Eligibility",
                                       fluidPage(
                                         h2("A Pass in HSC / equivalent exam with Mathematics, 
                                            Physics and Chemistry as the subjects of study. 
                                            However, Candidates (other than SC/ST) with an aggregate of 75% 
                                            and above in Mathematics, Physics, and Chemistry or 
                                            Computer Science only need to apply."),
                                       )
                                       
                              ),
                      tabItem("PlacementCriteria",
                              fluidPage(
                                h1("PLACEMENT CRITERIA :"),
                              h2("Based on the following :"),
                              h2(),
                              h2("i)60 per cent in 10th & 12th"),
                              h2("ii)70 per cent in Graduate"),
                              h2("iii) No drop in any semester/ year throughout the course."),
                              h2("iv) Max of two backlogs/ re-attempts in your under graduation"),
                              )
                              ),
                      tabItem("Placement",
                              img(src="download.png",height=400,width=800),
                              img(src="line.png",height=400,width=700),
                              img(src="pic.png",height=400,width=800)
                      ),
                      tabItem("ApplicationsReceived",
                              img(src="app.png",height=800,width=1200)
                      ),
                      tabItem("Feedback",
                                      fluidPage(
                                        textInput("name","Name: "),
                                        textInput("email", "Email: "),
                                        textAreaInput("feedback", 'Give your feedback:', ""),
                                        actionButton('writetodb', 'SAVE'),
                                        
                                      ),
                              
                      )
                      
                    )
                  ))

server <- function(input, output) {
  output$packages <- renderDataTable(pacakages)
  mytableinshiny <- reactive({
    dbtrigger$depend()
    dbGetQuery(con, 'SELECT txt1, txt2 from student_details12')
  })
  observeEvent(input$writetodb, {
    sql1 = "INSERT INTO student_details12 (txt1, txt2) VALUES (?txt1, ?txt2)"
    sql <- sqlInterpolate(con, sql1, txt1=input$txt1, txt2=input$txt2)
    dbExecute(con, sql)
    dbtrigger$trigger()
  })
  output$dbtable <- renderTable({
    mytableinshiny()
  })
  
  output$myname1<-renderText(input$username)
  output$myreview<-renderText(input$request)
  
  
}

shinyApp(ui, server)