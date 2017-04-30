##  *Women in Science Project* ##
#==============================================================================================================================

## This code is for the women in science event. We will be using the animalTrack package and the shiny package to create fun way to teach young girls how to code. 

#==============================================================================================================================

#==============================================================================================================================
## Load libraries
library(animalTrack) # load the animalTrack Package
library(shiny) # Load the shiny package
library(shinythemes)
library(shinyjs)
#==============================================================================================================================

#==============================================================================================================================
# Seal Dive Data
#==============================================================================================================================

# Load the seal data
data(hugh)

# find out how many dives are in the data set
DiveNum = unique(hugh$diven)
DiveNum = DiveNum[DiveNum!=0]

# Fix values
hugh$speed.cal[hugh$speed.cal<0] = 0 # make any negative speed = 0
hugh$speed.cal[is.na(hugh$speed.cal)] = 0 # make anymissing speeds = 0
hugh$depth.cal = -hugh$depth.cal # Change the depth data into a positive number

# Calculate Pitch
hugh$pitch <- pitch(hugh$Ax.dec,hugh$Ay.dec,hugh$Az.dec)

# Calculate Roll
hugh$roll <- roll(hugh$Ay.dec,hugh$Az.dec)

# Define the declination
dec = 143.56*(pi/180)

# Calculate the tilt aka headig values
hugh.tilt <- tilt_compensate(hugh$MXone,hugh$MYone,hugh$MZone,hugh$pitch,hugh$roll,declination = dec,angle = "radian")

# Define the new variables
# Course Steered X & Y
hugh$CSx = 0
hugh$CSy = 0
# Course Made Good X & Y
hugh$CMGx = 0
hugh$CMGy = 0

# Compute the dead reckoning values for all dives in hugh
for(i in DiveNum){
  Nav = dead_reckoning(hugh$speed.cal[hugh$diven==i],hugh.tilt$heading_mag[hugh$diven==i],angle = 'radian',depth = hugh$depth.cal[hugh$diven==i],pitch = hugh$pitch[hugh$diven==i])
  
  # Fill in the variables in dive by dive
  hugh$CSx[hugh$diven==i] = Nav$CSx
  hugh$CSy[hugh$diven==i] = Nav$CSy
  hugh$CMGx[hugh$diven==i] = Nav$CMGx
  hugh$CMGy[hugh$diven==i] = Nav$CMGy
}


# length(nav1$CMGx)
Fish = seq(200,400, by = 100)
FishNames = c("antarcticum","setebos","mawsoni")




#==============================================================================================================================
## The ui Section. 
#==============================================================================================================================

ui <- navbarPage(theme=shinytheme("cyborg"),
    title = "'Antarctic Seals & Computers' - Tracking seals in 3D!",
    # Main Page
    tabPanel(title = "Home",
             
             column(12, wellPanel(
                  align="center",h3("Welcome to the UT Marine Science Institute's:",p("Women in Marine Science!")),
                  tags$br(),
                  tags$img(src="UTWIS.jpg"))),
             
             column(6,wellPanel(
                      h4("Come explore the world of Hugh the seal!"),
                      tags$br(),
                      p(h4("Weddell seals are found near and around Antarctica. Weddell seals spend most of their time socializing, 
                           eating, and living in the cold water underneath Antarctic Sea ice. They are also mammals like you or I so they have to
                           breath air to survive.  So Weddell seals have to stay close to holes and cracks in the ice where they can breathe. 
                           Learn more about Weddell seals",
                           a(tags$u("here."),
                             href = "http://www.seals-world.com/weddell-seal/"))),
                      tags$br(),
                      p(h4("Each tab on this webpage takes a 2D or 3D look at a snapshot of part of a day in the life of the Weddell seal Hugh.")),
                      tags$br(),
                      p(h4(tags$strong(tags$span(style="color:#00BFFF", "Depth:")),
                           "Play with the interactive 2D recreation of six of Hugh's dives. How deep did he dive?")),
                      tags$br(),
                      p(h4(tags$strong(tags$span(style="color:#00BFFF","Speed:")), 
                           " Play with the interactive 2D recreation of six of Hugh's dives. How fast did he swim?")),
                      tags$br(),
                      p(h4(tags$strong(tags$span(style="color:#00BFFF","Prey:")),
                           " Play with the interactive 3D recreation of one of Hugh's dives. What did he eat on that dive?")),
                      tags$br(),
                      # tags$br(),
                      # tags$br(),
                      # tags$br(),
                      # tags$br(),
                      # tags$br(),
                      # tags$br(),
                      # tags$br(),
                      # tags$br(),
                      # tags$br(),
                      tags$br()
               )),
               column(6, wellPanel(
                 alinment="center",tags$img(src= "Weddell.jpg",width= 650, hight= 650)
                 # p(h6("Can't forget to put the project number!"))
               ))

    ),
    
    tabPanel(title = "Weddell Seal Project",
             column(12,wellPanel(
               align="center", h3("B-017 Weddell Seal Project"),
               tags$br(),
               HTML('<iframe src="https://www.youtube.com/embed/9ldvfujNcas" width="620" height="515" frameborder="1" 
                    allowfullscreen=""></iframe>'),
               tags$br(),
               tags$br(),
               tags$br(),
               p(h4('Erin Frolli and Dr. Lee Fuiman and a team of researchers from four universities are trying to discover how Weddell seals
                    navigate under Antarctic sea ice. The scientists use state-of-art critter cams and equipment to test their idea that the
                    seals navigate underwater using an internal magnetic compass sensory system, much as homing pigeons do. Read about their
                    experiences on their last field season',
                    a(tags$u("here."),
                      href = "http://williams.eeb.ucsc.edu/field-notes/antarctica-2016-weddell-seal-navigation/"))),
               tags$br(),
               p(h4('The multi-year project is being conducted at McMurdo Station, Antarctica, and is funded by the National Science Foundation.
                    The project is a collaboration with colleagues at Texas A&M University - Galveston, University of California - Santa Cruz,
                    and the University of Auckland.')),
               tags$br(),
               tags$br(),
               p(h4('A video story about the project courtesy of the National Science Foundation is available:')),
               tags$br(),
               # tags$video(src = "SealVideo.mp4", type = "video/mp4", controls = "controls", width= "650px", hight= "650px"),
               HTML('<iframe src="https://www.youtube.com/embed/HIOVBSCCdDY" width="650" height="650" frameborder="1" 
                    allowfullscreen=""></iframe>'),
               p(tags$em("(Prior to this research, these biologists performed many tests to ensure that the equipment adds very little")),
               p(tags$em("drag to the seals and make sure that the equipment does not adversely affect a seals ability to eat or behave.)"))
               
                ))
    ),
    
    
    # Page 1
    tabPanel(title = "Depth data",
             
            fluidRow(
             useShinyjs(),  # Set up shinyjs

              column(2,wellPanel(selectInput("D.Dropdown","Choose Dive Number:",DiveNum))),
              column(6,wellPanel(align="center",plotOutput("p2Ddive",width= "600px", height="600px")))
             ),
             
            fluidRow(
              column(8, wellPanel(
               actionButton(inputId = "D.go", label = "Click for Data Summary",class="btn-info"),
               verbatimTextOutput("D.stats"),
               plotOutput("Depth")
             )))
    ),
    
    
    # Page 2
    tabPanel(title = "Speed data",
             fluidRow(
               useShinyjs(),  # Set up shinyjs
               
               column(2,wellPanel(selectInput("S.Dropdown","Choose Dive Number:",DiveNum))),
               column(6,wellPanel(align="center",plotOutput("s2Ddive",width= "600px", height="600px")))
             ),
             
             fluidRow(
               column(8, wellPanel(
                 actionButton(inputId = "S.go", label = "Click for Data Summary",class="btn-info"),
                 verbatimTextOutput("S.stats"),
                 plotOutput("Speed")
               )))
    ),
    
    # Page 3
    tabPanel(title = "Prey data",
      
        column(12, wellPanel(
          align="center", h3("Hugh the Seal's Overhead View:",p("Dive Number 1")),
          tags$br(),
          tags$br(),
          rglwidgetOutput("Prey")
      )),
      
      fluidRow(
        useShinyjs(),  # Set up shinyjs
        
        column(4,wellPanel(selectInput("p.Dropdown","Choose Prey Type:",FishNames)))
     ),
     
     column(12, wellPanel(
       # This outputs the dynamic UI component
       uiOutput("p.ui")
     ))
     
  ),
  
    windowTitle="Seals & Computers"

)


#==============================================================================================================================
## The server Section. 
#==============================================================================================================================

options(rgl.useNULL=TRUE)

library(shiny) # load the shiny library
library(rgl)
library(shinyRGL)


server <- function(input, output) {
  
  # Depth Data =========================================================================================================================
  observeEvent(input$D.Dropdown, {
    # Change the following line for more examples
    hide("D.stats")
    hide("Depth")
  })

  output$p2Ddive <- renderPlot({
    D = as.numeric(input$D.Dropdown)
    X_d = hugh$CMGx[hugh$diven==D]
    Y_d = hugh$CMGy[hugh$diven==D]
    
    d.cal = hugh$depth.cal[hugh$diven==D]
    
    Low = which(d.cal < 20)
    Mid = which(d.cal >= 20 & d.cal <= 40)
    Mid_2 = which(d.cal >= 40 & d.cal <= 60)
    High = which(d.cal > 60)
    
    plimmax = max(X_d,Y_d)
    plimmin = min(X_d,Y_d)
    
    plot(X_d,Y_d,main = paste("Hugh the Seal's Overhead View: \nDive Number",D),ylab = "Y",xlab = "X",pch=20,
         ylim = c(plimmin,plimmax),xlim = c(plimmin,plimmax))
    points(X_d[Low],Y_d[Low],col="yellow",pch=20)
    points(X_d[Mid],Y_d[Mid],col="gold",pch=20)
    points(X_d[Mid_2],Y_d[Mid_2],col="orange",pch=20)
    points(X_d[High],Y_d[High],col="Red",pch=20)
    
    })
  
  output$Depth <- renderPlot({
    plot(hugh$depth.cal[hugh$diven==as.numeric(input$D.Dropdown)], main = paste("Hugh the Seal's Depth \nDive Number:",input$D.Dropdown),
         xlab = "Time (seconds)", ylab ="Depth (meeters)",ylim = rev(range(hugh$depth.cal[hugh$diven==as.numeric(input$D.Dropdown)])))
    rect(-18, -2, length(hugh$depth.cal[hugh$diven==as.numeric(input$D.Dropdown)])+20, 20, col='yellow', border=NA)
    rect(-18,20,length(hugh$depth.cal[hugh$diven==as.numeric(input$D.Dropdown)])+20,40,col = "gold", border=NA)
    rect(-18,40,length(hugh$depth.cal[hugh$diven==as.numeric(input$D.Dropdown)])+20,60,col="orange", border=NA)
    rect(-18,60,length(hugh$depth.cal[hugh$diven==as.numeric(input$D.Dropdown)])+20,150,col="red", border=NA)
    points(hugh$depth.cal[hugh$diven==as.numeric(input$D.Dropdown)])
  })
  
  # Depth Data Sunmmery
  D.data <- eventReactive(input$D.go, { # Has to click on button to do it
    Tt = paste("Depth Data Summary for: Dive",input$D.Dropdown) # So know which dive the summery is for
    RO = summary(hugh$depth.cal[hugh$diven==as.numeric(input$D.Dropdown)]) # The stats summery
    D = c() # Create a data frame
    D$Title = Tt # First var is title
    D$Read_Out = RO # Second var is summery
    D
  })
  
  observeEvent(input$D.go, {
    # Change the following line for more examples
    show("D.stats")
    show("Depth")
  })
  
  output$D.stats <- renderPrint({
    D.data() # set D.Dtats to the data value when press the button
  })
  

  
  # Speed Data =========================================================================================================================
  observeEvent(input$S.Dropdown, {
    # Change the following line for more examples
    hide("S.stats")
    hide("Speed")
  })
  
  output$s2Ddive <- renderPlot({
    D = as.numeric(input$S.Dropdown)
    
    X_s = hugh$CMGx[hugh$diven==D]
    Y_d = hugh$CMGy[hugh$diven==D]
    
    S.cal = hugh$speed.cal[hugh$diven==D]
    
    Low = which(S.cal < 0.5)
    Mid = which(S.cal >= .5 & S.cal <= 1)
    Mid_2 = which(S.cal >= 1 & S.cal <= 1.5)
    High = which(S.cal > 1.5)
    
    plimmax = max(X_s,Y_d)
    plimmin = min(X_s,Y_d)
    
    plot(X_s,Y_d,main = paste("Hugh the Seal's Overhead View: \nDive Number",D),ylab = "Y",xlab = "X",pch=20,
         ylim = c(plimmin,plimmax),xlim = c(plimmin,plimmax))
    points(X_s[Low],Y_d[Low],col="darkorchid3",pch=20)
    points(X_s[Mid],Y_d[Mid],col="darkcyan",pch=20)
    points(X_s[Mid_2],Y_d[Mid_2],col="goldenrod3",pch=20)
    points(X_s[High],Y_d[High],col="Red",pch=20)
    
  })
  
  output$Speed <- renderPlot({
    plot(hugh$speed.cal[hugh$diven==as.numeric(input$S.Dropdown)], main = paste("Hugh the Seal's Speed \nDive Number:",input$S.Dropdown),
         xlab = "Time (seconds)", ylab ="Speed (meeters/second)" )
    rect(-18, 0, length(hugh$speed.cal[hugh$diven==as.numeric(input$S.Dropdown)])+20, .5, col='darkorchid3', border=NA)
    rect(-18,.5,length(hugh$speed.cal[hugh$diven==as.numeric(input$S.Dropdown)])+20,1,col = "darkcyan", border=NA)
    rect(-18,1,length(hugh$speed.cal[hugh$diven==as.numeric(input$S.Dropdown)])+20,1.5,col="goldenrod3", border=NA)
    rect(-18,1.5,length(hugh$speed.cal[hugh$diven==as.numeric(input$S.Dropdown)])+20,3.05,col="red", border=NA)
    points(hugh$speed.cal[hugh$diven==as.numeric(input$S.Dropdown)])
  })
  
  
  
  # Speed Data Sunmmery
  S.data <- eventReactive(input$S.go, { # Has to click on button to do it
    Tt = paste("Speed Data Summary for: Dive",input$S.Dropdown) # So know which dive the summery is for
    RO = summary(hugh$speed.cal[hugh$diven==as.numeric(input$S.Dropdown)]) # The stats summery
    S = c() # Create a data frame
    S$Title = Tt # First var is title
    S$Read_Out = RO # Second var is summery
    S
  })
  
  observeEvent(input$S.go, {
    # Change the following line for more examples
    show("S.stats")
    show("Speed")
  })
  
  output$S.stats <- renderPrint({
    S.data() # set D.Dtats to the data value when press the button
  })
  
  # Prey Data =========================================================================================================================
  
  output$Prey <- renderRglwidget({
    bg3d("grey8")
    plot3d(hugh$CMGx[hugh$diven==1],hugh$CMGy[hugh$diven==1],-hugh$depth.cal[hugh$diven==1],type="s",aspect=1,axes=FALSE,
           main="", xlab="", ylab='',zlab='',size=.5,cex=2, col="white")
    par3d(windowRect = c(50,50,700,700))
    points3d(hugh$CMGx[hugh$diven==1][Fish],hugh$CMGy[hugh$diven==1][Fish],-hugh$depth.cal[hugh$diven==1][Fish],col="blue",pch3d = 4,size=20)
    points3d(hugh$CMGx[hugh$diven==1][1],hugh$CMGy[hugh$diven==1][1],-hugh$depth.cal[hugh$diven==1][1],col="purple",size=20,pch=20)
    points3d(c(-40:40),c(-40:40),rep(0,80),size=.5,cex=2, col="white",pch3d=18)
    text3d(hugh$CMGx[hugh$diven==1][Fish],hugh$CMGy[hugh$diven==1][Fish],-hugh$depth.cal[hugh$diven==1][Fish]-10,FishNames,col="white")
    rglwidget(width=2000, height=2000)
  })
  
  
  output$p.ui <- renderUI({
    if (is.null(input$p.Dropdown))
      return()
    
    # Depending on input$p.Dropdown, we'll generate a different
    # UI component and send it to the client.
    switch(input$p.Dropdown,
           "antarcticum" = p(h4("Pleuragramma antarcticum - Antarctic silverfish"),
                             tags$br(),
                             tags$img(src="Pluragrama.png"),
                             tags$br(),
                             p(h5("Small fish that is in the haring family. They grow to about 15 cm in length.  
                                  This fish produces an antifreeze agent in its blood to prevent it from freezing in the extreme cold Antarctic
                                  waters. You can learn more about the Antarctic silverfish",
                                  a(tags$u("here."),
                                    href = "https://en.wikipedia.org/wiki/Antarctic_silverfish"))),
                             tags$br(),
                             tags$br(),
                             # tags$video(src = "Short_antarcticum.mp4", type = "video/mp4", controls = "controls", width= "950px", hight= "950px")
                             HTML('<iframe src="https://www.youtube.com/embed/kFRaWkW7AxY" width="950" height="950" frameborder="1" 
                              allowfullscreen=""></iframe>')),
           
           "setebos" = p(h4("Megaleledone setebos - giant Antarctic octopus"),
                         tags$br(),
                         tags$img(src="Megaleledone_setebos.png"),
                         tags$br(),
                         p(h5("This is a large octopus. They grow about 28 cm in mantle length and 90 cm in total length. 
                              They have special sub-zero venoms to immobilize their prey. You can learn more about the Giant Antarctic octopus",
                              a(tags$u("here."),
                                href = "https://en.wikipedia.org/wiki/Megaleledone_setebos"))),
                         tags$br(),
                         tags$br(),
                         # tags$video(src = "Short_setebos.mp4", type = "video/mp4", controls = "controls", width= "950px", hight= "950px")
                         HTML('<iframe src="https://www.youtube.com/embed/_4zG9Aksh7Y" width="950" height="950" frameborder="1" 
                              allowfullscreen=""></iframe>')),
           
           "mawsoni" = p(h4("Dissostichus mawsoni - Antarctic toothfish"),
                         tags$br(),
                         tags$img(src="Dissostichus_mawsoni.png"),
                         tags$br(),
                         p(h5("Large fish that is in the cod family. They grow to about 1.7 m (5 ft 7 in) in length and 135 kg in weight. 
                              This fish is a voracious predator eating other fishes and other organisms. It is by far the favorite choice 
                              of the Weddell Seal. You can learn more about the Antarctic toothfish",
                              a(tags$u("here."),
                                href = "https://en.wikipedia.org/wiki/Antarctic_toothfish"))),
                         tags$br(),
                         tags$br(),
                         # tags$video(src = "Short_mawsoni.mp4", type = "video/mp4", controls = "controls", width= "950px", hight= "950px")
                         HTML('<iframe src="https://www.youtube.com/embed/E6QCHU5zn9M" width="950" height="950" frameborder="1" 
                              allowfullscreen=""></iframe>'))


    )
  })
  
}

#==============================================================================================================================

#==============================================================================================================================
## The Run the app Section. 
shinyApp(server = server, ui = ui)
#==============================================================================================================================



