require("shiny")
options(shiny.deprecation.messages=FALSE) #to disable deprecated message

d <- read.csv("mockup.csv",stringsAsFactors=T)
min.molew <- min(d$Molecular.Weight)	# getting the minimum molecular weight 
max.molew <- max(d$Molecular.Weight)	# getting the maximum molecular weight
ppl.classes <- levels(d$Polyphenol.Class)	# getting all the classes
show.pph.classes <- as.list(0:length(ppl.classes))	# formatting the to retrieve the value input later
names(show.pph.classes) <- c("Select All",ppl.classes)	# adding also the "Select All"

polyphenol.sub.classes <- levels(d$Polyphenol.Sub.Class) 	# getting all the Sub classes

show.pph.sub.classes <- as.list(0:length(polyphenol.sub.classes))	# formatting the to retrieve the value of subclasses input later
names(show.pph.sub.classes) <- c("Select All",polyphenol.sub.classes)	# adding also the "Select All"
temptab <- table(d$Polyphenol.Sub.Class,d$Polyphenol.Class)		# frequency of bivariate class and sub classes


# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Polyphenol Status"),

  # Sidebar with the required inputs
  sidebarLayout(
    sidebarPanel(
    # Amount Input
      sliderInput("amount",
                  "Amount",
                  min = 0,
                  max = max(temptab),
                  value = max(temptab)),
    # Molecular weights Input
      sliderInput("molew",
                  "Molecular weight",
                  min = min.molew,
                  max = max.molew,
                  value = c(min.molew,max.molew)),
    # classes input
      checkboxGroupInput("polyphenol.class", 
        label = h3("Polyphenol Class"), 
        choices = show.pph.classes,
        selected = show.pph.classes),
    # Subclasses input
      checkboxGroupInput("polyphenol.sub.class", 
        label = h3("Polyphenol Sub-Class"), 
        choices = show.pph.sub.classes,
        selected = show.pph.sub.classes)

    ),
      
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),		# Main histogram
      h3("Data Table",style="margin-top: 50px;"), # Table title
      p(pre(includeText("include.txt"))),	# custom message after table title
      dataTableOutput("datatable")	# output of data
    )
  )
))
