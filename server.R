require("shiny")	# Load the required packages
options(shiny.deprecation.messages=FALSE) #to disable deprecated message
d <- read.csv("mockup.csv",stringsAsFactors=T)	# read the data file 
ppl.classes <- levels(d$Polyphenol.Class)	# listing all classes
polyphenol.sub.classes <- levels(d$Polyphenol.Sub.Class)	# listing all subclasses
d$Polyphenol.Class <- as.character(d$Polyphenol.Class)	# change factor class to character
d$Polyphenol.Sub.Class <- as.character(d$Polyphenol.Sub.Class) # change factor class to character

# Define server logic required to Bar plot
shinyServer(function(input, output, session) {

  # Expression that generates a barplot. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

  #######################################
  # all of the above code it will execute only once at server loading.
  #######################################
  observe({
    x <- as.integer(input$polyphenol.class)
    if( 0 %in% x ){
      updateCheckboxGroupInput( session,"polyphenol.class",selected = as.character(1:length(ppl.classes)) )
    }
  })
  observe({
    y <- as.integer(input$polyphenol.sub.class)
    if( 0 %in% y ){
      updateCheckboxGroupInput(session,"polyphenol.sub.class",selected = as.character(1:length(polyphenol.sub.classes)) )
    }

  })

  
  output$distPlot <- renderPlot({
  	###############			Getting input from ui 	###############
  	ph.cls.ind <- as.integer(input$polyphenol.class)	# Find the indexes which classes are being selected in ui
  	ph.cls.ind <- ph.cls.ind[ph.cls.ind>0]	# getting the indexes except "select all"
  	valid.cls <- ppl.classes[ph.cls.ind]	# Get the class name for the corresponding indexes

  	ph.sub.cls.ind <- as.integer(input$polyphenol.sub.class) # Find the indexes which sub classes are being selected in ui
  	ph.sub.cls.ind <- ph.sub.cls.ind[ph.sub.cls.ind>0]	# getting the indexes except "select all"
  	valid.sub.cls <- polyphenol.sub.classes[ph.sub.cls.ind]	# Get the sub class name for the corresponding indexes

    wtl <- input$molew[1] # input of molecular weight from ui(lower value)
    wtu <- input$molew[2] # input of molecular weight from ui(upper value)
  	amnt <- input$amount	# input of Amount from ui

  	###############		End of Getting input from ui 	###############

  	tempd <- d[( (d$Polyphenol.Sub.Class %in% valid.sub.cls) & (d$Polyphenol.Class %in% valid.cls) & (d$Molecular.Weight >= wtl & d$Molecular.Weight <= wtu) ), ]
  	#sub set of the data set for the corresponding input
  	temptab <- table(tempd$Polyphenol.Sub.Class,tempd$Polyphenol.Class)		# frequency table of class and subclasses
  	tempm <- as.matrix ( temptab )	# frequency table of class and subclasses as matrix form
    tempt <- apply(tempm,2,function(x)which(x>0)) #Listing only the existing class and subclasses
    #ntotal <- length(unlist(tempt))
    
    obs <- c()	#initialization
    cls <- c()	#initialization
    sub.cls <-c()	#initialization

    # reformat the listing object to data.frame object
    for (i in 1:length(tempt)) {
    	obj <- tempt[[i]]
    	obs <- c(obs,obj)
    	cls.names <- rep( names(tempt)[i],length(obj))
	    cls <- c( cls, cls.names )
	    sub.cls <-c( sub.cls, names(obj) )
    }

    data.table <- data.frame( count=obs, class=cls, sub.class=sub.cls )
    # reformat the listing object to data.frame object is done
    data.table <- data.table[data.table$count<= amnt,]	# subsetting the dataset using input
    barplot(data.table$count,names.arg=data.table$sub.class,las=2,col="skyblue")	# ploting the required data set for the inputed value

  })

  output$datatable<- renderDataTable({
  	###############			Getting input from ui 	###############
  	ph.cls.ind <- as.integer(input$polyphenol.class)	# Find the indexes which classes are being selected in ui
  	ph.cls.ind <- ph.cls.ind[ph.cls.ind>0]	# getting the indexes except "select all"
  	valid.cls <- ppl.classes[ph.cls.ind]	# Get the class name for the corresponding indexes

  	ph.sub.cls.ind <- as.integer(input$polyphenol.sub.class) # Find the indexes which sub classes are being selected in ui
  	ph.sub.cls.ind <- ph.sub.cls.ind[ph.sub.cls.ind>0]	# getting the indexes except "select all"
  	valid.sub.cls <- polyphenol.sub.classes[ph.sub.cls.ind]	# Get the sub class name for the corresponding indexes

  	wtl <- input$molew[1] # input of molecular weight from ui(lower value)
    wtu <- input$molew[2] # input of molecular weight from ui(upper value)
  	amnt <- input$amount	# input of Amount from ui

  	###############		End of Getting input from ui 	###############

  	tempd <- d[( (d$Polyphenol.Sub.Class %in% valid.sub.cls) & (d$Polyphenol.Class %in% valid.cls) & (d$Molecular.Weight >= wtl & d$Molecular.Weight <= wtu) ), ]
  	tempd	# to display the data
  })
})