library(shiny)
library(DT)
library(data.table)
library(arules)
library(arulesViz)
library(visNetwork)
library(shinyWidgets)

#dat <- read.csv('~/York/CSDA1040/Lab1/Group4Lab1/boardgame_data_with_expanded.csv')
dat <- read.csv('boardgame_data_with_expanded.csv')

########################################################################################################
# Attempt 7 - use all attributes only, associate with average category (good >7, fair 5-7, poor <5), customize parameters
########################################################################################################
########################################################################################################
# in order to use apriori, change the attributes to factors
dat_factors <- NULL
dat_factors$stats.average = dat$stats.average

# assign categories instead of values to stats.average
dat_factors$stats.average[dat_factors$stats.average < 5] <- "Poor"
dat_factors$stats.average[dat_factors$stats.average < 7] <- "Fair"
dat_factors$stats.average[dat_factors$stats.average < 9.999] <- "Good"



# add the category and mechanic attribute info
dat_factors <- cbind(dat_factors,  dat[,grep("attributes.", colnames(dat))])
dat_factors$stats.average <- as.factor(dat_factors$stats.average)
dat_factors[sapply(dat_factors, is.integer)] <- lapply(dat_factors[sapply(dat_factors, is.integer)], as.factor)
names(dat_factors) <- gsub("attributes.boardgame", "", names(dat_factors))
names(dat_factors) <- gsub("mechanic", "m", names(dat_factors))
names(dat_factors) <- gsub("category", "c", names(dat_factors))


# determine the most likely rules for Good games
rules<-apriori((dat_factors),parameter=list(minlen=4,support=0.012,confidence=0.75,target="rules",maxtime=15,maxlen=10), 
               appearance = list(rhs=c("stats.average=Good", "stats.average=Poor")))

rules_sorted <- sort(rules, by="lift")

slim_rules_sorted <- rules_sorted[1:50, ]



ui<- basicPage(
    setBackgroundColor(
        color = c("#F7FBFF", "#2171B5"),
        gradient = "linear",
        direction = "bottom"
    ),
    h3("CSDA1040 - Group 4"),
    h5("Aaron Fehir, Shawn Mills, Steven Too Heng Kwee"),
    mainPanel(
        visNetworkOutput("graph", width = "800px", height = "800px")
    )
)

server <- function(input, output) {
    output$graph = renderVisNetwork(
        plot(slim_rules_sorted, method = "graph", engine = "htmlwidget")
    ) 
        
    
}
shinyApp(ui=ui,server=server)