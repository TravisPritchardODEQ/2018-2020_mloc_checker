
library(shiny)
library(dplyr)


load("data/wide_data.Rdata")
AU_s = unique(data_wide$AU_ID)
param_groups <- c("Aquatic Life Toxics",
                  "Human Health Toxics",
                  "Bacteria",
                  "Chlorophyll",
                  "Dissolved Oxygen",
                  'pH',
                  'Temperature'
                  )

Bacteria_cols <- c("Enterococcus excursions",
                   "Escherichia coli excursions",
                   "Fecal Coliform excursions")


DO_cols <-c("Dissolved oxygen (DO)- Year round excursions",
            "Dissolved oxygen (DO)- Spawn excursions")

Temperature_cols <- c("Temperature, water- Year round excursions","Temperature, water- Spawn excursions" )

Al_cols <- c("Ammonia- Aquatic Life excursions",          
             "Copper- Aquatic Life excursions"                 , "Cadmium- Aquatic Life excursions"                           ,
             "Lead- Aquatic Life excursions"                  ,  "Nickel- Aquatic Life excursions"                            ,
             "Silver- Aquatic Life excursions"                ,  "Zinc- Aquatic Life excursions"                              ,
             "Arsenic, Inorganic- Aquatic Life excursions"    ,  "Aldrin- Aquatic Life excursions"                            ,
             "BHC Gamma (Lindane)- Aquatic Life excursions"   ,  "Chlordane- Aquatic Life excursions"                         ,
             "Chlorpyrifos- Aquatic Life excursions"          ,  "DDD 4,4'- Aquatic Life excursions"                          ,
             "DDE 4,4'- Aquatic Life excursions"              ,  "DDT 4,4'- Aquatic Life excursions"                          ,
             "Dieldrin- Aquatic Life excursions"              ,  "Endosulfan Alpha- Aquatic Life excursions"                  ,
             "Endrin- Aquatic Life excursions"                ,  "Guthion- Aquatic Life excursions"                           ,
             "Heptachlor- Aquatic Life excursions"            ,  "Heptachlor Epoxide- Aquatic Life excursions"                ,
             "Malathion- Aquatic Life excursions"             ,  "Methoxychlor- Aquatic Life excursions"                      ,
             "Mirex- Aquatic Life excursions"                 ,  "Polychlorinated Biphenyls (PCBs)- Aquatic Life excursions"  ,
             "Endosulfan Beta- Aquatic Life excursions"       ,  "Alkalinity- Aquatic Life excursions"                        ,
             "Chloride- Aquatic Life excursions"              ,  "Iron (total)- Aquatic Life excursions"                      ,
             "Parathion- Aquatic Life excursions"             ,  "Mercury (total)- Aquatic Life excursions"                   ,
             "Chlorine- Aquatic Life excursions"              ,  "Cyanide- Aquatic Life excursions"                           ,
             "Toxaphene- Aquatic Life excursions" ) 

HH_cols <- c("Antimony- Human Health excursions"                         ,  "Selenium- Human Health excursions"                          ,
             "Thallium- Human Health excursions"                         ,  "Acenaphthene- Human Health excursions"                      ,
             "Anthracene- Human Health excursions"                       ,  "Benz(a)anthracene- Human Health excursions"                 ,
             "Benzo(a)pyrene- Human Health excursions"                   ,  "Benzo(b)fluoranthene 3,4- Human Health excursions"          ,
             "Benzo(k)fluoranthene- Human Health excursions"             ,  "BHC Alpha- Human Health excursions"                         ,
             "BHC Beta- Human Health excursions"                         ,  "Butylbenzyl Phthalate- Human Health excursions"             ,
             "Chloroethyl Ether bis 2- Human Health excursions"          ,  "Chloronaphthalene 2- Human Health excursions"               ,
             "Chlorophenol 2- Human Health excursions"                   ,  "Chrysene- Human Health excursions"                          ,
             "Dibenz(a,h)anthracene- Human Health excursions"            ,  "Dichlorobenzene(o) 1,2- Human Health excursions"            ,
             "Dichlorobenzene(p) 1,4- Human Health excursions"           ,  "Dichlorophenol 2,4- Human Health excursions"                ,
             "Diethyl Phthalate- Human Health excursions"                ,  "Dimethyl Phthalate- Human Health excursions"                ,
             "Dimethylphenol 2,4- Human Health excursions"               ,  "Dinitrophenol 2,4- Human Health excursions"                 ,
             "Dinitrotoluene 2,4- Human Health excursions"               ,  "Dioxin (2,3,7,8-TCDD)- Human Health excursions"             ,
             "Endosulfan Beta- Human Health excursions"                  ,  "Endosulfan Sulfate- Human Health excursions"                ,
             "Endrin Aldehyde- Human Health excursions"                  ,  "Fluoranthene- Human Health excursions"                      ,
             "Fluorene- Human Health excursions"                         ,  "Hexachlorobenzene- Human Health excursions"                 ,
             "Hexachlorobutadiene- Human Health excursions"              ,  "Hexachlorocyclopentadiene- Human Health excursions"         ,
             "Hexachloroethane- Human Health excursions"                 ,  "Isophorone- Human Health excursions"                        ,
             "Manganese- Human Health excursions"                        ,  "Methyl-4,6-dinitrophenol- Human Health excursions"          ,
             "Nitrobenzene- Human Health excursions"                     ,  "Nitrosodibutylamine, N- Human Health excursions"            ,
             "Nitrosodiethylamine, N- Human Health excursions"           ,  "Nitrosodimethylamine, N- Human Health excursions"           ,
             "Nitrosodi-n-propylamine, N- Human Health excursions"       ,  "Nitrosopyrrolidine, N- Human Health excursions"             ,
             "Pentachlorobenzene- Human Health excursions"               ,  "Pentachlorophenol- Human Health excursions"                 ,
             "Phenol- Human Health excursions"                           ,  "Pyrene- Human Health excursions"                            ,
             "Tetrachlorobenzene, 1,2,4,5-- Human Health excursions"     ,  "Trichlorobenzene 1,2,4- Human Health excursions"            ,
             "Trichlorophenol 2,4,6- Human Health excursions"            ,  "Trichlorophenol, 2, 4, 5-- Human Health excursions"         ,
             "DDD 4,4'- Human Health excursions"                         ,  "DDT 4,4'- Human Health excursions"                          ,
             "Endosulfan Alpha- Human Health excursions"                ,   "DDE 4,4'- Human Health excursions"                          ,
             "Barium- Human Health excursions"                          ,   "Chlorophenoxy Herbicide (2,4,5,TP)- Human Health excursions",
             "Chlorophenoxy Herbicide (2,4-D)- Human Health excursions" ,   "Benzene- Human Health excursions"                           ,
             "Bromoform- Human Health excursions"                       ,   "Carbon Tetrachloride- Human Health excursions"              ,
             "Chlorobenzene- Human Health excursions"                   ,   "Chlorodibromomethane- Human Health excursions"              ,
             "Chloroform- Human Health excursions"                      ,   "Dichloroethane 1,2- Human Health excursions"                ,
             "Dichloroethylene 1,1- Human Health excursions"            ,   "Dichloroethylene trans 1,2- Human Health excursions"        ,
             "Dichloropropane 1,2- Human Health excursions"             ,   "Dichloropropene 1,3- Human Health excursions"               ,
             "Ethylbenzene- Human Health excursions"                    ,   "Methyl Bromide- Human Health excursions"                    ,
             "Methylene Chloride- Human Health excursions"              ,   "Tetrachloroethane 1,1,2,2- Human Health excursions"         ,
             "Tetrachloroethylene- Human Health excursions"             ,   "Toluene- Human Health excursions"                           ,
             "Trichloroethane 1,1,2- Human Health excursions"           ,   "Trichloroethylene- Human Health excursions"                 ,
             "Vinyl Chloride- Human Health excursions"                  ,   "Nitrates- Human Health excursions"                          ,
             "Ethylhexyl Phthalate bis 2- Human Health excursions"      ,   "Benzidine- Human Health excursions"                         ,
             "Dichlorobenzidine 3,3'- Human Health excursions"          ,   "Acrolein- Human Health excursions"                          ,
             "Acrylonitrile- Human Health excursions"                   ,   "Dichlorobenzene(m) 1,3- Human Health excursions"            ,
             "Dichlorobromomethane- Human Health excursions"            ,   "Di-n-butyl Phthalate- Human Health excursions"              ,
             "Indeno(1,2,3-cd)pyrene- Human Health excursions"          ,   "Nitrosodiphenylamine, N- Human Health excursions"           ,
             "Nickel- Human Health excursions"                          ,   "Zinc- Human Health excursions"                              ,
             "Methylmercury- Human Health excursions" )

# populate values in selectize boxes
#get a vector of unique AUs


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(
        fluidRow(
            column(6, img(src = "logo.png")), 
            column(6,  "2018/2020 IR Monitoring Location Checker - DRAFT: NOT COMPLETE",style = "font-family: 'Arial'; font-si16pt; vertical-align: 'bottom'")),
        windowTitle = "IR Monitoring Location Checker"
    ),


    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("go", "Filter",  icon("filter")),
            selectizeInput("AUs",
                           "Select Assessment Unit",
                           choices = AU_s,
                           multiple = TRUE,
                           options = list(maxOptions = 7000)),
            selectizeInput("param_groups",
                           "Select Parameter Group",
                           choices = param_groups,
                           multiple = TRUE,
                           options = list(maxOptions = 7000))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tableOutput('table')
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    
    table_Data <- eventReactive(input$go,{
        
        t <- data_wide %>%
            filter(AU_ID %in% input$AUs)
        
        
        if(!is.null(input$param_groups)){
        select_vectors <- c("AU_ID", "AU_Type", "MLocID")


        if("Bacteria" %in% input$param_groups) {
            select_vectors <- c(select_vectors, Bacteria_cols)

        }

        if("Temperature" %in% input$param_groups){
            select_vectors <- c(select_vectors, Temperature_cols)
        }

        if("Aquatic Life Toxics" %in% input$param_groups){
            select_vectors <- c(select_vectors, Al_cols)
        }

        if("Human Health Toxics" %in% input$param_groups){
            select_vectors <- c(select_vectors, HH_cols)
        }


        if("Chlorophyll" %in% input$param_groups){
            select_vectors <- c(select_vectors, "Chlorophyll a excursions")
        }

        if("Dissolved Oxygen" %in% input$param_groups){
            select_vectors <- c(select_vectors, DO_cols)
        }
        if("pH" %in% input$param_groups){
            select_vectors <- c(select_vectors, "pH excursions")
        }


        t <- t %>%
            select(select_vectors)



             }
        
        return(t)
    })
    
    
    output$table <- renderTable(table_Data(),
                                bordered = TRUE,  
                                align = 'c',  
                                na = 'Unassessed'
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
