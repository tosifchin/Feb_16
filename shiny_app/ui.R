shinyUI(navbarPage("User Ratings",
                   tabPanel("",
                            fluidPage(theme = shinytheme("cosmo"),
                                      sidebarLayout(
                                        sidebarPanel(h2("Is there a difference among user groups?", align = 'center'),
                                                     #br(),
                                                     radioButtons("graph_type",h4(strong("Graph a single product or a brand and category?")),
                                                                  choices = list("Product Name", "Brand and Category")),
                                                     conditionalPanel(condition = 'input.graph_type == "Product Name"',
                                                                selectInput("tax", h4(strong("Narrow the list of products to choose from by weeSpring grouping:")),
                                                                 choices = unique(as.character(prod.scores.df$taxonomy)))),
                                                     conditionalPanel(condition = 'input.graph_type == "Product Name" ',
                                                                     uiOutput("prod_names")),
                                                     conditionalPanel(condition = 'input.graph_type == "Brand and Category"',
                                                                     uiOutput("brands")),
                                                     conditionalPanel(condition = 'input.graph_type == "Brand and Category" &&
                                                                       input.brand != ""',
                                                                      uiOutput("categories"))
                                                     ),
                                        mainPanel(
                                          column(12,
                                            plotOutput("intro",width="100%"),
                                            br(),
                                            br(),
                                            wellPanel(span(textOutput("n_reviews"),textOutput("p_value")))
                                          ),
                                          br(),

                                          br(),
                                          br(),
                                          br() #,

                                        )))
                   
                            
)
) #tab panel
)