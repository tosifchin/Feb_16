shinyServer(function(input,output) {
  
  # Tab 1
  output$prod_names <- renderUI({
    #prod.scores.df <- prod_scores %>% filter(taxonomy == input$tax)
    prod.list <- prod_scores %>% 
                filter(taxonomy == input$tax) %>% group_by(product_name) %>%
                summarise(total_reviews=n())
    prod.list <- prod.list[with(prod.list, order(-total_reviews)), ]
    prod.list <- filter(prod.list,total_reviews >= 50)
    l <- unique(as.character(prod.list$product_name))
    if (!("" %in% l)) {
      l[length(l)+1] <- ""
    }
    selectizeInput("prod_name", h4(strong("Select a Product:")),
                choices = as.character(prod.list$product_name),
                selected = "")})
  
  output$brands <- renderUI({
    #prod.scores.df <- prod_scores %>% filter(taxonomy == input$tax)
    brand.list <- prod_scores %>% 
                  group_by(brand) %>%
                  summarise(total_reviews=n())
    brand.list <- brand.list[with(brand.list, order(-total_reviews)), ]
    brand.list <- filter(brand.list,total_reviews >= 50)
    l <- unique(as.character(brand.list$brand))
    if (!("" %in% l)) {
      l[length(l)+1] <- ""
    }
    selectizeInput("brand", h4(strong("Select a Brand:")),
              choices = l,
              selected = "")
    })
  
  output$categories <- renderUI({
    #prod.scores.df <- prod_scores %>% filter(taxonomy == input$tax, brand == input$brand)
    prod.scores.df <- prod_scores %>% filter(brand == input$brand)
    cat.list <- prod_scores %>% 
                filter(brand == input$brand) %>%
                group_by(category) %>%
                summarise(total_reviews=n())
    cat.list <- cat.list[with(cat.list, order(-total_reviews)), ]
    cat.list <- filter(cat.list,total_reviews >= 50)
    l <- unique(as.character(cat.list$category))
    if (!("" %in% l)) {
      l[length(l)+1] <- ""
    }
    selectizeInput("category", h4(strong("Select a Category:")),
              choices = l,
              selected = "")})

     grp_prod_scores <- reactive({
    
        #execute this every time the graph.type radio button is changed, as well as when 
        #the select inputs are changed
        input$graph.type
        selected_tax <- input$tax
        #prod.scores.df <- prod_scores %>% filter(taxonomy == selected_tax)
        contingency.table <- table(prod.scores.df$group,prod.scores.df$grade)
        
        num.of.reviews <- "NA"
        pvalue <- "NA"
        selected_prod <- input$prod_name
        if (input$graph_type == "Product Name") {
          if (!is.null(selected_prod) && (selected_prod != "")) {
             prod.scores.df <- prod.scores.df %>% filter(product_name == selected_prod)
             num.of.reviews <- nrow(prod.scores.df)
             contingency.table <- table(prod.scores.df$group,prod.scores.df$grade)
             #If low cell values, use Fisher's Exact Test
             if (sum(contingency.table<=5)>0) {
               pvalue <- fisher.test(contingency.table, simulate.p.value=TRUE, B=1e2)$p.value
             } else if (sum(contingency.table<=5)==0) {
             pvalue <- chisq.test(contingency.table)$p.value
             }
          }
        }
        
        selected_brand <- input$brand
        selected_cat <- input$category
        if (input$graph_type == "Brand and Category") {
           if (selected_brand != "") {
              prod.scores.df <- prod.scores.df %>% filter(brand == selected_brand)
              num.of.reviews <- nrow(prod.scores.df)
              contingency.table <- table(prod.scores.df$group,prod.scores.df$grade)
              #If low cell values, use Fisher's Exact Test
              if (sum(contingency.table<=5)>0) {
                pvalue <- fisher.test(contingency.table, simulate.p.value=TRUE, B=1e2)$p.value
              } else if (sum(contingency.table<=5)==0) {
                pvalue <- chisq.test(contingency.table)$p.value
              }
              if (!is.null(selected_cat) && (selected_cat != "")) {
                 prod.scores.df <- prod.scores.df %>% filter(category == selected_cat)
                 num.of.reviews <- nrow(prod.scores.df)
                 contingency.table <- table(prod.scores.df$group,prod.scores.df$grade)
                 #If low cell values, use Fisher's Exact Test
                 if (sum(contingency.table<=5)>0) {
                   pvalue <- fisher.test(contingency.table, simulate.p.value=TRUE, B=1e2)$p.value
                 } else  {
                   pvalue <- chisq.test(contingency.table)$p.value
                 }
                 
              }
                                                        
           }
        }
        df <- as.data.frame(contingency.table)
        totals <- summarise(group_by(df,Var1),total_reviews=sum(Freq))
#         tot <- function (x) {
#           as.character(totals[totals$Var1==df$Var1,]$Var1)
#         }
        #create the df that you can graph:
        df$tot_per_group <- sapply(df$Var1, function(x) {as.character(totals[totals$Var1==x,]$total_reviews)})
        df[,"percentage"] <- apply(df[,c(3,4)], 1, function(x){as.integer(x[1])/as.integer(x[2])})
        
        #return terms in an object that can be referenced
        #by other reactive functions:
        all_results <- list(prod_scores=as.data.frame(df),
                          num.reviews=num.of.reviews,
                          p.value=pvalue)
        return (all_results)
  })
  
  output$intro <- renderPlot({
     
  #don't graph anything until they select a product id or a brand, category combination
  if (input$graph_type == "Product Name") {
     if (!is.null(input$prod_name) && input$prod_name != "") {
           ggplot(data=grp_prod_scores()$prod_scores, aes(factor(Var1), percentage, fill = Var2)) + 
           geom_bar(stat = 'identity', position = 'dodge') +
           theme_bw() + ylab('% Reviews per User Group') + xlab('User Group') + 
           scale_fill_hue(name = "Rating", c=45, l=80) +
           ggtitle(input$prod_name) +
           theme(axis.title = element_text(size = 24), axis.text = element_text(size=18), 
                 #legend.position="none",
                 plot.title = element_text(size=24, face="bold"))
       
     }
  } else {
    if (input$brand != "") {
      ggplot(data=grp_prod_scores()$prod_scores, aes(factor(Var1), percentage, fill = Var2)) + 
        geom_bar(stat = 'identity', position = 'dodge') +
        theme_bw() + ylab('% Reviews per User Group') + xlab('User Group') +
        scale_fill_hue(name = "Rating", c=45, l=80) +
        ggtitle(paste(input$brand,input$category)) +
        theme(axis.title = element_text(size = 24), axis.text = element_text(size=18), 
              #legend.position="none",
              plot.title = element_text(size=24, face="bold"))
      
    }
  }
     
  }, #height = 600, 
  width = 700)
  
  
  output$n_reviews <- renderText({
    paste("Number of reviews: ", grp_prod_scores()$num.reviews,"\n")
    })  
  output$p_value <- renderText({
    paste("P-value: ", grp_prod_scores()$p.value)
  })   
})


  

  

  

