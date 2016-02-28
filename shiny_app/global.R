library(dplyr)
library(ggmap)
library(timeDate)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(data.table)
centre <- function(x, type) {
  switch(type,
                   mean = {x=x+12;mean(x)},
                   median = median(x),
                  trimmed = mean(x, trim = .1))
}

do_anova <- function (x) {
  r <- aov(scaled_score ~ group, data=x)
  summary(r)
  p_vals <- summary(r)[[1]][["Pr(>F)"]]
  p_val <- p_vals[1]
  p_val
}
do_chisqr <- function (x) {
  contingency.table <- table(x$group,x$grade)
  chisq.test(contingency.table)$p.value
}

prod_scores = read.csv('product_grades.csv')
prod_scores$grade <- ordered(prod_scores$grade, levels=c("love","want","have","regret"))
#prod_scores$taxonomy <- as.character(prod_scores$taxonomy)
#prod_scores[prod_scores$taxonomy=="\\N",]$taxonomy <- " "
prod.scores.df <- prod_scores
num.reviews <- nrow(prod.scores.df)
#build the list of product names to display in the dropdown -- products with at least 50 reviews
prod.list <- summarise(group_by(prod_scores,product_name),total_reviews=n())
prod.list <- prod.list[with(prod.list, order(-total_reviews)), ]
prod.list <- filter(prod.list,total_reviews >= 50)


#build the list of brands to display in the dropdown -- brands with at least 50 reviews
brand.list <- summarise(group_by(prod_scores,brand),total_reviews=n())
brand.list <- brand.list[with( brand.list, order(-total_reviews)), ]
brand.list <- filter( brand.list,total_reviews >= 50)
