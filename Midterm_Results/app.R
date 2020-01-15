rsconnect::setAccountInfo(name='lizarova777', token='A82632B84B58B29E75A9305C573AA3B9', secret='NjOyxji+TGcavxIcxuCwHe8HJwO7mk0wq3/2SoMx')

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)


# Retrieve datasets
q <- read.csv("quiz-categories.csv", header=TRUE)
m <- read.csv("midterm-results.csv", header=TRUE)


# Aggregate Data
catgeories <- colnames(q[,2:12])
m$Number_Correct <- rowSums(m[,grepl("_c", names(m))])
m$Percent_Correct <- ((m$Number_Correct)/30)*100
m$Percent_Correct <- round(m$Percent_Correct, 0)
m$Total_Time = rowSums(m[, grepl("_time", names(m))])
m$Total_ClickCount = rowSums(m[, grepl("_Click.Count", names(m))])

# Skills for each student

s <- m[,1:32] 
s <- s[, -2]
s <- s %>% group_by(id) %>% gather(Question, Score, 2:31 )
s <- full_join(s, q, by = "Question" )
s1 <- filter(s, Score != 0)



q2 <- q %>% select(-c(googleable, non.googleable)) %>% summarise_at(c("wrangling", "coding", "d.trees", "sna", "nlp", "viz", "n.nets", "jitl", "substantive"), sum)
s2 <- s1 %>% group_by(id) %>% summarise_at(c("wrangling", "coding", "d.trees", "sna", "nlp", "viz", "n.nets", "jitl", "substantive"), sum)
s3 <- s2 %>% mutate(pct_wrangling = wrangling/q2$wrangling, pct_coding = coding/q2$coding, pct_d.trees=d.trees/q2$d.trees, pct_sna = sna/q2$sna, pct_nlp = nlp/q2$nlp, pct_viz = viz/q2$viz, pct_n.nets = n.nets/q2$n.nets, pct_jitl = jitl/q2$jitl, pct_substantive = substantive/q2$substantive)
s4 <- s3[,c(1, 11:19)]
s5 <- s4 %>% gather(Skill, Percent, 2:10 )
s6 <- s5 %>% group_by(Skill) %>% spread(id, Percent)




# Visualization

ui <- navbarPage("Midterm Results Dashboard",
                 tabPanel("Skill Mastery",
                          fluidPage(
                            titlePanel("Skill Mastery"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "id", label = "Student ID:", choices= s5$id, hr())
                              ),
                              mainPanel(plotOutput("skillhist"))
                              
                            )
                          )
                 ),
                 tabPanel("Time Spent vs. Grades vs. Click Count",
                          fluidPage(
                            titlePanel("Time Spent vs. Grades vs. Click Count"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "x", label = "X-axis:", choices = c("Percent_Correct", "Total_Time", "Total_ClickCount"), selected = "Total_Time"),
                                selectInput(inputId = "y", label = "Y-axis:", choices = c("Percent_Correct", "Total_Time", "Total_ClickCount"), selected = "Total_ClickCount"),
                                selectInput(inputId = "col", label = "Color by:", choices = c("Percent_Correct", "Total_Time", "Total_ClickCount"), selected = "Percent_Correct")
                                
                              ),
                              mainPanel(
                                textOutput("selected_y"),
                                textOutput("selected_x"),
                                textOutput("selected_color"),
                                plotOutput("timegradesscatter"))))         
                 )
                 
)


server <- function(input, output) {
  output$skillhist <- renderPlot({
    data <- reactive(s5[s5$id%in% input$id, ])
    ggplot(data(), aes(Skill,Percent)) + geom_col(fill="blue") + labs(title = "Skill Mastery", y = "Portion Correct") + scale_x_discrete(labels = c("Coding", "D.Trees", "JITL", "Neural Networks", "NLP", "SNA", "Substantive", "Viz", "Wrangling" ))
  });
  output$timegradesscatter <- renderPlot({
    data <- reactive({
      switch(input$y,
             Percent_Correct = m$Percent_Correct,
             Total_Time = m$Total_Time,
             Click_Count = m$Total_ClickCount )
    }
    
    )
    
    ggplot(data = m, mapping= aes_string(input$x, input$y, col= input$col)) + geom_point(size=10) + labs(title = "Time Spent, Grade, and Click Count") + theme_bw()
    
  })
}

shinyApp(ui = ui, server = server)