library(shiny)
library(dplyr)
library(maps)
library(ggplot2)
library(ROCR)
library(popbio)
library(mapproj)
library(rsconnect)
library(visreg)
library(magrittr)

dataset <- read.csv("data/restaurant_customer_satisfaction.csv") %>%
  mutate(ServiceRatingBinary = ifelse(ServiceRating >= 5, 1, 0),
         FoodRatingBinary = ifelse(FoodRating >= 5, 1, 0),
         AmbianceRatingBinary = ifelse(AmbianceRating >= 5, 1, 0))

ui <- fluidPage(
  titlePanel("Restaurant Customer Satisfaction"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore various plots related to customer satisfaction:"),
      
      sliderInput("ageRange", 
                  label = "Select Age Range:",
                  min = min(dataset$Age, na.rm = TRUE),
                  max = max(dataset$Age, na.rm = TRUE),
                  value = c(min(dataset$Age, na.rm = TRUE), max(dataset$Age, na.rm = TRUE))),
      
      selectInput("plotType", 
                  label = "Choose an Algorithmic Analysis Plot:",
                  choices = c("Exploratory Plot", 
                              "Estimated Category Probability", 
                              "Conditional Density Plot", 
                              "Logistic Regression Results - Service Rating",
                              "Logistic Regression Results - Food Rating",
                              "Logistic Regression Results - Ambiance Rating",
                              "Logistic Regression Results - Overall Satisfaction",
                              "Another Logistic Regression Results - Overall Satisfaction",
                              "Model Evaluation (ROC Curve)",
                              "Random Model (ROC Curve)"),
                  selected = "Exploratory Plot"),
      
      selectInput("plotType2", 
                  label = "Choose an Exploratory Variable Plot:",
                  choices = c("Frequency of High Satisfaction by Age", 
                              "Mean Satisfaction vs Age Group",
                              "Mean Satisfaction vs Service Rating",
                              "Mean Satisfaction vs Food Rating",
                              "Mean Satisfaction vs Ambiance Rating"),
                  selected = "Frequency of High Satisfaction by Age"),
      
      selectInput("textOption", 
                  label = "Choose Text Output:",
                  choices = c("Problem Statement", 
                              "Exploration", 
                              "Algorithm Explanation"),
                  selected = "Problem Statement"),
      
      textOutput("textOutput")
    ),
    
    mainPanel(
      plotOutput("plotOutput1"),
      plotOutput("plotOutput2"),
      verbatimTextOutput("modelOutput")
    )
  )
)

server <- function(input, output) {
  filteredData <- reactive({
    dataset %>%
      filter(Age >= input$ageRange[1] & Age <= input$ageRange[2])
  })
  output$plotOutput1 <- renderPlot({
    generate_plot(filteredData(), input$plotType)
  })
  output$plotOutput2 <- renderPlot({
    generate_plot2(filteredData(), input$plotType2)
  })
  output$textOutput <- renderText({
    if (input$textOption == "Problem Statement") {
      return("My problem statement is how can restaurants improve overall customer satisfaction by identifying key drivers? My initial hypothesis that I had is that customer satisfaction is influenced primarily by service quality, food quality, and ambiance. I also thought that age also plays a role. This is because I believed that different demographics have varying expectations. 
I used a random sample of restaurant satisfaction data to build logistic regression models. I did this in order to predict satisfaction levels and identify factors with the highest impact. The reason that I chose logistic regression is because it is effective for binary classification problems like predicting 'High Satisfaction' (1) vs. 'Low Satisfaction' (0). Logistic regression calculates the log odds of an event, satisfaction, as a linear combination of predictor variables. A sigmoid function maps these odds to probabilities. Key metrics like the Area Under the Curve (AUC) will evaluate model performance.
When looking at the results it can be seen that the logistic regression models highlight service rating as the most significant predictor of satisfaction. Adjustments to service processes may be the way to improve satisfaction. Insights from this analysis can help restaurants prioritize investments, such as training staff or upgrading menu offerings, to target areas with the most potential for boosting customer satisfaction.")
       } else if (input$textOption == "Exploration") {
         return("I started my analysis of the data by exploring the relationships between various factors influencing customer satisfaction, such as age, service rating, food quality, and ambiance. Another thing that I looked into is how, and if, Income affected anything with satisfaction. My results showed that Income does not have a significant impact on overall satisfaction. These results with income showed me that it didn’t really have a huge impact of the overall satisfaction of the customers so I continued looking into different factors that could affect customer satisfaction. An exploratory plot of Age vs. High Satisfaction revealed that younger customers generally report lower satisfaction scores. This showed that there were specific factors that went into affecting overall satisfaction. The plots of age against other variables like Service Rating highlight the area where improvements could make a significant impact on the increase of higher satisfaction ratings. When I looked at this plot of age and service rating, I saw that there was a decline with age and service ratings. Another important finding is when looking at the Mean Satisfaction vs. Age Group plot. This plot showed that there was a slight incline in satisfaction as age increases. These results were not exactly what I was expecting as I expected customers older in age to be more critical, but overall, the younger age customers were. These results however are helpful as they suggest that specific demographics may require tailored services to boost satisfaction.")
       } else {
      return("When I just plotted the data in the exploratory plot, I was not able to get much information from it because there was just so much going on. So, I made other plots like the Conditional Density Plot and Estimated Category Probability plot which show how the satisfaction of a customer changed with age. I also employed logistic regression to predict customer satisfaction ('High Satisfaction' vs. 'Low Satisfaction'). This approach models the probability of a positive outcome as a function of predictor variables like Age, Service Rating, Food Rating, and Ambiance Rating. In order for me to do this, binary columns were created for key variables such as ServiceRatingBinary, FoodRatingBinary, and AmbianceRatingBinary. These plots helped me identify the specific factors that were affecting overall satisfaction of customers to help me identify what specific areas could improve. I created more plots such as a Receiver Operating Characteristic (ROC) curve plot which evaluates model performance by plotting true positive rates vs. false positive rates. The Area Under the Curve (AUC) is then looked at with these plots to quantify how well the model distinguishes between high satisfied and low satisfied customers. In order for me to validate the model's performance, a random baseline is compared, where satisfaction is randomly assigned. The results from these plots showed that there is no significant gap between the AUC of the trained model and the random model. Due to this, it is shown that the predictors used less meaningful. The model achieves an area under the curve of 0.5303, indicating semi good model performance. This is shown by the Model Evaluation (ROC Curve). The next thing I did was I created random data and built a model on it in order to test the effectiveness of my model. Only the satisfaction variable was changed. All of this showed me that my model worked well as the random model did not work as well when looking at the area under the curve for the random model. Also, when going back to the Logistic Regression Result plots, I discovered that service rating has the highest coefficient, showing it is the strongest driver of satisfaction. By visualizing relationships and building a logistic regression model, my analysis uncovers actionable insights to help restaurants focus their efforts on factors that matter most to customers.")
    }
  })
}

generate_plot <- function(data, plotType) {
  if (plotType == "Exploratory Plot") {
    ggplot(data, aes(x = Age, y = HighSatisfaction)) +
      geom_jitter(width = 0.5, height = 0.05, alpha = 0.5, color = "#AA336A") +
      labs(title = "Age vs High Satisfaction",
           x = "Age", y = "High Satisfaction") +
      theme_minimal()
    
  } else if (plotType == "Estimated Category Probability") {
    cdplot(factor(HighSatisfaction) ~ Age, data = data,
           main = "Estimated Category Probability",
           ylab = "High Satisfaction")
    
  } else if (plotType == "Conditional Density Plot") {
    ggplot(data, aes(x = Age, fill = factor(HighSatisfaction))) +
      geom_density(position = "fill", alpha = 0.4) +
      ylab("Probability") +
      labs(title = "Conditional Density of Age by Satisfaction Level",
           x = "Age", fill = "High Satisfaction") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
  } else if (plotType == "Logistic Regression Results - Service Rating") {
    model <- glm(ServiceRatingBinary ~ Age, data = data, family = binomial(link = "logit"))
    visreg::visreg(model, "Age", scale = "response", partial = FALSE,
                   xlab = "Age", ylab = "P(Service Rating >= 5)")
    
  } else if (plotType == "Logistic Regression Results - Food Rating") {
    model <- glm(FoodRatingBinary ~ Age, data = data, family = binomial(link = "logit"))
    visreg::visreg(model, "Age", scale = "response", partial = FALSE,
                   xlab = "Age", ylab = "P(Food Rating >= 5)")
    
  } else if (plotType == "Logistic Regression Results - Ambiance Rating") {
    model <- glm(AmbianceRatingBinary ~ Age, data = data, family = binomial(link = "logit"))
    visreg::visreg(model, "Age", scale = "response", partial = FALSE,
                   xlab = "Age", ylab = "P(Ambiance Rating >= 5)")
    
  } else if (plotType == "Logistic Regression Results - Overall Satisfaction") {
    model <- glm(HighSatisfaction ~ Age, data = data, family = binomial(link = "logit"))
    visreg::visreg(model, "Age", scale = "response", partial = FALSE,
                   xlab = "Age", ylab = "P(High Satisfaction)")
    
  } else if (plotType == "Another Logistic Regression Results - Overall Satisfaction") {
    model <- glm(HighSatisfaction ~ Age + ServiceRating + FoodRating + AmbianceRating, 
                 data = data, family = binomial(link = "logit"))
    popbio::logi.hist.plot(data$Age, data$HighSatisfaction, boxp = FALSE, type = "hist", col = "gray")
    title(main = "Another Logistic Regression - Overall Satisfaction")
    
  } else if (plotType == "Model Evaluation (ROC Curve)") {
    model <- glm(HighSatisfaction ~ Age, data = data, family = binomial(link = "logit"))
    pred <- predict(model, data, type = "response")
    pObject <- ROCR::prediction(pred, data$HighSatisfaction)
    rocObj <- ROCR::performance(pObject, measure = "tpr", x.measure = "fpr")
    aucObj <- ROCR::performance(pObject, measure = "auc")
    plot(rocObj, main = paste("ROC Curve - AUC:", round(aucObj@y.values[[1]], 4)), col = "#AA336A", lwd = 2)
    abline(a = 0, b = 1, col = "gray", lty = 2)
    
  } else if (plotType == "Random Model (ROC Curve)") {
    set.seed(1235)
    data$HighSatisfaction <- sample(c(0, 1), replace = TRUE, size = nrow(data))
    model <- glm(HighSatisfaction ~ Age, data = data, family = binomial(link = "logit"))
    pred <- predict(model, data, type = "response")
    pObject <- ROCR::prediction(pred, data$HighSatisfaction)
    rocObj <- ROCR::performance(pObject, measure = "tpr", x.measure = "fpr")
    aucObj <- ROCR::performance(pObject, measure = "auc")
    plot(rocObj, main = paste("Random Model ROC - AUC:", round(aucObj@y.values[[1]], 4)), col = "#AA336A", lwd = 2)
    abline(a = 0, b = 1, col = "gray", lty = 2)
  }
}

generate_plot2 <- function(data, plotType2) {
  if (plotType2 == "Frequency of High Satisfaction by Age") {
    ggplot(data, aes(x = Age, fill = factor(HighSatisfaction))) +
      geom_histogram(binwidth = 5, position = "dodge") +
      labs(x = "Age", y = "Frequency", title = "Frequency of High Satisfaction by Age") +
      theme_minimal()
    
  } else if (plotType2 == "Mean Satisfaction vs Age Group") {
    data$AgeGroup <- cut(data$Age, breaks = seq(0, 100, by = 10))
    mean_satisfaction <- data %>%
      group_by(AgeGroup) %>%
      summarise(mean_satisfaction = mean(HighSatisfaction))
    
    ggplot(mean_satisfaction, aes(x = AgeGroup, y = mean_satisfaction)) +
      geom_point(size = 3) +
      labs(x = "Age Group", y = "Mean Satisfaction", title = "Mean Satisfaction vs Age Group") +
      theme_minimal()
    
  } else if (plotType2 == "Mean Satisfaction vs Service Rating") {
    mean_seat <- data %>%
      group_by(ServiceRating) %>%
      summarise(mean_satisfaction = mean(HighSatisfaction))
    
    ggplot(mean_seat, aes(x = factor(ServiceRating), y = mean_satisfaction)) +
      geom_point(size = 3) +
      labs(x = "Service Rating", y = "Mean Satisfaction", title = "Mean Satisfaction vs Service Rating") +
      theme_minimal()
  } else if (plotType2 == "Mean Satisfaction vs Food Rating") {
    mean_seat <- data %>%
      group_by(FoodRating) %>%
      summarise(mean_satisfaction = mean(HighSatisfaction))
    
    ggplot(mean_seat, aes(x = factor(FoodRating), y = mean_satisfaction)) +
      geom_point(size = 3) +
      labs(x = "Food Rating", y = "Mean Satisfaction", title = "Mean Satisfaction vs Food Rating") +
      theme_minimal()
  } else if (plotType2 == "Mean Satisfaction vs Ambiance Rating") {
    mean_seat <- data %>%
      group_by(AmbianceRating) %>%
      summarise(mean_satisfaction = mean(HighSatisfaction))
    
    ggplot(mean_seat, aes(x = factor(AmbianceRating), y = mean_satisfaction)) +
      geom_point(size = 3) +
      labs(x = "Ambiance Rating", y = "Mean Satisfaction", title = "Mean Satisfaction vs Ambiance Rating") +
      theme_minimal()
  }
}

shinyApp(ui = ui, server = server)
