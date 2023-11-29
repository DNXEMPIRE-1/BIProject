# LAST VERSION SO FAR
library(shiny)
library(shinydashboard)
library(gridExtra)
library(factoextra)
library(mice)
library(naniar)
library(VIM)
library("plotrix")
library(ggplot2)
library(dplyr)
library(corrplot)
library(factoextra)
library(visdat)
library(caret)
library(caTools)
library(kohonen)
library(pROC)
library(tidyverse)
library(MASS)




ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis"),
  
  dashboardSidebar(
    fileInput("file", "Choose CSV file", accept = c(".csv", ".xlsx"), multiple = FALSE, 
              buttonLabel = "Browse", placeholder = "No file selected", width = "100%"),
    actionButton("submit","Submit"),
    tags$br(),
    tags$br(),
    sidebarMenu(
      menuItem("Documentation", tabName = "documentation", icon = icon("book")),
      menuItem("Data Cleaning", tabName = "data_cleaning", icon = icon("eye-slash"),
               menuSubItem("Missing Values", tabName = "missing_values"),
               menuSubItem("Distributions", tabName = "distributions")
      ),
      menuItem("Data Analysis", tabName = "data_analysis", icon = icon("chart-line"),
               menuSubItem("Overview", tabName = "overview"),
               menuSubItem("Relationships", tabName = "relationships"),
               menuSubItem("Models", tabName = "models"),
               menuSubItem("Machine Learning Models", tabName = "ml_models"),
               menuSubItem("Model Visualization", tabName = "model_visualization")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "documentation",
        includeMarkdown("documentation.rmd")
      ),
      tabItem(tabName = "overview", 
              fluidRow(
                box(tableOutput("summary"), width = 6, height = 2000),
                box(plotOutput("outcome_pie_chart"), width = 6, height = 500)
              )
      ),
      
      tabItem(tabName = "missing_values",
              fluidRow(
                box(plotOutput("miss_plot"), width = 6, height = 500),
                box(plotOutput("vim_plot1"), width = 6, height = 500)
              ),
              fluidRow(
                box(plotOutput("vim_plot2"), width = 6, height = 500),
                box(tabsetPanel(
                  tabPanel("Pattern 1", plotOutput("multi_vim_plot1")),
                  tabPanel("Pattern 2", plotOutput("multi_vim_plot2")),
                  tabPanel("Pattern 3", plotOutput("multi_vim_plot3")),
                  tabPanel("Pattern 4", plotOutput("multi_vim_plot4")),
                  tabPanel("Pattern 5", plotOutput("multi_vim_plot5")),
                  tabPanel("Pattern 6", plotOutput("multi_vim_plot6")),
                  tabPanel("Pattern 7", plotOutput("multi_vim_plot7")),
                  tabPanel("Pattern 8", plotOutput("multi_vim_plot8"))
                  
                ), width = 6, height = 500)
              )
      ),
      
      tabItem(tabName = "relationships",
              fluidRow(
                box(plotOutput("k_means_plot"), width = 6, height = 500),
                # box(plotOutput("corrplott"), width = 6, height = 500),
                box(tabsetPanel(
                  tabPanel("Pregnancies Indicator", tableOutput("summarize_preg_by_age")),
                  tabPanel("Pregnancies BoxPlot", plotOutput("anoma_eval_preg")),
                  tabPanel("Insulin Indicator", tableOutput("summarize_insu_by_age")),
                  tabPanel("Insulin BoxPlot", plotOutput("anoma_eval_insu")),
                  tabPanel("BloodPressure Indicator", tableOutput("summarize_blpr_by_age")),
                  tabPanel("BloodPressure BoxPlot", plotOutput("anoma_eval_blpr"))
                ), width = 6, height = 2000),
                box(tabsetPanel(
                  tabPanel("Positive Outcome", tableOutput("summarize_pos_outcome")),
                  tabPanel("Negative Outcome", tableOutput("summarize_neg_outcome"))
                ), width = 6, height = 2000)
              )
      ),
      
      
      tabItem(tabName = "models",
              fluidRow(
                box(plotOutput("model_plot1"), width = 6, height = 500),
                box(plotOutput("model_plot2"), width = 6, height = 500),
                box(tableOutput("diabvsno"), width = 2, height = 100)
              )
      ),
      
      tabItem(tabName = "ml_models",
              fluidRow(
                # RANDOM FOREST
                box(tabsetPanel(
                  tabPanel("Random Forest", plotOutput("rf_partplot")),
                  tabPanel("Mean Importance % for Each Factor", plotOutput("rf_impplot"))
                ), width = 6, height = 500),
                # KNN
                box(tabsetPanel(
                  tabPanel("KNN Accuracy vs. Neighbors Nbr", plotOutput("knn_plot")),
                  tabPanel("Mean Accuracy % for Each Neighbor", plotOutput("knn_accplot")),
                  tabPanel("KNN Performance", plotOutput("knn_prfplot"))
                ), width = 6, height = 500),
                # LOGISTIC REGRESSION
                box(tabsetPanel(
                  tabPanel("Accuracy Distribution", plotOutput("logreg_plot"))
                ), width = 6, height = 500),
                # Linear Discriminant Analysis (LDA)
                box(tabsetPanel(
                  #tabPanel("Table of the LDA Model Creation", tableOutput("lda_table")),
                  tabPanel("Performance Indexes BarPlot", plotOutput("lda_plot")),
                  tabPanel("LDA Histogram", plotOutput("lda_histplot")),
                  tabPanel("LDA Density Plot", tableOutput("lda_densplot"))
                ), width = 6, height = 500)
              )
      ),
      
      tabItem(tabName = "model_visualization", 
              fluidRow(
                box(plotOutput("som_modelplot2"), width = 6, height = 500),
                box(plotOutput("som_modelplot22"), width = 6, height = 500),
                box(plotOutput("som_modelplot3"), width = 6, height = 500),
                box(plotOutput("som_modelplot33"), width = 6, height = 500),
                box(plotOutput("som_modelplot1"), width = 6, height = 500)
              )
      ),
      tabItem(tabName = "distributions", 
              fluidRow(
                box(tabsetPanel(
                  tabPanel("Distribution of Pregnancies", plotOutput("dist_preg_impd")),
                  tabPanel("Distribution of Glucose Levels", plotOutput("dist_gluc_impd")),
                  tabPanel("Distribution of BMI", plotOutput("dist_bmi_impd")),
                  tabPanel("Distribution of Blood Pressure Levels", plotOutput("dist_blpr_impd")),
                  tabPanel("Distribution of Skin Thickness", plotOutput("dist_skth_impd")),
                  tabPanel("Distribution of Age", plotOutput("dist_age_impd")),
                  tabPanel("Distribution of Diabetes Pedigree Fct", plotOutput("dist_diab_impd"))
                ), width = 6, height = 600),
                box(tabsetPanel(
                  tabPanel("BoxPlot of Pregnancies", plotOutput("preg_boxp")),
                  tabPanel("BoxPlot of Skin Thickness", plotOutput("skth_boxp")),
                  tabPanel("BoxPlot of BMI", plotOutput("bmi_boxp")),
                  tabPanel("BoxPlot of Glucose", plotOutput("gluco_boxp"))
                ))
              )
      )
    )
  )
)

# defines the server function
server <- function(input, output) {
  # observeEvent to trigger the code when the submit button is clicked.
  observeEvent(input$submit, {
    req(input$file)
    
    # read the data
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE, header = TRUE)
    #  replace the 0 values with NA
    data[, 2:9][data[, 2:9] == 0] <- NA
    
    data <- read.csv(file="Diabetes dataset.csv",
                     stringsAsFactors=FALSE,header=TRUE)
    # summary statistics for the data 
    summary_data <- summary(data)
    output$summary <- renderTable(summary_data)
    
    
    data[, 2:9][data[, 2:9] == 0] <- NA
    # calculating the percentage of missing values in each column of the data
    col_missing <- colMeans(is.na(data)) * 100
    #  doing the same to each row of the data
    row_missing <- rowMeans(is.na(data)) * 100
    
    col_missing <- sapply(data, function(x) sum(is.na(x))/length(x)) * 100
    row_missing <- apply(data, 1, function(x) sum(is.na(x))/length(x)) * 100
    
    # creating a missingness plot from the visdat package
    miss_plott <- vis_miss(data)
    # The plot visualizes the pattern of missing values in the dataset
    output$miss_plot <- renderPlot(miss_plott)
    
    # generating a visualization of the data
    # provides an overview of the dataset, displaying the missingness and distribution of variables
    vim_plot1 <- vis_dat(data)
    output$vim_plot1 <- renderPlot(vim_plot1)
    
    
    vim_plot2 <- gg_miss_var(data)
    output$vim_plot2 <- renderPlot(vim_plot2)
    
    # Each plot compares the first variable (data[1]) to the other variable
    output$multi_vim_plot1 <- renderPlot(marginplot(data[c(1,2)]))
    output$multi_vim_plot2 <- renderPlot(marginplot(data[c(1,3)]))
    output$multi_vim_plot3 <- renderPlot(marginplot(data[c(1,4)]))
    output$multi_vim_plot4 <- renderPlot(marginplot(data[c(1,5)]))
    output$multi_vim_plot5 <- renderPlot(marginplot(data[c(1,6)]))
    output$multi_vim_plot6 <- renderPlot(marginplot(data[c(1,7)]))
    output$multi_vim_plot7 <- renderPlot(marginplot(data[c(1,8)]))
    output$multi_vim_plot8 <- renderPlot(marginplot(data[c(1,9)]))
    
    # applying multiple imputations using the mice()
    # selecting all the columns except the 'outcome' with 5 imputations
    tempData <- mice(data[, !names(data) %in% "Outcome"],m=5,maxit=5,meth='norm.predict',seed=500)
    
    # a vector that contains the vars that need to be imputed
    vars_to_impute <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
    # iterate over the 'vars_to_impute' and access the imoutations stored in tempData$imp... 
    for (i in 1:length(vars_to_impute)) {
      tempData$imp[[vars_to_impute[i]]]
    }
    # extracting the completed dataset
    impdata <- complete(tempData,2)
    impdata$Outcome <- data$Outcome
    
    # for unsupervised dataset used for further analysis.
    UnSupData <-impdata
    
    # providing a summary of the imputed dataset
    summary(impdata)
    str(impdata)
    
    # applying table() to obtain the counts for each outcome
    outcome_counts <- as.data.frame(table(impdata$Outcome))
    colnames(outcome_counts) <- c("Outcome", "Freq")
    
    # generating a pie chart of the outcome variable based on the outcome_counts data frame
    pie_chart <- ggplot(outcome_counts, aes(x = "", y = Freq, fill = factor(Outcome))) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y") +
      ggtitle("Pie Chart of Outcome") +
      scale_fill_manual(values = c("darkseagreen1", "red")) +
      labs(fill = "Outcome", y = NULL) +
      theme_void()+
      geom_text(aes(label = paste0(round(Freq/sum(Freq)*100), "%")), position = position_stack(vjust = 0.5))
    output$outcome_pie_chart <- renderPlot(pie_chart)
    
    # counting the occurrences of each unique value
    # count() groups the data by "Outcome" and calculates the count for each value.
    # mutate() to add a new column "prop" that represents the proportion of each count
    diabvsnovar <- data %>%
      count(Outcome) %>%
      mutate(prop = n / sum(n))
    output$diabvsno <- renderTable(diabvsnovar)
    
    # grouping the impdata dataframe by "Age"
    # calculating the mean, median, and standard dev of "Pregnancies" for each age group.
    summarize_preg_by_agevar <- impdata %>%
      group_by(Age) %>%
      summarise(Pregnancies_mean = mean(Pregnancies),
                Pregnancies_median = median(Pregnancies),
                Pregnancies_sd = sd(Pregnancies))
    
    output$summarize_preg_by_age <- renderTable(summarize_preg_by_agevar)
    
    # grouping the impdata dataframe by "Age"
    # calculating the mean, median, and standard dev of "Insulin" for each age group. 
    summarize_insu_by_agevar <- impdata %>%
      group_by(Age) %>%
      summarise(Insulin_mean = mean(Insulin),
                Insulin_median = median(Insulin),
                Insulin_sd = sd(Insulin))
    
    output$summarize_insu_by_age <- renderTable(summarize_insu_by_agevar)
    
    # grouping the impdata dataframe by "Age"
    # calculating the mean, median, and standard dev of "BloodPressure" for each age group.
    summarize_blpr_by_agevar <- impdata %>%
      group_by(Age) %>%
      summarise(BloodPressure_mean = mean(BloodPressure),
                BloodPressure_median = median(BloodPressure),
                BloodPressure_sd = sd(BloodPressure))
    
    output$summarize_blpr_by_age <- renderTable(summarize_blpr_by_agevar)
    
    # render boxplots for the "Pregnancies," "Insulin," and "BloodPressure"
    output$anoma_eval_preg <- renderPlot(boxplot(Pregnancies ~ Age, data=impdata, outline = TRUE, names, plot = TRUE, 
                                                 col= 'light blue',   xlab = "Age", ylab = "Pregnancies"))
    
    output$anoma_eval_insu <- renderPlot(boxplot(Insulin ~ Age, data=impdata, outline = TRUE, names, plot = TRUE, 
                                                 col= 'light blue',   xlab = "Age", ylab = "Insulin"))
    
    output$anoma_eval_blpr <- renderPlot(boxplot(BloodPressure ~ Age, data=impdata, outline = TRUE, names, plot = TRUE, 
                                                 col= 'light blue',   xlab = "Age", ylab = "BloodPressure"))
    
    # cut() to bin the "Age" variable into groups with a sequence from 20 to 80 with a step of 1.
    impdata$age_group <- cut(impdata$Age, breaks = seq(20, 80, by = 1))
    
    # generating a boxplot of "Pregnancies" by "age_group"
    model_plot1var <- ggplot(impdata, aes(x = age_group, y = Pregnancies)) +
      geom_boxplot() +
      scale_fill_manual(values = c(rep("light blue"))) +
      labs(title = "Boxplots of Pregnancies by Age Group", x = "Age Group", y = "Pregnancies")
    
    output$model_plot1 <- renderPlot(model_plot1var)
    
    # generating a scatter plot of "Age" vs. "DiabetesPedigreeFunction" using geom_point()
    model_plot2var <- ggplot(impdata, aes(x = Age, y = DiabetesPedigreeFunction)) +
      geom_point() +
      facet_wrap(~ Outcome, nrow = 1) +
      labs(title = "Scatter Plot of Age vs. Diabetes Pedigree Function",
           x = "Age", y = "Diabetes Pedigree Function")
    
    output$model_plot2 <- renderPlot(model_plot2var)
    
    
    # linear regression models fitted to the impdata dataset between independent variables and the dependent var (outcome)
    fit0 <- lm(Age ~ DiabetesPedigreeFunction, data = impdata)
    fit <- lm(Outcome ~ Age + Pregnancies, data = impdata)
    fit1 <- lm(Outcome ~ Glucose + Insulin, data = impdata)
    fit2 <- lm(Outcome ~ BMI + SkinThickness, data = impdata)
    
    # filtering rows depending on the value of (outcome) 
    NegativeOutcome <- filter(impdata, Outcome == 0)
    PositiveOutcome <- filter(impdata, Outcome == 1)
    nrow(NegativeOutcome)
    nrow(PositiveOutcome)
    
    output$summarize_neg_outcome <- renderTable(table(NegativeOutcome$Age))
    output$summarize_pos_outcome <- renderTable(table(PositiveOutcome$Age)) 
    
    # creating a variable in the impdata dataframe with two levels: "Negative" and "Positive."
    impdata$Outcome_factor <- factor(impdata$Outcome, levels = c(0, 1), labels = c("Negative", "Positive"))
    
    # showing the distribution of (Ages) based on the outcome 
    age_vs_outcome_plotvar <- ggplot(impdata, aes(x = Outcome_factor, y = Age, fill = Outcome_factor)) + 
      geom_boxplot() +
      labs(title = "Boxplots of Age by Outcome", x = "Outcome", y = "Age") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
    output$age_vs_outcome_plot <- renderPlot(age_vs_outcome_plotvar)
    
    #  histogram plot that shows the distribution of (Ages)
    # The plot is facet-wrapped by outcome
    age_vs_diab_histplotvar <- ggplot(impdata, aes(x=Age, fill=factor(Outcome))) +
      geom_histogram(alpha=0.5, position="identity", bins=30) +
      scale_fill_manual(values = c("green", "red")) +
      facet_wrap(~Outcome, ncol=2) +
      xlab("Age") +
      ylab("Count")
    # plot
    output$age_vs_diab_histplot <- renderPlot(age_vs_diab_histplotvar)
    
    # applying filters and plots
    100 * sum(NegativeOutcome$Pregnancies > 5) / nrow(NegativeOutcome)
    100 * sum(PositiveOutcome$Pregnancies > 5) / nrow(PositiveOutcome)
    
    
    100 * sum(NegativeOutcome$Age > 40) / nrow(NegativeOutcome)
    100 * sum(PositiveOutcome$Age > 40) / nrow(PositiveOutcome)
    
    
    100 * sum(NegativeOutcome$Pregnancies > 5, NegativeOutcome$Age > 35) / nrow(NegativeOutcome)
    100 * sum(PositiveOutcome$Pregnancies > 5, PositiveOutcome$Age > 40) / nrow(PositiveOutcome)
    
    (nrow(filter(NegativeOutcome,NegativeOutcome$Pregnancies > 5, NegativeOutcome$Age > 35))/
        nrow(NegativeOutcome))*100
    (nrow(filter(PositiveOutcome,PositiveOutcome$Pregnancies > 5, PositiveOutcome$Age > 35))
      /nrow(PositiveOutcome))*100
    
    gluc_vs_outcome_plotvar <- ggplot(impdata, aes(x = Outcome_factor, y = Glucose, fill = Outcome_factor)) + 
      geom_boxplot() +
      labs(title = "Boxplots of Age by Outcome", x = "Outcome", y = "Glucose") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    output$gluc_vs_outcome_plot <- renderPlot(gluc_vs_outcome_plotvar)
    
    
    gluc_vs_outcome_distplotvar <- ggplot(impdata, aes(x=Glucose, fill=factor(Outcome))) +
      geom_histogram(alpha=0.5, position="identity", bins=30) +
      scale_fill_manual(values = c("green", "red")) +
      facet_wrap(~Outcome, ncol=2) +
      xlab("Age") +
      ylab("Glucose")
    # plot
    output$gluc_vs_outcome_distplot <- renderPlot(gluc_vs_outcome_distplotvar)
    
    
    
    impdata <- impdata %>% 
      mutate(GlucoseBinning = ifelse(Glucose >= 110, 1, 0))
    
    
    sum(NegativeOutcome$Glucose >= 110)
    
    sum(PositiveOutcome$Glucose >= 110)
    
    sum(impdata$Glucose >= 110)
    
    sum(PositiveOutcome$Glucose > 110)
    
    sum(impdata$Glucose > 110)
    sum(NegativeOutcome$Glucose > 110)
    sum(PositiveOutcome$Glucose > 110)
    
    (sum(NegativeOutcome$Glucose > 110) / nrow(NegativeOutcome)) * 100
    (sum(PositiveOutcome$Glucose > 110) / nrow(PositiveOutcome)) * 100
    (sum(NegativeOutcome$Glucose > 110) / nrow(NegativeOutcome)) * 100
    (sum(PositiveOutcome$Glucose > 110) / nrow(PositiveOutcome)) * 100
    
    (sum(impdata$Glucose > 110) / nrow(impdata)) * 100
    
    
    sum(NegativeOutcome$Insulin < 166)
    
    sum(PositiveOutcome$Insulin < 166)
    
    sum(impdata$Insulin < 166)
    
    sum(PositiveOutcome$Insulin >= 166)
    
    
    insu_vs_outcome_plotvar <- ggplot(impdata, aes(x = Outcome_factor, y = Insulin, fill = Outcome_factor)) + 
      geom_boxplot() +
      labs(title = "Boxplots of Age by Outcome", x = "Outcome", y = "Insulin") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    output$insu_vs_outcome_plot <- renderPlot(insu_vs_outcome_plotvar)
    
    
    insu_vs_diab_plotvar <- ggplot(impdata, aes(x=Insulin, fill=factor(Outcome))) +
      geom_histogram(alpha=0.5, position="identity", bins=30) +
      scale_fill_manual(values = c("green", "red")) +
      facet_wrap(~Outcome, ncol=2) +
      xlab("Age") +
      ylab("Insulin")
    output$insu_vs_diab_plot <- renderPlot(insu_vs_diab_plotvar)
    
    
    sum(NegativeOutcome$Insulin > 166)
    sum(PositiveOutcome$Insulin > 166)
    
    (sum(NegativeOutcome$Insulin > 166) / nrow(NegativeOutcome)) * 100
    (sum(PositiveOutcome$Insulin > 166) / nrow(PositiveOutcome)) * 100
    
    
    sum(NegativeOutcome$BMI > 26)
    sum(PositiveOutcome$BMI > 26)
    
    (sum(NegativeOutcome$BMI > 30) / nrow(NegativeOutcome)) * 100
    (sum(PositiveOutcome$BMI > 30) / nrow(PositiveOutcome)) * 100
    
    
    sum(NegativeOutcome$SkinThickness > 26)
    sum(PositiveOutcome$SkinThickness > 26)
    
    (sum(NegativeOutcome$SkinThickness > 26) / nrow(NegativeOutcome)) * 100
    (sum(PositiveOutcome$SkinThickness > 26) / nrow(PositiveOutcome)) * 100
    
    
    # histogram plots created that displays the distribution of the variables
    # The x-axis represents each respective variable, 
    # the y-axis represents the density
    dist_preg_impdvar <- ggplot(data = impdata, aes(x = Pregnancies)) + 
      geom_histogram(aes(y = ..density..), col = 'black', fill = 'light blue') + 
      stat_function(fun = dnorm, args = list(mean = mean(impdata$Pregnancies), sd = sd(impdata$Pregnancies)), aes(colour = "Density")) +
      labs(x = "Pregnancies", y = "Density") +
      theme_classic()
    output$dist_preg_impd <- renderPlot(dist_preg_impdvar)
    
    dist_gluc_impdvar <- ggplot(data = impdata, aes(x = Glucose)) + 
      geom_histogram(aes(y = ..density..), col = 'black', fill = 'light blue') + 
      stat_function(fun = dnorm, args = list(mean = mean(impdata$Glucose), sd = sd(impdata$Glucose)), aes(colour = "Density")) +
      labs(x = "Glucose", y = "Density") +
      theme_classic()
    output$dist_gluc_impd <- renderPlot(dist_gluc_impdvar)
    
    dist_blpr_impdvar <- ggplot(data = impdata, aes(x = BloodPressure)) + 
      geom_histogram(aes(y = ..density..), col = 'black', fill = 'light blue') + 
      stat_function(fun = dnorm, args = list(mean = mean(impdata$BloodPressure), sd = sd(impdata$BloodPressure)), aes(colour = "Density")) +
      labs(x = "BloodPressure", y = "Density") +
      theme_classic()
    output$dist_blpr_impd <- renderPlot(dist_blpr_impdvar)
    
    dist_skth_impdvar <- ggplot(data = impdata, aes(x = SkinThickness)) + 
      geom_histogram(aes(y = ..density..), col = 'black', fill = 'light blue') + 
      stat_function(fun = dnorm, args = list(mean = mean(impdata$SkinThickness), sd = sd(impdata$SkinThickness)), aes(colour = "Density")) +
      labs(x = "SkinThickness", y = "Density") +
      theme_classic()
    output$dist_skth_impd <- renderPlot(dist_skth_impdvar)
    
    dist_insu_impdvar <- ggplot(data = impdata, aes(x = SkinThickness)) + 
      geom_histogram(aes(y = ..density..), col = 'black', fill = 'light blue') + 
      stat_function(fun = dnorm, args = list(mean = mean(impdata$SkinThickness), sd = sd(impdata$SkinThickness)), aes(colour = "Density")) +
      labs(x = "SkinThickness", y = "Density") +
      theme_classic()
    output$dist_insu_impd <- renderPlot(dist_insu_impdvar)
    
    dist_bmi_impdvar <- ggplot(data = impdata, aes(x = BMI)) + 
      geom_histogram(aes(y = ..density..), col = 'black', fill = 'light blue') + 
      stat_function(fun = dnorm, args = list(mean = mean(impdata$BMI), sd = sd(impdata$BMI)), aes(colour = "Density")) +
      labs(x = "BMI", y = "Density") +
      theme_classic()
    output$dist_bmi_impd <- renderPlot(dist_bmi_impdvar)
    
    dist_age_impdvar <- ggplot(data = impdata, aes(x = Age)) + 
      geom_histogram(aes(y = ..density..), col = 'black', fill = 'light blue') + 
      stat_function(fun = dnorm, args = list(mean = mean(impdata$Age), sd = sd(impdata$Age)), aes(colour = "Density")) +
      labs(x = "Age", y = "Density") +
      theme_classic()
    output$dist_age_impd <- renderPlot(dist_age_impdvar)
    
    dist_diab_impdvar <- ggplot(data = impdata, aes(x = DiabetesPedigreeFunction)) + 
      geom_histogram(aes(y = ..density..), col = 'black', fill = 'light blue') + 
      stat_function(fun = dnorm, args = list(mean = mean(impdata$DiabetesPedigreeFunction), sd = sd(impdata$DiabetesPedigreeFunction)), aes(colour = "Density")) +
      labs(x = "Pedigree", y = "Density") +
      theme_classic()
    output$dist_diab_impd <- renderPlot(dist_diab_impdvar)
    
    
    preg_boxpvar <- ggplot(impdata, aes(x = "", y = Pregnancies)) +
      geom_boxplot(fill = "light blue", color = "black") +
      labs(x = "Pregnancies")
    # plot
    output$preg_boxp <- renderPlot(preg_boxpvar)
    
    skth_boxpvar <- ggplot(impdata, aes(x = "", y = SkinThickness)) +
      geom_boxplot(fill = "light blue", color = "black") +
      labs(x = "SkinThickness")
    # plot
    output$skth_boxp <- renderPlot(skth_boxpvar)
    
    bmi_boxpvar <- ggplot(impdata, aes(x = "", y = BMI)) +
      geom_boxplot(fill = "light blue", color = "black") +
      labs(x = "BMI")
    # plot
    output$bmi_boxp <- renderPlot(bmi_boxpvar)
    
    gluco_boxpvar <- ggplot(impdata, aes(x = "", y = Glucose)) +
      geom_boxplot(fill = "light blue", color = "black") +
      labs(x = "Glucose")
    # plot
    output$gluco_boxp <- renderPlot(gluco_boxpvar)
    
    
    # a function 'outliers' that calculates the interquartile range
    # then identifies lower outliers that are < (y - (1.5 * i))
    # and greater outliers that are > (z + (1.5 * i))
    outliers <- function(df, x) {
      i <- IQR(x)
      y <- quantile(x, 0.25)
      z <- quantile(x, 0.75)
      
      lower <- df[x < (y - (1.5 * i)), ]
      greater <- df[x > (z + (1.5 * i)), ]
      
      return(rbind(lower, greater))
    }
    # applying the 'outliers' function to different variables from the impdata data frame
    df_bmi_outliers <- outliers(df = impdata, x = impdata$BMI)
    df_glucose_outliers <- outliers(df = impdata, x = impdata$Glucose)
    df_preg_outliers <- outliers(df = impdata, x = impdata$Pregnancies)
    df_dpf_outliers <- outliers(df = impdata, x = impdata$DiabetesPedigreeFunction)
    df_age_outliers <- outliers(df = impdata, x = impdata$Age)
    df_st_outliers <- outliers(df = impdata, x = impdata$SkinThickness)
    df_insulin_outliers <- outliers(df = impdata, x = impdata$Insulin)
    df_bp_outliers <- outliers(df = impdata, x = impdata$BloodPressure)
    
    df_insulin_outliers$Outcome
    
    # show the number of observations and variables.
    dim(df_bmi_outliers)[1]
    dim(df_glucose_outliers)[1]
    dim(df_preg_outliers)[1]
    dim(df_dpf_outliers)[1]
    dim(df_age_outliers)[1]
    dim(df_st_outliers)[1]
    dim(df_insulin_outliers)[1]
    dim(df_bp_outliers)[1]
    
    
    # computing boxplot statistics for each variable
    # which() to find the indices of 'outliers' for each var by comparing them to the out values from the boxplot statistics
    (a <- which(impdata$BMI %in% boxplot.stats(impdata$BMI)$out))
    (b <- which(impdata$Glucose %in% boxplot.stats(impdata$Glucose)$out))
    (c <- which(impdata$Pregnancies %in% boxplot.stats(impdata$Pregnancies)$out))
    (d <- which(impdata$SkinThickness %in% boxplot.stats(impdata$SkinThickness)$out))
    (e <- which(impdata$Age %in% boxplot.stats(impdata$Age)$out))
    (f <- which(impdata$DiabetesPedigreeFunction %in% boxplot.stats(impdata$DiabetesPedigreeFunction)$out))
    (g <- which(impdata$Insulin %in% boxplot.stats(impdata$Insulin)$out))
    (h <- which(impdata$BloodPressure %in% boxplot.stats(impdata$BloodPressure)$out))
    
    # all the outliers are combined into a single vector all_outliers using c()
    all_outliers <- c(a, b, c, d)
    
    # removing the rows corresponding to the outlier indices using negative indexing
    removeOutliers <- function(data, indicesOfAllOutliers) {
      return(data[-unlist(indicesOfAllOutliers),])
    }
    
    transformedData<-removeOutliers(impdata, as.data.frame(all_outliers))
    
    nrow(transformedData)
    
    # 'responseY' is assigned the last column of 'UnSupData'
    responseY <- UnSupData[,dim(UnSupData)[2]]
    # 'predictorX' is assigned all columns except the last one
    predictorX <- UnSupData[,1:(dim(UnSupData)[2]-1)] #Remove OUTCOME column
    
    # performing Principal Component Analysis (PCA) on 'predictorX'
    prcma <- princomp(predictorX, cor=T)  #cor is correlation matrix
    prcma
    pc.cmp <- prcma$scores
    
    pc.cmp1 <- -1*pc.cmp[,1] # principal component 1 scores (negated for convenience)
    pc.cmp2 <- -1*pc.cmp[,2] # principal component 2 scores (negated for convenience)
    
    
    # performing k-means clustering on the data X using kmeans() with 13 clusters.
    library(factoextra)
    
    X <- cbind(pc.cmp1, pc.cmp2)
    cl <- kmeans(X, 13)
    
    k_means_plotvar <- fviz_cluster(cl, data = X, geom = "point", palette = "jco", 
                                    main = "k-means Clustering analysis")
    output$k_means_plot <- renderPlot(k_means_plotvar)
    
    
    
    impdata$Outcome <- factor(make.names(impdata$Outcome))
    
    split <- sample.split(transformedData$Outcome, SplitRatio = 0.70)
    
    train_data <- subset(impdata,split == T)
    test_data <- subset(impdata,split == F)
    
    dim(train_data)[1]
    dim(test_data)[1]
    
    # standardizing the data to the first 8 columns of 'impdata'
    scale_data = scale(impdata[, 1:8])
    
    grid = somgrid(xdim = 5, ydim=5, topo="hexagonal")
    
    
    
    str(UnSupData)
    
    UnSupData$Outcome
    
    # splitting the 'UnSupData' into training and testing data
    split <- sample.split(UnSupData$Outcome, SplitRatio = 0.70)
    
    U_train_data <- subset(UnSupData,split == T)
    U_test_data <- subset(UnSupData,split == F)
    
    nrow(U_train_data)
    nrow(U_test_data)
    
    # standardizing the data
    scale_data = scale(impdata[, 1:8])
    # SOM (Self-Organizing Map) grid using somgrid() with dimensions 5x5 and a hexagonal topology
    grid = somgrid(xdim = 5, ydim=5, topo="hexagonal")
    
    # train the SOM model with rlen=number of iterations
    som = som(scale_data, grid=grid, rlen=100, alpha=c(0.05,0.01))
    
    # scaling the U_train_data using scale() and converting it to a matrix using as.matrix()
    data_train_matrix <- as.matrix(scale(U_train_data),center = TRUE, scale = TRUE)
    # train the SOM mode using the previous matrix using the same grid
    som_model <- som(data_train_matrix, 
                     grid=grid, 
                     rlen=100, 
                     alpha=c(0.05,0.01))
    
    # store the changes in the SOM model during training
    # generate a sequence of numbers corresponding to the iterations,
    # visualize the changes in the SOM model over iterations
    plot_changes <- data.frame(Iteration = seq_along(som_model$changes), Changes = som_model$changes)
    som_modelvar1 <- ggplot(plot_changes, aes(x = Iteration, y = Changes)) +
      geom_line() +
      labs(x = "Iteration", y = "Changes", title = "SOM Model Changes")
    output$som_modelplot1 <- renderPlot(som_modelvar1)
    
    
    output$som_modelplot2 <- renderPlot(plot(som_model, type = "count"))
    
    # create a data frame with counts of observations in each neuron
    counts_df <- as.data.frame(table(som_model$unit.classif))
    
    # plot a histogram of the counts
    som_modelvar22 <- ggplot(counts_df, aes(x = Freq)) + 
      geom_histogram() +
      xlab("Number of observations") + 
      ylab("Count") + 
      ggtitle("SOM Model Counts")
    output$som_modelplot22 <- renderPlot(som_modelvar22)
    
    output$som_modelplot3 <- renderPlot(plot(som_model, type = "dist.neighbours"))
    
    # get the distance matrix of the SOM model
    dist_matrix <- som_model$distances
    
    # create a data frame with the distances between neighboring neurons
    dist_df <- data.frame(
      distance = dist_matrix[lower.tri(dist_matrix, diag = FALSE)]
    )
    
    # plot a histogram of the distances
    som_modelvar33 <- ggplot(dist_df, aes(x = distance)) +
      geom_histogram() +
      xlab("Distance") +
      ylab("Count") +
      ggtitle("SOM Model Distance Between Neighboring Units")
    
    output$som_modelplot33 <- renderPlot(som_modelvar33)
    
    
    ######################### RANDOM FOREST #########################
    run_random_forest <- function(diabetes) {
      all_train_accuracies_rf <- numeric(100)
      all_test_accuracies_rf <- numeric(100)
      all_importances_rf <- matrix(0, nrow = 100, ncol = 8)
      
      for (split_number in c(1:100)){
        train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
        test_ind <- !train_ind 
        rf <- randomForest(as.factor(Outcome) ~ ., data = diabetes[train_ind,],ntree=100)
        train_accuracy <- sum(diag(rf$confusion))/sum(train_ind)
        cm <- table(predict(rf,diabetes[test_ind,]),diabetes$Outcome[test_ind])
        test_accuracy <- sum(diag(cm))/sum(test_ind)
        
        all_train_accuracies_rf[split_number] <- train_accuracy
        all_test_accuracies_rf[split_number] <- test_accuracy
        
        importance <- rf$importance/sum(rf$importance)
        all_importances_rf[split_number,1] <- importance["Glucose",]
        all_importances_rf[split_number,2] <- importance["BMI",]
        all_importances_rf[split_number,3] <- importance["Age",]
        all_importances_rf[split_number,4] <- importance["Insulin",]
        all_importances_rf[split_number,5] <- importance["DiabetesPedigreeFunction",]
        all_importances_rf[split_number,6] <- importance["Pregnancies",]
        all_importances_rf[split_number,7] <- importance["BloodPressure",]
        all_importances_rf[split_number,8] <- importance["SkinThickness",]
      }
      
      return(list(all_train_accuracies_rf = all_train_accuracies_rf,
                  all_test_accuracies_rf = all_test_accuracies_rf,
                  all_importances_rf = all_importances_rf))
    }
    result <- run_random_forest(diabetes)
    
    # fit random forest model
    rf <- randomForest(as.factor(Outcome) ~ ., data = diabetes, ntree = 100)
    
    # extract first tree from the random forest model
    tree_rpart <- getTree(rf, 1, labelVar = TRUE)
    
    # fit rpart model with model = TRUE
    tree_model <- rpart(as.factor(Outcome) ~ ., data = diabetes, method = "class", model = TRUE)
    
    # plot rpart tree
    rf_partplot_var <- rpart.plot(tree_model, extra = 1, under = TRUE, cex = 0.8)
    # output
    output$rf_partplot <- renderPlot(rf_partplot_var)
    
    
    # Calculate the mean importance percentages for each factor
    importance_df <- data.frame(
      factor = c("Glucose", "BMI", "Age", "Insulin", "DiabetesPedigreeFunction", "Pregnancies", "BloodPressure", "SkinThickness"),
      importance = colMeans(all_importances_rf) * 100
    )
    
    # Plot the mean importance percentages
    rf_impplot_var <- ggplot(importance_df, aes(x = factor, y = importance)) +
      geom_bar(stat = "identity", fill = "darkcyan") +
      labs(title = "Mean Importance Percentage for Each Factor",
           x = "Factor", y = "Importance Percentage") +
      theme_minimal()
    # output
    output$rf_impplot <- renderPlot(rf_impplot_var)
    
    
    ######################### K Nearest Neighbour #########################
    
    all_test_accuracies_knn <- matrix(nrow=100,ncol=9)
    
    for (split_number in c(1:100)){
      set.seed(split_number)
      train_ind <- sample(nrow(diabetes), nrow(diabetes)*0.8)
      test_ind <- setdiff(1:nrow(diabetes), train_ind)
      
      neighbors <- c(2:10)
      accuracies <- matrix(nrow=1, ncol=9)
      
      for (n_neighbors in neighbors){
        knn_fit <- knn(diabetes[train_ind,-9], diabetes[test_ind,-9], diabetes$Outcome[train_ind],k=n_neighbors)
        cm <- table(Actual = diabetes$Outcome[test_ind],Predicted = knn_fit)
        accuracy <- sum(diag(cm))/sum(test_ind)
        accuracies[n_neighbors-1] <- accuracy
      }
      all_test_accuracies_knn[split_number,] <- accuracies
    }
    
    library(tidyverse)
    # Create a data frame with accuracies and neighbors
    df <- data.frame(accuracy = c(all_test_accuracies_knn), 
                     neighbors = rep(2:10, each = 100), 
                     split_number = rep(1:100, times = 9))
    
    # Plot the data
    knn_plot_var <- ggplot(df, aes(x = neighbors, y = accuracy, group = split_number, color = factor(split_number))) + 
      geom_line() + 
      geom_point() + 
      labs(title = "KNN Accuracy vs. Number of Neighbors", 
           x = "Number of Neighbors", 
           y = "Accuracy", 
           color = "Split Number")
    # output
    output$knn_plot <- renderPlot(knn_plot_var)
    
    # hada 'numero 1' kaydir nfs l5dma dya; 'numero 2'
    # Create accuracies_df data frame
    accuracies_df <- data.frame(neighbors = neighbors, 
                                accuracy = accuracies[1,], 
                                std_dev = apply(accuracies, 2, sd))
    
    # Plot mean accuracy for each number of neighbors
    knn_accplot_var <- ggplot(accuracies_df, aes(x = neighbors, y = accuracy)) + 
      geom_line() +
      geom_errorbar(aes(ymin = accuracy - std_dev, ymax = accuracy + std_dev),
                    width = 0.2) +
      labs(title = "KNN Accuracy vs. Number of Neighbors", 
           x = "Number of Neighbors", 
           y = "Accuracy")
    # output
    output$knn_accplot <- renderPlot(knn_accplot_var)
    
    
    # hada 'numero 2' kaydir nfs l5dma dya; 'numero 1'
    # Create melted data frame
    melted_accuracies <- melt(all_test_accuracies_knn, variable.name = "Neighbors", value.name = "Accuracy")
    
    # Plot mean accuracy for each number of neighbors
    knn_prfplot_var <- ggplot(melted_accuracies, aes(x = Var1, y = Accuracy)) + 
      stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", color = "black", size = 0.5) +
      labs(title = "KNN Performance", 
           x = "Number of Neighbors", 
           y = "Accuracy")
    # output
    output$knn_prfplot <- renderPlot(knn_prfplot_var)
    
    
    ######################### LOGISTIC REGRESSION #########################
    
    all_test_accuracies_logistic <- matrix(nrow=100,ncol=1)
    for (split_number in c(1:100)){
      train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
      test_ind <- !train_ind
      
      logit_fit <- glm(Outcome ~ ., data=diabetes[train_ind,], family="binomial")
      p <- predict(logit_fit,diabetes[test_ind,],family="binomial")
      probs <- exp(p)/(1+exp(p))
      test_outcomes <- probs>0.5
      cm <- table(Actual = diabetes$Outcome[test_ind],Predicted = test_outcomes)
      accuracy <- sum(diag(cm))/sum(test_ind)
      all_test_accuracies_logistic[split_number] <- accuracy
    }
    
    # Create a data frame with the accuracy values
    df <- data.frame(Accuracy = all_test_accuracies_logistic)
    
    # Plot a histogram of the accuracy distribution
    logreg_plot_var <- ggplot(df, aes(x = Accuracy)) +
      geom_histogram(binwidth = 0.05, color = "black", fill = "lightblue") +
      labs(title = "Accuracy Distribution of Logistic Regression Model",
           x = "Accuracy", y = "Frequency")
    # output
    output$logreg_plot <- renderPlot(logreg_plot_var)
    
    ######################### Linear Discriminant Analysis (LDA) #########################
    
    # Step 3: Split the dataset into train and test subsets
    #Predicting: Let’s first add an identification number for the patients
    data = data %>% mutate(id = row_number())
    
    #Then split the dataset into training and test data.
    set.seed(11, sample.kind="Rejection")
    tr_index = sample(1:nrow(data),0.7*nrow(data), replace=FALSE)
    train=data[tr_index,]
    test=data[-tr_index,]
    
    # Step 4: Create your model with MASS library
    library(MASS)
    LDA = lda(Outcome~.,data=train)
    # LDA
    # output$lda_table <- renderTable(LDA)
    
    # Step 5: Prediction
    #Predict: We have Class, Posterior, and x.
    lda_pred = predict(LDA, newdata = test)
    names(lda_pred)
    # head(lda_pred$posterior)
    
    # Step 6: Validate your model with Confusion Matrix
    perf_indexes = function(cm){
      sensitivity = cm[2,2] / (cm[1,2] + cm[2,2])
      specificity = cm[1,1] / (cm[1,1] + cm[2,1])
      accuracy = sum(diag(cm)) / sum(cm)
      return(c(sens=sensitivity,spec=specificity,acc=accuracy))
    }
    perf_indexes(table(lda_pred$class, test$Outcome))
    
    
    # create confusion matrix
    cm <- table(lda_pred$class, test$Outcome)
    
    # calculate performance indexes
    perf <- perf_indexes(cm)
    
    
    # Create a data frame
    perf_df <- data.frame(Index=c("Sensitivity", "Specificity", "Accuracy"), 
                          Value=perf)
    
    # Create the bar plot
    lda_plot_var <- ggplot(perf_df, aes(x=Index, y=Value, fill=Index)) +
      geom_bar(stat="identity", color="black") +
      ggtitle("Performance Indexes") +
      xlab("") +
      ylab("Value") +
      theme_minimal()
    # output
    output$lda_plot <- renderPlot(lda_plot_var)
    
    
    ######### PLOTS
    
    lda_data <- data.frame(LD1 = lda_pred$x[,1], Outcome = test$Outcome)
    
    lda_histplot_var <- ggplot(lda_data, aes(x=LD1, fill=Outcome)) +
      geom_histogram(alpha=0.5, bins=20) +
      labs(title="LDA Histogram", x="LD1", y="Count", fill="Outcome")
    # output
    output$lda_histplot <- renderPlot(lda_histplot_var)
    
    
    lda_data <- data.frame(LD1 = lda_pred$x[,1], Outcome = test$Outcome)
    #lda_data <- data.frame(lda_pred$posterior, Outcome=test$Outcome)
    lda_densplot_var <- ggplot(lda_data, aes(x=LD1, fill=Outcome)) +
      geom_density(alpha=0.5) +
      labs(title="LDA Density Plot", x="LD1", y="Density", fill="Outcome")
    # output
    output$lda_densplot <- renderPlot(lda_densplot_var)
    
    
    
    
  })
}

shinyApp(ui, server)