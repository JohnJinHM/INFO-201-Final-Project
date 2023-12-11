library("dplyr")
library("stringr")
library("ggplot2")
library("shiny")
library("plotly")
library("rsconnect")

df <- read.csv(paste0(getwd(),"/Datasets/MergedDataset.csv"))
econDf <- read.csv(paste0(getwd(),"/Datasets/Global_Economy_Indicators_cleaned.csv"))
addResourcePath("library", getwd())

cm_choices <- count(df, cm_name)
indicatorNames <- list("Year"="Year","Population" = "Population","mean_price"="Food Price","Gross.Domestic.Product..GDP." = "Gross Domestic Product (GDP)", "Gross.National.Income.GNI..in.USD" = "Gross National Income (GNI)", "Per.capita.GNI" = "GNI per person")

ui <- navbarPage(inverse = TRUE, "Global Food Price",
    tabPanel("Intro",
        tags$iframe(style="height:800px; width:80%", src="library/IntroductoryPage.pdf")
    ),
    tabPanel("How much food can you purchase?",
        fluidPage(
            sidebarLayout(
                sidebarPanel(
                    selectInput(inputId = "cm_name", label = "Select a commodity (Ordered according to number of observations)", choices = cm_choices[order(cm_choices$n, decreasing = TRUE),]$cm_name, selected = "Maize - Retail"),
                    wellPanel(h4("Basic Information"),
                        "This plot shows the amount of selected type of food that can be perchased with the mean gross income per person per year. Mind that the size of the points are proportional to the population (according to standard deviation) of the country, and the y-axis is in log scale."),
                    sliderInput(inputId = "yearSlider", label = "Focus on time periods", min = 1994, max = 2021, value = c(1994, 2021), step = 1),
                    wellPanel(h4("Insights on the plot"),"The plot can provide many inspiring insights.", br(), 
                        "As an example, in the year of 2008, the purchasing power around the world has significantly decreased.",br(),
                        "hint: all buttons can bw clicked twice to return to the original view.",br(),
                        actionButton("focus1", "Focus on 2008"), br(), 
                        "This was likely to be the result of the 2007-2008 financial crisis, which caused a severe global economic downturn.", br(),
                        "Althought it doesn't seem significant, remember the unit of y-axis is log10 for better view. Let's try a another view.", br(),
                        actionButton("focus2", "Changes to regular Axis"), br(), 
                        "The purchasing power of Malawi declined by almost a half, and the increasing trend for most other countries halted, many showed significant decrease.", br(),
                        "Though also, after each declines of purchasing power, we an see a significant rise. This was due to the decrease of sales forced the market to lower price of food.", br(),
                    ),
                ),
                mainPanel(
                    htmlOutput(outputId = "Page1")
                )
            )
        )
    ),
    tabPanel("What is the economic trend of the world?",
        fluidPage(
            sidebarLayout(
                sidebarPanel(
                    selectInput(inputId = "EconIndicator", label = "Select an economic indicator that you want to visualize", choices = list("GDP"="Gross.Domestic.Product..GDP.","GNI"="Gross.National.Income.GNI..in.USD","GNI per person"="Per.capita.GNI"), selected = "Gross.Domestic.Product..GDP."),
                    wellPanel(h4("Basic Information"),
                        textOutput("econSelection"),
                    ),
                    wellPanel(h4("Looks Messy?"),
                        "Due to the significant difference between population(it has already been standard-deviation scaled!) and economy betweeen countries, we are unable to see the specific values clearly. Let's try to clear them out!",br(),
                        actionButton("alter1", "Use a log scale"), br(), 
                        "Now we can clearly visualize the clearing trends. If you examine closely, there are two global recessions in 2008 and 2020. Also we can see that the economy size of most countries doubled through 1970 to 2020.",br(),
                        "However, it is still not clear enough. Let's take a look at the plot per year:",br(),
                        actionButton("alter2", "Switch to scatterplot per year"), br(), 
                        "Obviously, there is a strong positive correlation between population and GDP/GNI, and there's a moderate negative correlation between population and GNI per person.",br(),
                        htmlOutput(outputId = "page2Slider")
                    )
                ),
                mainPanel(
                    htmlOutput(outputId = "Page2")
                ),
                position = "right"
            )
        )
    ),
    tabPanel("How do this two relate?",
        fluidPage(
            sidebarLayout(
                sidebarPanel(
                    htmlOutput(outputId = "Page1Selections"),br(),
                    wellPanel( 
                        "Comparing the two plots representing food purchasing power and economy, we can see that they are obviously correlated, though the fluctuations of food price can be independent from economy trends.",br(),
                        actionButton("page3focus1", "Focus on Panama"), br(), 
                        "Take Panama as an example, in 2020 its GDP shrinked by around 24%, and the food purchasing power decreased by 20%. This is obviously the result of the COVID-19 pandemic.",br(),
                        "However, in 2011 the food purchasing power decreased by 7% while no obvious recessions appeared in the economy plot.",br(),
                        "A possible cause of this decrease would be the election of a conservative president in 2009, who's also a supermarket magnate in Panama."
                    ),
                    wellPanel(
                        "Many other trends may be found in the plots, click the following button to explore correlation between all the variables in the plot.",
                        "Note that by default outliers are removed, as there are exponential differences between data of certain countries.",br(),
                        actionButton("page3focus2", "Explore Correlations"),
                        htmlOutput(outputId = "Explore")
                    ),
                    wellPanel(
                        "In conclusion, according to my personal analysis the economic trends does have an impact on food purchasing power, but this impact is not decisive. For countries facing food depravation, economic aid might not be the most decisive factor, we should also acknowledge the impact of special situations within the country.",br(),
                        "By comparing and contrasting data of food prices and various indicators, we might be able to identify various correlations, which could potentially indicate the cause of increase/decrease of food prices - this also applies to other fields of study. Thus, we can use these data to provide insights on how to resolve global issues, and furthermore improve the daily lives of average people"
                    )
                ),
                mainPanel(
                    htmlOutput(outputId = "ExplorePlot"),
                    wellPanel(htmlOutput(outputId = "Page3Upper")),
                    wellPanel(htmlOutput(outputId = "Page3Lower"))
                ),
            )
        )
    )
)

server <- function(input, output, session) {
    output$Page1 <- renderUI({
        selectedDf <- df[df$cm_name == input$cm_name,]
        selectedDf <- selectedDf[!is.na(selectedDf$mean_price),]

        if(input$focus1 %% 2 == 1){
            selectedDf <- selectedDf[selectedDf$Year >= 2005 & selectedDf$Year <= 2011,]
        }
        else{
            selectedDf <- selectedDf[selectedDf$Year >= input$yearSlider[1] & selectedDf$Year <= input$yearSlider[2],]
        }

        sd_pop <- sd(selectedDf$Population)
        mean_pop <- mean(selectedDf$Population)
        # Remove outliers
        # iqr <- IQR(selectedDf$mean_price)
        # selectedDf <- selectedDf[selectedDf$mean_price < quantile(selectedDf$mean_price, 0.75) + 1.5 * iqr & selectedDf$mean_price > quantile(selectedDf$mean_price, 0.25) - 1.5 * iqr,]
        if(input$focus2 %% 2 == 1){
            selectedDf <- selectedDf[selectedDf$Per.capita.GNI/selectedDf$mean_price<20,] #limit for better view
            plot <- ggplot(data = selectedDf, aes(x = Year, y = Per.capita.GNI/mean_price, color = Country, Population = Population)) + geom_point(size = (selectedDf$Population-mean_pop)/sd_pop+2) + geom_line() + labs(x = "Date", y = "Price", color = "Country") + ggtitle(label = paste0(input$cm_name,"(",selectedDf$um_name[1],")"))
        }
        else{
            plot <- ggplot(data = selectedDf, aes(x = Year, y = Per.capita.GNI/mean_price, color = Country, Population = Population)) + geom_point(size = (selectedDf$Population-mean_pop)/sd_pop+2) + geom_line() + labs(x = "Date", y = "Price", color = "Country") + ggtitle(label = paste0(input$cm_name,"(",selectedDf$um_name[1],")")) + scale_y_continuous(trans='log10')
        }
        return(ggplotly(plot, tooltip = c("color", "x", "y", "Population"), height = 800))
    })

    output$Page2 <- renderUI({
        sd_pop <- sd(econDf$Population)
        mean_pop <- mean(econDf$Population)
        if(input$alter2 %% 2 == 1){
            econDf <- econDf[econDf$Year == input$yearSlider2,]
            plot <- ggplot(data=econDf,aes_string(x="Population",y=input$EconIndicator,color="Country", CountryName="Country")) + geom_point(size=1) + labs(x = "Population", y = "Indicator Value (USD)", color = "Country") + ggtitle(label = indicatorNames[[input$EconIndicator]]) + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + geom_smooth(method='lm', color = "Black", aes(CountryName = "Mean"))
            return(ggplotly(plot, tooltip = c("CountryName", "x", "y"), height = 800))
        }
        else{
            if(input$alter1 %% 2 == 1){
                plot <- ggplot(data = econDf, aes_string(x = "Year", y = input$EconIndicator, color = "Country", Population = "Population")) + geom_point(size = 0.5) + geom_line() + labs(x = "Date", y = "Indicator Value (USD)", color = "Country") + ggtitle(label = indicatorNames[[input$EconIndicator]]) + scale_y_continuous(trans='log10')
            }
            else{
                plot <- ggplot(data = econDf, aes_string(x = "Year", y = input$EconIndicator, color = "Country", Population = "Population")) + geom_point(size = (econDf$Population-mean_pop)/sd_pop+2) + geom_line() + labs(x = "Date", y = "Indicator Value (USD)", color = "Country") + ggtitle(label = indicatorNames[[input$EconIndicator]])
            }
        }
        return(ggplotly(plot, tooltip = c("color", "x", "y", "Population"), height = 800))
    })

    output$econSelection <- renderText({
        if(input$EconIndicator=="Gross.Domestic.Product..GDP.") {text <- "GDP, it represents the total value of goods produced and services provided in a country during one year. In other words, it includes all expenditures in the country's economy. Therefore it is often used to measure the strength of a country's economy."}
        else if(input$EconIndicator=="Gross.National.Income.GNI..in.USD") {text <- "GNI, it represents all income earned by a country's residents, including any income earned abroad by its citizens. It can be considered an alternative of GDP to measure economy strength."}
        else {text <- "GNI per person, it represents the average income earned by a person in a country. Generally speaking, it represents the wealth and living standard of a country's citizens. Note that it might not accurately display the true average living standard due to different commodity prices and wealth distributions."}
        paste0("You have selected ", text)
    })

    output$page2Slider <- renderUI({
        if(input$alter2 %% 2 == 1){
            return(sliderInput(inputId = "yearSlider2", label = "Focus on time periods", min = 1970, max = 2021, value = 2021, step = 1))
        }
    })

    output$Page3Upper <- renderUI({
        selectedDf <- df[df$cm_name == input$cm_name,]
        if(input$page3focus1 %% 2 == 1){
            selectedDf <- selectedDf[selectedDf$Country == "Panama",]
        }
        selectedDf <- selectedDf[selectedDf$Year >= input$yearSlider[1] & selectedDf$Year <= input$yearSlider[2],]
        selectedDf <- selectedDf[!is.na(selectedDf$mean_price),]
        sd_pop <- sd(selectedDf$Population)
        mean_pop <- mean(selectedDf$Population)
        plot <- ggplot(data = selectedDf, aes(x = Year, y = Per.capita.GNI/mean_price, color = Country, Population = Population)) + geom_point(size = 1) + geom_line() + labs(x = "Date", y = "Price", color = "Country") + ggtitle(label = paste0(input$cm_name,"(",selectedDf$um_name[1],")"))
        return(ggplotly(plot, tooltip = c("color", "x", "y", "Population"), height = 400))
    })

    output$Page3Lower <- renderUI({
        econDf <- df[df$Year >= input$yearSlider[1] & df$Year <= input$yearSlider[2],]
        if(input$page3focus1 %% 2 == 1){
            econDf <- econDf[df$Country == "Panama",]
        }
        econDf <- econDf[econDf$cm_name == input$cm_name,]
        econDf <- econDf[!is.na(econDf$mean_price),]
        sd_pop <- sd(econDf$Population)
        mean_pop <- mean(econDf$Population)
        plot <- ggplot(data = econDf, aes_string(x = "Year", y = input$EconIndicator, color = "Country", Population = "Population")) + geom_point(size = 1) + geom_line() + labs(x = "Date", y = "Indicator Value (USD)", color = "Country") + ggtitle(label = indicatorNames[[input$EconIndicator]])
        return(ggplotly(plot, tooltip = c("color", "x", "y"), height = 400))
    })

    output$Page1Selections <- renderUI({
        return(
            wellPanel(
                paste0("You have selected ", input$cm_name," from ", input$yearSlider[1], " to ", input$yearSlider[2], " at page 1, ",indicatorNames[[input$EconIndicator]], " at page 2."),
                "You can go back and edit them.",
                "(I removed the log y-axis for better comparison)."
            )
        )
    })

    observeEvent(input$page3focus2,{
        output$Explore <- renderUI({
            return(
                wellPanel(
                    selectInput(inputId = "XIndicator", label = "Select an indicator for x-axis", choices = list("Year"="Year","Food Price"="mean_price","Population"="Population","GDP"="Gross.Domestic.Product..GDP.","GNI"="Gross.National.Income.GNI..in.USD","GNI per person"="Per.capita.GNI"), selected = "Population"),
                    selectInput(inputId = "YIndicator", label = "Select an indicator for y-axis", choices = list("Year"="Year","Food Price"="mean_price","Population"="Population","GDP"="Gross.Domestic.Product..GDP.","GNI"="Gross.National.Income.GNI..in.USD","GNI per person"="Per.capita.GNI"), selected = "Gross.Domestic.Product..GDP."),
                    selectInput(inputId = "Commodity", label = "Select a commodity", choices = cm_choices[order(cm_choices$n, decreasing = TRUE),]$cm_name, selected = "Maize - Retail"),
                    sliderInput(inputId = "yearSlider3", label = "Time periods", min = 1970, max = 2021, value = c(2008,2016), step = 1),
                    checkboxInput(inputId = "logy", label = "Use log scale for y-axis", value = FALSE),
                    checkboxInput(inputId = "logx", label = "Use log scale for x-axis", value = FALSE),
                    checkboxInput(inputId = "outlier", label = "Remove outliers", value = TRUE),
                )
            )
        })

        output$ExplorePlot <- renderUI({
            if(input$XIndicator=="mean_price"|input$YIndicator=="mean_price"){
                selectedDf <- df[df$cm_name == input$Commodity,]
            }
            else{
                selectedDf <- econDf
            }
            if(!input$XIndicator=="Year"&!input$YIndicator=="Year"){
                selectedDf <- selectedDf[selectedDf$Year >= input$yearSlider3[1] & selectedDf$Year <= input$yearSlider3[2],]
            }
            if(input$outlier){
                iqrX <- IQR(selectedDf[,input$XIndicator], na.rm = TRUE)
                selectedDf <- selectedDf[selectedDf[,input$XIndicator] < quantile(selectedDf[,input$XIndicator], 0.75) + 1.5 * iqrX & selectedDf[,input$XIndicator] > quantile(selectedDf[,input$XIndicator], 0.25) - 1.5 * iqrX,]
                iqrY <- IQR(selectedDf[,input$YIndicator], na.rm = TRUE)
                selectedDf <- selectedDf[selectedDf[,input$YIndicator] < quantile(selectedDf[,input$YIndicator], 0.75) + 1.5 * iqrY & selectedDf[,input$YIndicator] > quantile(selectedDf[,input$YIndicator], 0.25) - 1.5 * iqrY,]
            }
            plot <- ggplot(data = selectedDf, aes_string(x = input$XIndicator, y = input$YIndicator, color = "Country", Population = "Population", CountryName = "Country")) + geom_point(size = 1) + labs(x = indicatorNames[[input$XIndicator]], y = indicatorNames[[input$YIndicator]], color = "Country") + ggtitle(label = paste0(input$indicatorNames[[input$XIndicator]]," vs. ",input$indicatorNames[[input$YIndicator]])) + geom_smooth(method='lm', color = "Black", aes(CountryName = "Mean"))
            if(input$logy){
                plot + scale_y_continuous(trans='log10')
            }
            if(input$logx){
                plot + scale_x_continuous(trans='log10')
            }
            return(wellPanel(ggplotly(plot, tooltip = c("color", "x", "y", "Population"), height = 800)))
        })
    })
}

library(rsconnect)

shinyApp(ui = ui, server = server)