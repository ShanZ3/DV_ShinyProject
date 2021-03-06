library(shiny)
library(shinydashboard)
library(rsconnect)
library(ggplot2)
library(googleVis)
library(tidyverse)
library(DT)
library(leaflet)
library(maps)
library(geojsonio)
library(RColorBrewer)
library(stats)
library(shinyWidgets)

ui <- fluidPage(theme = "style.css",
                shinyUI(
                        dashboardPage(
                                skin = "yellow",
                                dashboardHeader(title = "Kwai Live Commerce"),
                                dashboardSidebar(sidebarMenu(
                                        menuItem(
                                                "Introduction",
                                                tabName = "intr",
                                                icon = icon("align-justify")
                                        ),
                                        menuItem("Geographic", tabName = "location", icon = icon("map")),
                                        menuItem("Sales Trend", tabName = "date", icon = icon("line-chart")),
                                        menuItem(
                                                "Tracks",
                                                tabName = "cat",
                                                icon = icon("dashboard")
                                        ),
                                        menuItem("Conclusion", tabName = "Insights", icon = icon("globe-americas"))
                                )),
                                dashboardBody(
                                        tags$style(type = "text/css", "#location {height: calc(100vh - 80px) !important;}"),
                                        tabItems(
                                                tabItem(tabName = "intr",
                                                        fluidRow(column(
                                                                width = 12,
                                                                box(
                                                                        title = "Introduction",
                                                                        solidHeader = T,
                                                                        width = NULL,
                                                                        #status = "danger",
                                                                        id = "intro",
                                                                        
                                                                        tags$h5(tags$strong("Project Description")),
                                                                        tags$h5("With the huge increase in traffic in the e-commerce industry, more and more people start to sale their products via live streaming, blogs of expert or celebrities and lifestyle stories from short videos and photos. 
                                                                                Nevertheless, the sales and operations part of the business is still very important in the retail industry. They can be greatly improved, and has huge potential, just like demand forecasting. 
                                                                                The supply chain network as a whole is an extremely long and complex process, and saving 3-5% of transportation or inventory costs is a big deal for many companies, especially for Kwai Inc. 
                                                                                It can be seen from the reduction of physical stores that e-commerce is an inevitable choice for retail enterprises. One of the biggest differences between Amazon and other online retailers is the speed of delivery. 
                                                                                This visual dashboard provides insights into the health and performance of real-time business services, with different priorities and responsibilities for different purposes from different perspectives."),
                                                                        tags$h5(tags$i("Background knowledge from KDD Cup 2020 Challenges for Modern E-Commerce Platform ")),
                                                                        tags$h5(tags$strong("Research Questions")),
                                                                        tags$h5("Which candidate products are most popular among customers?"),
                                                                        tags$h5("Which is the most important factor to measure customer satisfaction?"),
                                                                        tags$h5("What is the average product delivery time and how can we improve it?"),
                                
                                                                        tags$h5(tags$strong("What is Kwai App?")),
                                                                        HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/prbUkLykzaE" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                                )
                                                                
                                                        )),
                                                        fluidRow(column(
                                                                width = 12,
                                                                box(
                                                                        title = "About the Dataset",
                                                                        solidHeader = T,
                                                                        width = NULL,
                                                                        collapsible = TRUE,
                                                                        #status = "danger",
                                                                        id='dataset',
                                                                        tags$h5("The dataset comes from the real-scenario multimodal data of Kwai's E-commerce."),
                                                                        tags$h5(tags$strong("Order Dataset:")),
                                                                        tags$h5("1) order_id 2) user_id 3) order_status 4) order_submit_timestamp 5) order_to_carrier_date 6) order_to_customer_date 7) estimated_delivery_date"),
                                                                        tags$h5(tags$strong("Reviews Dataset:")),
                                                                        tags$h5("1) review_id 2) order_id 3) review_score"),
                                                                        tags$h5(tags$strong("Customers Dataset:")),
                                                                        tags$h5("1) customer_id 2) customer_zip_code 3) customer_city 5) customer_province"),
                                                                        tags$h5(tags$strong("Sellers Dataset:")),
                                                                        tags$h5("1) product_id 2) product_category_name 3) product_name"),
                                                                        tags$h5(tags$strong("Geography Dataset:")),
                                                                        tags$h5("1) geolocation_zip_code_2) geolocation_lat 3) geolocation_long 4) geolocation_city 5) geolocation_province"),
                                                                        
                                                                        tags$h5("Source: Kwai App")
                                                                       
                                                                )
                                                                
                                                        )),
                                                        fluidRow(
                                                          column(
                                                            width = 12,
                                                            box(
                                                              title = "About Us", solidHeader = TRUE, 
                                                              id = "us",
                                                              status = "danger", 
                                                              width = NULL, 
                                                              collapsible = TRUE,
                                                              tags$h5(
                                                                      "     Ying Chen: ychen448@jhu.edu", br(),br(),
                                                                      "     Zeyu Chen: zchen121@jh.edu", br(),br(),
                                                                      "     Yuxin Liu: yliu387@jh.edu", br(),br(),
                                                                      "     Xiao Zhang: xzhan224@jh.edu", br(),br(),
                                                                      "     Shan Zhong: szhong11@jh.edu" )
                                                              )
                                                            
                                                          )
                                                               
                                                        )
                                                        
      
                                                ),
                                                tabItem(tabName = "location",
                                                        fluidRow(
                                                                column(
                                                                        width = 9,
                                                                        box(
                                                                                title = "Map",
                                                                                solidHeader = F,
                                                                                #status = "info",
                                                                                leafletOutput("location", height = 800),
                                                                                width = NULL,
                                                                                height = "auto"
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Select to Plot",
                                                                                solidHeader = F,
                                                                                width = NULL,
                                                                                #status = "info",
                                                                                selectizeInput("geoin", label = NULL, select1)
                                                                        ),
                                                                        box(
                                                                                id = "scatter",
                                                                                title = "Scatter Plot",
                                                                                solidHeader = F,
                                                                                collapsible = T,
                                                                                width = NULL,
                                                                                #status = "info",
                                                                                dropdownButton(
                                                                                        tags$h3("List of Input"),
                                                                                        selectInput(
                                                                                                inputId = 'xcol',
                                                                                                label = 'X Variable',
                                                                                                choices = select2,
                                                                                                selected = select3[[1]]
                                                                                        ),
                                                                                        selectInput(
                                                                                                inputId = 'ycol',
                                                                                                label = 'Y Variable',
                                                                                                choices = select3,
                                                                                                selected = select3[[1]]
                                                                                        ),
                                                                                        circle = TRUE,
                                                                                        #status = "info",
                                                                                        icon = icon("gear"),
                                                                                        width = "250px",
                                                                                        tooltip = tooltipOptions(title = "Correlations")
                                                                                ),
                                                                                htmlOutput("geoscat"),
                                                                                tags$h4(textOutput("cor"))
                                                                        ),
                                                                        box(
                                                                                title = "Data",
                                                                                solidHeader = F,
                                                                                collapsible = T,
                                                                                width = NULL,
                                                                                #status = "info",
                                                                                tableOutput({
                                                                                        "table"
                                                                                })
                                                                        )
                                                                )
                                                        )),
                                                
                                                tabItem(tabName = "date",
                                                        fluidRow(
                                                                column(
                                                                        width = 12,
                                                                        box(
                                                                                title = "Trends",
                                                                                solidHeader = F,
                                                                                width = NULL,
                                                                                height = 600,
                                                                                #status = "info",
                                                                                htmlOutput("tim")
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Date Range Input",
                                                                                solidHeader = F,
                                                                                width = NULL,
                                                                                #status = "info",
                                                                                dateRangeInput(
                                                                                        "datein",
                                                                                        label = NULL,
                                                                                        start = head(date$purchase_date, 1),
                                                                                        end = tail(date$purchase_date, 1),
                                                                                        min = head(date$purchase_date, 1),
                                                                                        max = tail(date$purchase_date, 1)
                                                                                )
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Categories To Plot",
                                                                                solidHeader = F,
                                                                                width = NULL,
                                                                                #status = "info",
                                                                                pickerInput(
                                                                                        inputId = "trdcats",
                                                                                        choices = select4,
                                                                                        selected = select4[12],
                                                                                        options = list(`actions-box` = TRUE),
                                                                                        multiple = TRUE
                                                                                )
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Category Input",
                                                                                solidHeader = F,
                                                                                width = NULL,
                                                                                #status = "info",
                                                                                selectInput("catsfortable", label = NULL, select4)
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Data",
                                                                                solidHeader = F,
                                                                                collapsible = T,
                                                                                width = NULL,
                                                                                #status = "danger",
                                                                                DT::dataTableOutput({
                                                                                        "trdtable"
                                                                                })
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 12,
                                                                        box(
                                                                                title ="Insights",
                                                                                solidHeader =F,
                                                                                width=NULL,
                                                                                tags$h5("From July 2019, to June 2020, the overall product sales has increased cumulatively over the years. 
                                                                                However, if transform the data into logarithmic scale, it can be observed that the growth was the fastest in the initial stages, with a slight boost in June 2020 but then, 
                                                                                the rate of growth slowed down. Although such trend is regular while business keep growing, it still worth to take note and try to continuously increase its conversion and sales.")
                                                                        )
                                                                )
                                                        )),
                                                tabItem(tabName = "cat",
                                                        fluidRow(
                                                                column(
                                                                        width = 12,
                                                                        box(
                                                                                title = "Bar Chart",
                                                                                solidHeader = F,
                                                                                width = NULL,
                                                                                height = 500,
                                                                                status = "danger",
                                                                                htmlOutput("cat")
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 4,
                                                                        box(
                                                                                title = "Variables Input",
                                                                                solidHeader = F,
                                                                                width = NULL,
                                                                                status = "danger",
                                                                                selectInput(
                                                                                        "catvalue",
                                                                                        label = NULL,
                                                                                        choices = select5,
                                                                                        selected = "total_sales"
                                                                                )
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 4,
                                                                        box(
                                                                                title = "Categories Input",
                                                                                solidHeader = F,
                                                                                width = NULL,
                                                                                status = "danger",
                                                                                pickerInput(
                                                                                        inputId = "cats",
                                                                                        label = NULL,
                                                                                        choices = select6,
                                                                                        selected = select6,
                                                                                        options = list(`actions-box` = TRUE),
                                                                                        multiple = TRUE
                                                                                )
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 4,
                                                                        box(
                                                                                title = "Data",
                                                                                solidHeader = F,
                                                                                collapsible = T,
                                                                                width = NULL,
                                                                                status = "danger",
                                                                                tableOutput({
                                                                                        "cattable"
                                                                                })
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 12,
                                                                        box(
                                                                                title ="Insights",
                                                                                solidHeader =F,
                                                                                width=NULL,
                                                                                status = "danger",
                                                                                tags$h5("Household are the best-selling products (in terms of sales) for Kwai's live-commerce site. Therefore, it is reasonable for Kwai to focus on further growing and milking the sales of products from this category. 
                                                                                One such way would be to increase the diversity of household products and sellers that are focused on this category. Engaging in any promotion in price might not be useful as this is already a mature category where willing users are less sensitive to price."),
                                                                               
                                                                                tags$h5("On the other hand, categories such as clothing, DIY goods and services seem to lag behind in sales. Should Kwai's ecommerce want to try and balance their sales portfolio more, marketing strategies targeted at boosting sales in these categories should be developed. 
                                                                                For example, it could consider cross selling or promotions such as free shipping vouchers which, according to industry knowledge, is the most attractive offer to users.")
                                                                        )
                                                                )
                                                                ),
                                                        fluidRow(
                                                                column(
                                                                        width=12,
                                                                        box(
                                                                                title='Lead Time',
                                                                                width = NULL,
                                                                                height = 600,
                                                                                plotOutput(outputId = "LT_plot"))
                                                                ),
                                                                column(
                                                                        width=6,
                                                                        box(
                                                                                title="List of Input",
                                                                                width = NULL,
                                                                                #pickerInput(inputId = "locWeek", "Week", choices=c(1:52), selected =c(1:52), options = list(`actions-box` = TRUE),multiple = T, width='auto'),
                                                                                pickerInput(inputId = "locInput", "Location", choices=CNprovince, selected = CNprovince, options = list(`actions-box` = TRUE),multiple = T),
                                                                                pickerInput(inputId = "locYear", "Year", choices=Years, selected = Years[2], options = list(`actions-box` = TRUE),multiple = F))
                                                                ),
                                                                column( 
                                                                        width=6,
                                                                        box(
                                                                                title='Problems',
                                                                                width = NULL,
                                                                                tags$h5( "1) Order-inventory consumption is hard to priortize since customer requested delivery date is often set too far off.",br(),br(),"2) Invertory stocking cost a lot and is vulnerable to theft due to the fact that carrier holds on to the product for too long, which might cause opportunity losses"))
                                                                ),
                                                                
                                                                column(
                                                                        width = 12,
                                                                        box(
                                                                                title ="Insights",
                                                                                solidHeader =F,
                                                                                width=NULL,
                                                                                tags$h5("Most orders are fulfilled within a week which is quite a fair delivery time for ecommerce orders. 
                                                                                        However, it should also be noted that some orders take up to or more than two weeks to be fulfilled. 
                                                                                        These would negatively affect the user experience and efforts should be carried out to improve this.")
                                                                )
                                                                )
                                                        )
                                                        
                                                ),
                                                
                                             
                                                tabItem(
                                                        tabName = "Insights",
                                                        fluidRow(
                                                                column(
                                                                        width = 12,
                                                                        box(
                                                                                title="Choosing Warehouse Location",
                                                                                solidHeader = F,
                                                                                width = NULL,
                                                                                tags$h4("If we need to choose specific city in China for locating warehouses, where would it be?"),
                                                                                tags$h5("To determine which value is the best for K of K-means, we used the elbow method to clarify fuzzy coodinate data.",br(),
                                                                               "The method takes the percentage of variance explained as a function of the number of clusters:",plotOutput(outputId = 'k_op'),
                                                                               "The within cluster variation is calculated as the sum of the euclidean distance between the data points and their respective clusters centroid",
                                                                               "adding a new cluster to the total variation within each cluster will be smaller than before and at some point the marginal gain will drop, giving an acute angle in the graph"
                                                                               )
                                                                        )
                                                                        
                                                                ),
                                                                
                                                                column( 
                                                                  width=12,
                                                                  box(
                                                                    title='Best Locations',
                                                                    width = NULL,
                                                                    tags$h5( "The graph on the left illustrates the distribution of orders across the country divided into five transportation warehouses. 
                                                                    By selecting the location of the transportation warehouse through K-mean, the average delivery time can be reduced most efficiently, thus improving user satisfaction."),
                                                                      tags$h5("The best locations in cluster 1 is Changsha from Hunan", br(),br(),
                                                                              "The best location in cluster 2 is Kunming from Yunnan", br(),br(),
                                                                              "The best location in cluster 3 is Hefei from Anhui", br(),br(),
                                                                              "The best location in cluster 4 is Jinan from Shandong", br(),br(),
                                                                              "The best location in cluster 5 are Xining from Qinghai and Huhehaote from Inner Mongolia", br()

                                                                      )
                                                                  )
                                                                  )
                                                                
                                                )  
                                                
                                        )
                                )
                        )
))
)
