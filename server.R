
server <- function(input, output, session) {
        bins = reactiveValues()
        labtxt = reactiveValues()

        # scatter Plot
        location_scat = reactive({
                req(input$xcol, input$ycol)
                
                location %>%
                        select(input$xcol, input$ycol)
                
        })
        
        # Reactive Data For Correlation computing
        geo_corx = reactive({
                req(input$xcol)
                
                location %>%
                        select(input$xcol)
        })
        geo_cory = reactive({
                req(input$ycol)
                
                location %>%
                        select(input$ycol)
        })
        
        # Reactive Data For geo Table
        location_table = reactive({
                req(input$geoin)
                location %>%
                        select(province, value = input$geoin) %>%
                        arrange(desc(value))
        })
        
        # Reactive Data For Line chart
        category_date_line = reactive({
                req(input$datein)
                req(input$trdcats)
                
                category_date %>%
                        select(purchase_date, input$trdcats) %>%
                        filter(purchase_date >= input$datein[1] &
                                       purchase_date <= input$datein[2])
                
        })
        
        # Reactive Data for Categories Trend table
        cat_time_table = reactive({
                req(input$datein)
                req(input$catsfortable)
                
                
                category_date %>%
                        filter(purchase_date >= input$datein[1] &
                                       purchase_date <= input$datein[2]) %>%
                        select(Date = purchase_date, input$catsfortable)
        })
        
        # Reactive Data For Bar Chart and Table
        product_category_bar = reactive({
                req(input$catvalue)
                
                product_category %>%
                        select(category, value = input$catvalue) %>%
                        filter(category %in% input$cats) %>%
                        arrange(.,desc(value))
        
                
        })
        
        # Switching labels for map
        observe({
                if (input$geoin == "sales") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Sales:</strong> $%g"
                        bins$y = c(
                                0,
                                50000,
                                100000,
                                200000,
                                300000,
                                400000,
                                1000000,
                                2000000,
                                5000000,
                                Inf
                        )
                } else if (input$geoin == "avg_shcsratio") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Ratio:</strong> %g"
                        bins$y = 9
                } else if (input$geoin == "avg_review") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Score:</strong> %g"
                        bins$y = 9
                } else if (input$geoin %in% c("avg_delidays", "avg_diffestdel")) {
                        labtxt$x = "<strong>%s</strong><br/>%g Days"
                        bins$y = 9
                } else {
                        labtxt$x = "<strong>%s</strong><br/>$%g BRL"
                        bins$y = 9
                }
                
                c = input$trdcats
                
                if (is.null(c))
                        c = character(0)
                
                updateSelectInput(
                        session,
                        "catsfortable",
                        choices = c,
                        selected = head(c, 1)
                )
                
        })
        
        
        #Graph for Map
        output$geo = renderLeaflet({
                pal = colorBin("Greens",
                               location[, input$geoin],
                               bins = bins$y,
                               pretty = F)
                
                labels = sprintf(labtxt$x,
                                 province$name,
                                 location[, input$geoin]
                ) %>% lapply(htmltools::HTML)
                
                geo = leaflet(province) %>%
                        addTiles() %>%
                        addPolygons(
                                fillColor = ~ pal(location[, input$geoin]),
                                weight = 2,
                                opacity = 1,
                                color = "white",
                                dashArray = "3",
                                fillOpacity = 0.7,
                                highlight = highlightOptions(
                                        weight = 5,
                                        color = "#666",
                                        dashArray = "",
                                        fillOpacity = 0.7,
                                        bringToFront = TRUE
                                ),
                                
                                label = labels,
                                labelOptions = labelOptions(
                                        style = list(
                                                "font-weight" = "normal",
                                                padding = "3px 8px"
                                        ),
                                        textsize = "15px",
                                        direction = "auto"
                                )
                        )
                geo %>%
                        addLegend(
                                pal = pal,
                                values = location[, input$geoin],
                                opacity = 0.7,
                                title = NULL,
                                position = "bottomright"
                        )
        })
        
        #Geo scatter plot
        output$geoscat = renderGvis({
                gvisScatterChart(
                        location_scat(),
                        options = list(
                                width = "300px",
                                height = "300px",
                                legend = "none"
                        )
                )
        })
        
        # Printing correlation
        output$cor = renderText({
                paste("Correlation:", round(cor(geo_corx(), geo_cory())[[1]], 2), sep = " ")
        })
        
        # Geo Data Output
        output$table = renderTable({
                head(location_table(), 6)
        },
        striped = T,
        spacing = 'l',
        width = '100%',
        colnames = F,
        digits = 2)
        
        
        # Trend Line Chart
        output$tim = renderGvis({
                gvisLineChart(
                        category_date_line(),
                        options = list(
                                width = "automatic",
                                height = "500px",
                                vAxis = "{title: 'Sales (in $US)', format: 'short'}",
                                hAxis = "{title: 'Date'}",
                                animation = "{startup: true}"
                        )
                )
        })
        
        # Trend table
        output$trdtable = DT::renderDataTable({
                datatable(cat_time_table(), rownames = F)
        })
        
        
        # Categories Bar Chart
        output$cat = renderGvis(gvisColumnChart(
                product_category_bar(),
                options = list(
                        width = "automatic",
                        height = "400px",
                        bar = "{groupWidth: '60%'}",
                        vAxis = "{title:'Sales (in $US)', format: 'short'}",
                        hAxis = "{title:'Categories'}",
                        animation = "{startup: true}",
                        legend = "none"
                )
        ))
        
        # Categories Table
        output$cattable = renderTable({
                head(product_category_bar(), 10)
        },
        striped = T,
        spacing = 'l',
        width = '100%',
        colnames = F)
        
        output$k_op <- renderPlot({
                plot(1:k.max, wss,type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
        })
        
        
        # graph of the cluster result
        output$k_plot <- renderPlot({
                ggplot(cluster_df, aes(x=c_lng, y=c_lat, color=clusters)) +
                        geom_point(alpha=.5)+
                        coord_cartesian(xlim = c(-70,-35), ylim = c(-35,10)) + 
                        geom_density_2d() +
                        theme_bw()
        })
        
       
        output$LT_plot <- renderPlot({
                LT_df =  LT2 %>% 
                        filter(delivered_year %in% input$locYear)%>% 
                        #filter(delivered_week %in% input$locWeek) %>% 
                        filter(s_province %in% input$locInput) %>% 
                        group_by(delivered_year,delivered_week,LT) %>% 
                        summarise(mean2 = mean(as.numeric(days))) %>%
                        ungroup()
                dD_df = merge %>% 
                        na.omit %>% 
                        filter(delivered_year %in% input$locYear) %>% 
                        group_by(delivered_year,delivered_week)%>%
                        summarise(m_POtoRDD = median(as.numeric(POtoRDD)), m_POtoDO = mean(as.numeric(POtoSO) + as.numeric(SOtoDO)))
                ggplot() +
                        geom_col(data=LT_df,aes(x=delivered_week,y=as.numeric(mean2),fill=LT))+
                        geom_line(data=dD_df,aes(x=delivered_week,y=as.numeric(m_POtoRDD)),linetype = "dashed",size = 1, label = 'POtoRDD')+
                        theme_bw() +
                        labs(title='Order Purchase to Delivery', x = 'Week Number', y = 'Number of Days (average)', color = 'Year')+ 
                        theme(legend.key=element_blank(), plot.title = element_text(size=15, face='bold',hjust = 0.5)) +
                        coord_cartesian(xlim = c(3,51))
        }, height = 500)
        
        
}
