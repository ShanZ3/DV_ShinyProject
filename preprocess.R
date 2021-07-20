
location = read.csv("location.csv", stringsAsFactors = F)
province = geojsonio::geojson_read("china.json",what='sp')
date = read.csv("date.csv", stringsAsFactors = F)
product_category = read.csv("category.csv", stringsAsFactors = F)
category_date = read.csv("category_date.csv", stringsAsFactors = F)
merge = read.csv("merge.csv", stringsAsFactors = F)

category_date$purchase_date<-as.Date(category_date$purchase_date)
date$purchase_date<-as.Date(date$purchase_date)

#China Province
as.vector(t(merge %>% distinct(s_province))) -> CNprovince
as.vector(t(merge %>% distinct(delivered_year) %>% arrange(delivered_year))) -> Years
as.vector(t(merge %>% distinct(product_category))) -> category

merge %>% 
        select('delivered_week','delivered_year','POtoSO','SOtoGI','GItoGR','s_province') %>% 
        gather('POtoSO','SOtoGI','GItoGR',key='LT',value=days )%>% 
        na.omit()  -> LT2

merge %>% 
        select(c_lat,c_lng) %>% 
        na.omit() ->cluster_df

test1<-scale(na.omit(data.matrix(cluster_df)))
data <- test1
set.seed(123)

k.max <- 10
wss <- c(235001.10, 120513.65,  71439.26,  60854.26,  41202.21,  32547.20,  27640.65,  22653.06,  20162.52,  16452.73)

# k-means 
clusters <- kmeans(cluster_df,5)
cluster_df$clusters <- as.factor(clusters$cluster)

#Variables:
select1 = list(
        "Gross Merchandise Volume" = names(location)[[2]],
        "Order Value (Avg)" = names(location)[[3]],
        "Shipping Cost (Avg)" = names(location)[[4]],
        "Delivery Days (Avg)" = names(location)[[6]],
        "Review Score (Avg)" = names(location)[[7]]
)

select2 = list(
        "Order Value (Avg)" = names(location)[[3]],
        "Shipping Cost (Avg)" = names(location)[[4]],
        "Delivery Days (Avg)" = names(location)[[6]]
)

select3 = list(
        "Review Score (Avg)" = names(location)[[7]],
        "Actual Delivery Time" = names(location)[[8]]
)

select4 = sort(colnames(category_date)[2:13])

select5 = list(
        "Gross Merchandise Value" = names(product_category)[[2]],
        "Sales per order" = names(product_category)[[3]],
        "Review Score (Avg)" = names(product_category)[[4]]
)

select6 = sort(product_category$category)


