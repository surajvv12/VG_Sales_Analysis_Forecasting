function(input, output, session) {
  
  
  
  vgsales_publisher<-reactive({
    
    data<-subset(vgsales,vgsales$Publisher==input$selection)
    
    return(data)
    
  })
  
  
  #Publisher Sales
  output$bar_publisher<-renderPlot({
    
    sales_publisher<-as.data.frame(table(vgsales$Publisher))
    colnames(sales_publisher)<-c("publisher","numbers")
    sales_publisher<-sales_publisher[order(-sales_publisher$numbers),]
    top_20_sales_publisher<-head(sales_publisher,n=20)
    ggplot(top_20_sales_publisher,aes(x=reorder(publisher,numbers),y=numbers))+geom_bar(stat="identity",fill="orange")+theme_minimal()+coord_flip()+geom_text(aes(label=numbers),vjust=0.5,color="black",size=4.0)+ylab("Total Number of Sales")+xlab("Publisher")+ggtitle("Top  Selling Publishers")
    
  }) 
  
  #Video Game releases per year
  
  output$bar_year<-renderPlot({
    
    sales_year<-as.data.frame(table(vgsales$Year))
    colnames(sales_year)<-c("Year","Numbers")
    sales_year<-sales_year[-nrow(sales_year),]
    ggplot(sales_year,aes(x=Year,y=Numbers))+geom_bar(stat="identity",fill="lightgreen")+theme(axis.text=element_text(size=8))+geom_text(aes(label=Numbers),vjust=0.5,color="black",size=4.0)+ylab("Total Number of Sales")+xlab("Year")+ggtitle("Video Game Sales by Year")
    
  }) 
  
  #Video Game Revenue by Year
  output$bar_year_revenue<-renderPlot({
    
    sales_year_revenue<-as.data.frame(aggregate(vgsales$Global_Sales,by=list(Year=vgsales$Year),FUN=sum))
    colnames(sales_year_revenue)<-c("Year","Sales")
    sales_year_revenue<-sales_year_revenue[-nrow(sales_year_revenue),]
    ggplot(sales_year_revenue,aes(x=Year,y=Sales))+geom_bar(stat="identity",fill="magenta")+theme(axis.text=element_text(size=8))+geom_text(aes(label=Sales),vjust=0.5,color="black",size=4.0)+ylab("Total  Sales Revenue")+xlab("Year")+ggtitle("Video Game Sales revenue by Year")
    
  }) 
  
  
  #Platform Sales
  output$bar_platform<-renderPlot({
    
    sales_platform<-as.data.frame(table(vgsales$Platform))
    colnames(sales_platform)<-c("platform","Numbers")
    sales_platform<-sales_platform[order(-sales_platform$Numbers),]
    top_20_sales_platform<-head(sales_platform,n=20)
    ggplot(top_20_sales_platform,aes(x=reorder(platform,Numbers),y=Numbers))+geom_bar(stat="identity",fill="steelblue")+theme_minimal()+coord_flip()+geom_text(aes(label=Numbers),vjust=0.5,color="black",size=4.0)+ylab("Total Number of Sales")+xlab("Platform")+ggtitle("Top  Selling Video Game Platforms")
    
  }) 
  
  year_country_sales<-reactive({
    
    vgsales_publisher<-vgsales_publisher()
    sales_year_NA<-as.data.frame(aggregate(vgsales_publisher$NA_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    sales_year_EU<-as.data.frame(aggregate(vgsales_publisher$EU_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    sales_year_JP<-as.data.frame(aggregate(vgsales_publisher$JP_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    sales_year_Others<-as.data.frame(aggregate(vgsales_publisher$Other_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    
    
    merge1<-merge(sales_year_NA,sales_year_EU,by="Year")
    colnames(merge1)<-c("Year","NA_sales","EU_Sales")
    merge2<-merge(merge1,sales_year_JP,by="Year")
    colnames(merge2)<-c("Year","NA_sales","EU_Sales","JP_Sales")
    merge3<-merge(merge2,sales_year_Others,by="Year")
    colnames(merge3)<-c("Year","NA","EU","JP","Others")
    
    return(merge3)
    
  })
  
  #Global Top Selling Video Games
  
  output$bar_global1<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    
    ggplot(head(vgsales_publisher,n=20),aes(x=reorder(Name,Global_Sales),y=Global_Sales))+geom_bar(stat="identity",fill="steelblue")+theme_minimal()+coord_flip()+geom_text(aes(label=Global_Sales),vjust=0.5,color="black",size=4.0)+ylab("Global Sales in Millions of Dollars")+xlab("Video Game")+ggtitle("Top Global Selling Games")
    
  }) 
  
  #Top North America selling games
 
  
  output$bar_NA1<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    
    ggplot(head(vgsales_publisher,n=20),aes(x=reorder(Name,NA_Sales),y=NA_Sales))+geom_bar(stat="identity",fill="orange")+theme_minimal()+coord_flip()+geom_text(aes(label=NA_Sales),vjust=0.5,color="black",size=4.0)+ylab("North America Sales in Millions of Dollars")+xlab("Video Game")+ggtitle("Top North America Selling Games")
    
  }) 
  
  #Top Europe Selling Games
  
  
  output$bar_EU1<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    ggplot(head(vgsales_publisher,n=20),aes(x=reorder(Name,EU_Sales),y=EU_Sales))+geom_bar(stat="identity",fill="purple")+theme_minimal()+coord_flip()+geom_text(aes(label=EU_Sales),vjust=0.5,color="black",size=4.0)+ylab("Europe Sales in Millions of Dollars")+xlab("Video Game")+ggtitle("Top Europe Selling Games")
    
  }) 
  
  #Top Japan Selling Games

  output$bar_JP1<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    ggplot(head(vgsales_publisher,n=20),aes(x=reorder(Name,JP_Sales),y=JP_Sales))+geom_bar(stat="identity",fill="red")+theme_minimal()+coord_flip()+geom_text(aes(label=JP_Sales),vjust=0.5,color="black",size=4.0)+ylab("Japan Sales in Millions of Dollars")+xlab("Video Game")+ggtitle("Top Japan Selling Games")
    
  }) 
  
  #Top Japan Selling Games
  
  output$bar_OTH1<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    ggplot(head(vgsales_publisher,n=20),aes(x=reorder(Name,Other_Sales),y=Other_Sales))+geom_bar(stat="identity",fill="lightgreen")+theme_minimal()+coord_flip()+geom_text(aes(label=Other_Sales),vjust=0.5,color="black",size=4.0)+ylab("Other Countries Sales in Millions of Dollars")+xlab("Video Game")+ggtitle("Top Other Countries Selling Games")
    
  }) 
  
  #Sales by Platform
 
   #Global
 
  output$pie_global1<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    
    sales_platform_global<-as.data.frame(aggregate(vgsales_publisher$Global_Sales,by=list(Platform=vgsales_publisher$Platform),FUN=sum))
    colnames(sales_platform_global)<-c("platform","total_Sales")
    
    Pie1<-gvisPieChart(sales_platform_global,labelvar = "Platform",options = list(title="Global Sales by Platform",width=1000,height=500))
    return(Pie1)
    
  })
  
  output$bar_global2<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    sales_platform_global<-as.data.frame(aggregate(vgsales_publisher$Global_Sales,by=list(Platform=vgsales_publisher$Platform),FUN=sum))
    colnames(sales_platform_global)<-c("platform","total_Sales")
    
    ggplot(sales_platform_global,aes(x=reorder(platform,total_Sales),y=total_Sales))+geom_bar(stat="identity",fill="steelblue")+theme_minimal()+coord_flip()+geom_text(aes(label=total_Sales),vjust=0.5,color="black",size=4.0)+ylab("Global Sales in Millions of Dollars")+xlab("Platform")+ggtitle("Top Global Selling Game Platform")
    
  }) 

  #North America
  
  output$pie_NA1<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    sales_platform_NA<-as.data.frame(aggregate(vgsales_publisher$NA_Sales,by=list(Platform=vgsales_publisher$Platform),FUN=sum))
    colnames(sales_platform_NA)<-c("platform","total_Sales")
    
    Pie2<-gvisPieChart(sales_platform_NA,labelvar = "Platform",options = list(title="North America Sales by Platform",width=1000,height=500))
    return(Pie2)
    
  }) 
  
  output$bar_NA2<-renderPlot({
   
    vgsales_publisher<-vgsales_publisher()
    sales_platform_NA<-as.data.frame(aggregate(vgsales_publisher$NA_Sales,by=list(Platform=vgsales_publisher$Platform),FUN=sum))
    colnames(sales_platform_NA)<-c("platform","total_Sales")
    
    ggplot(sales_platform_NA,aes(x=reorder(platform,total_Sales),y=total_Sales))+geom_bar(stat="identity",fill="orange")+theme_minimal()+coord_flip()+geom_text(aes(label=total_Sales),vjust=0.5,color="black",size=4.0)+ylab("North America Sales in Millions")+xlab("Platform")+ggtitle("Top North America Selling Game Platform")
    
  }) 
  
  #Europe
  
  output$pie_EU1<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    
    sales_platform_EU<-as.data.frame(aggregate(vgsales_publisher$EU_Sales,by=list(Platform=vgsales_publisher$Platform),FUN=sum))
    colnames(sales_platform_EU)<-c("platform","total_Sales")
    
    Pie3<-gvisPieChart(sales_platform_EU,labelvar = "Platform",options = list(title="EUROPE Sales by Platform",width=1000,height=500))
    return(Pie3)
    
  })  
  
  output$bar_EU2<-renderPlot({
  
    vgsales_publisher<-vgsales_publisher()
    sales_platform_EU<-as.data.frame(aggregate(vgsales_publisher$EU_Sales,by=list(Platform=vgsales_publisher$Platform),FUN=sum))
    colnames(sales_platform_EU)<-c("platform","total_Sales")
    ggplot(sales_platform_EU,aes(x=reorder(platform,total_Sales),y=total_Sales))+geom_bar(stat="identity",fill="purple")+theme_minimal()+coord_flip()+geom_text(aes(label=total_Sales),vjust=0.5,color="black",size=4.0)+ylab("Europe Sales in Millions")+xlab("Platform")+ggtitle("Top Europe Selling Game Platform")
    
    
  }) 
  
  #Japan
  
  output$pie_JP1<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    
    sales_platform_JP<-as.data.frame(aggregate(vgsales_publisher$JP_Sales,by=list(Platform=vgsales_publisher$Platform),FUN=sum))
    colnames(sales_platform_JP)<-c("platform","total_Sales")
    Pie4<-gvisPieChart(sales_platform_JP,labelvar = "Platform",options = list(title="JAPAN Sales by Platform",width=1000,height=500))
    return(Pie4)
    
  })  
  
  output$bar_JP2<-renderPlot({
  
    vgsales_publisher<-vgsales_publisher()
    sales_platform_JP<-as.data.frame(aggregate(vgsales_publisher$JP_Sales,by=list(Platform=vgsales_publisher$Platform),FUN=sum))
    colnames(sales_platform_JP)<-c("platform","total_Sales")
    
    ggplot(sales_platform_JP,aes(x=reorder(platform,total_Sales),y=total_Sales))+geom_bar(stat="identity",fill="red")+theme_minimal()+coord_flip()+geom_text(aes(label=total_Sales),vjust=0.5,color="black",size=4.0)+ylab("Japan Sales in Millions")+xlab("Platform")+ggtitle("Top Japan Selling Game Platform")
    
    
  }) 
  
  
  #Others
  
  output$pie_OTH1<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    
    sales_platform_other<-as.data.frame(aggregate(vgsales_publisher$Other_Sales,by=list(Platform=vgsales_publisher$Platform),FUN=sum))
    colnames(sales_platform_other)<-c("platform","total_Sales")
    Pie5<-gvisPieChart(sales_platform_other,labelvar = "Platform",options = list(title="Other Countries Sales by Platform",width=1000,height=500))
    return(Pie5)
    
  })  
  
  output$bar_OTH2<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    sales_platform_other<-as.data.frame(aggregate(vgsales_publisher$Other_Sales,by=list(Platform=vgsales_publisher$Platform),FUN=sum))
    colnames(sales_platform_other)<-c("platform","total_Sales")
    ggplot(sales_platform_other,aes(x=reorder(platform,total_Sales),y=total_Sales))+geom_bar(stat="identity",fill="lightgreen")+theme_minimal()+coord_flip()+geom_text(aes(label=total_Sales),vjust=0.5,color="black",size=4.0)+ylab("Other Countries Sales in Millions")+xlab("Platform")+ggtitle("Top Other Countries Selling Game Platform")
    
    
  }) 
  
  
  #Sales by Genre
  
  #Global
  
  output$pie_global2<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    
    sales_genre_global<-as.data.frame(aggregate(vgsales_publisher$Global_Sales,by=list(Genre=vgsales_publisher$Genre),FUN=sum))
    colnames(sales_genre_global)<-c("genre","total_Sales")
    Pie1<-gvisPieChart(sales_genre_global,labelvar = "Genre",options = list(title="Global Sales by Genre",width=1000,height=500))
    return(Pie1)
    
  })
  
  output$bar_global3<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    
    sales_genre_global<-as.data.frame(aggregate(vgsales_publisher$Global_Sales,by=list(Genre=vgsales_publisher$Genre),FUN=sum))
    colnames(sales_genre_global)<-c("genre","total_Sales")
    ggplot(sales_genre_global,aes(x=reorder(genre,total_Sales),y=total_Sales))+geom_bar(stat="identity",fill="steelblue")+theme_minimal()+coord_flip()+geom_text(aes(label=total_Sales),vjust=0.5,color="black",size=4.0)+ylab("Global Sales in Millions of Dollars")+xlab("Genre")+ggtitle("Top Global Selling Game Genre")
    
  }) 
  
  # NA
  
  output$pie_NA2<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    
    sales_genre_NA<-as.data.frame(aggregate(vgsales_publisher$NA_Sales,by=list(Genre=vgsales_publisher$Genre),FUN=sum))
    colnames(sales_genre_NA)<-c("genre","total_Sales")
    Pie2<-gvisPieChart(sales_genre_NA,labelvar = "Genre",options = list(title="North America Sales by Genre",width=1000,height=500))
    return(Pie2)
    
  })
  
  output$bar_NA3<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    sales_genre_NA<-as.data.frame(aggregate(vgsales_publisher$NA_Sales,by=list(Genre=vgsales_publisher$Genre),FUN=sum))
    colnames(sales_genre_NA)<-c("genre","total_Sales")
    
    ggplot(sales_genre_NA,aes(x=reorder(genre,total_Sales),y=total_Sales))+geom_bar(stat="identity",fill="orange")+theme_minimal()+coord_flip()+geom_text(aes(label=total_Sales),vjust=0.5,color="black",size=4.0)+ylab("North America Sales in Millions of Dollars")+xlab("Genre")+ggtitle("Top North America Selling Game Genre")
    
  }) 
  
  # EUROPE
  
  output$pie_EU2<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    sales_genre_EU<-as.data.frame(aggregate(vgsales_publisher$EU_Sales,by=list(Genre=vgsales_publisher$Genre),FUN=sum))
    colnames(sales_genre_EU)<-c("genre","total_Sales")
    Pie3<-gvisPieChart(sales_genre_EU,labelvar = "Genre",options = list(title="Europe Sales by Genre",width=1000,height=500))
    return(Pie3)
    
  })
  
  output$bar_EU3<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    sales_genre_EU<-as.data.frame(aggregate(vgsales_publisher$EU_Sales,by=list(Genre=vgsales_publisher$Genre),FUN=sum))
    colnames(sales_genre_EU)<-c("genre","total_Sales")
    ggplot(sales_genre_EU,aes(x=reorder(genre,total_Sales),y=total_Sales))+geom_bar(stat="identity",fill="purple")+theme_minimal()+coord_flip()+geom_text(aes(label=total_Sales),vjust=0.5,color="black",size=4.0)+ylab("Europe Sales in Millions of Dollars")+xlab("Genre")+ggtitle("Top Europe Selling Game Genre")
    
  }) 
  
  # JAPAN
  
  output$pie_JP2<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    
    sales_genre_JP<-as.data.frame(aggregate(vgsales_publisher$JP_Sales,by=list(Genre=vgsales_publisher$Genre),FUN=sum))
    colnames(sales_genre_JP)<-c("genre","total_Sales")
    Pie4<-gvisPieChart(sales_genre_JP,labelvar = "Genre",options = list(title="Japan Sales by Genre",width=1000,height=500))
    return(Pie4)
    
  })
  
  output$bar_JP3<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    
    sales_genre_JP<-as.data.frame(aggregate(vgsales_publisher$JP_Sales,by=list(Genre=vgsales_publisher$Genre),FUN=sum))
    colnames(sales_genre_JP)<-c("genre","total_Sales")
    ggplot(sales_genre_JP,aes(x=reorder(genre,total_Sales),y=total_Sales))+geom_bar(stat="identity",fill="red")+theme_minimal()+coord_flip()+geom_text(aes(label=total_Sales),vjust=0.5,color="black",size=4.0)+ylab("Japan Sales in Millions of Dollars")+xlab("Genre")+ggtitle("Top Japan Selling Game Genre")
    
  }) 
  
  
  # OTHERS
  
  output$pie_OTH2<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    sales_genre_other<-as.data.frame(aggregate(vgsales_publisher$Other_Sales,by=list(Genre=vgsales_publisher$Genre),FUN=sum))
    colnames(sales_genre_other)<-c("genre","total_Sales")
    Pie5<-gvisPieChart(sales_genre_other,labelvar = "Genre",options = list(title="Other Countries Sales by Genre",width=1000,height=500))
    return(Pie5)
    
  })
  
  output$bar_OTH3<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    sales_genre_other<-as.data.frame(aggregate(vgsales_publisher$Other_Sales,by=list(Genre=vgsales_publisher$Genre),FUN=sum))
    colnames(sales_genre_other)<-c("genre","total_Sales")
    ggplot(sales_genre_other,aes(x=reorder(genre,total_Sales),y=total_Sales))+geom_bar(stat="identity",fill="lightgreen")+theme_minimal()+coord_flip()+geom_text(aes(label=total_Sales),vjust=0.5,color="black",size=4.0)+ylab("Other Countries Sales in Millions of Dollars")+xlab("Genre")+ggtitle("Top Other Countries Selling Game Genre")
    
  }) 
  
  #Sales by Year
  
  #Global
  
  output$line_global1<-renderGvis({
    
    
    vgsales_publisher<-vgsales_publisher()
    
    sales_year_global<-as.data.frame(aggregate(vgsales_publisher$Global_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    colnames(sales_year_global)<-c("Year","total_sales")
    sales_year_global<-sales_year_global[-nrow(sales_year_global),]
    line1<-gvisLineChart(sales_year_global ,options = list(title="Global Sales by Year",width=1000,height=500))
    return(line1)
    
  })
  
  #NA
  
  output$line_NA1<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    sales_year_NA<-as.data.frame(aggregate(vgsales_publisher$NA_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    colnames(sales_year_NA)<-c("Year","total_sales")
    sales_year_NA<-sales_year_NA[-nrow(sales_year_NA),]
    line2<-gvisLineChart(sales_year_NA, options = list(title="North America Sales by Year",width=1000,height=500))
    return(line2)
    
  })
  
  #Europe
  
  
  output$line_EU1<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    
    sales_year_EU<-as.data.frame(aggregate(vgsales_publisher$EU_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    colnames(sales_year_EU)<-c("Year","total_sales")
    sales_year_EU<-sales_year_EU[-nrow(sales_year_EU),]
    line3<-gvisLineChart(sales_year_EU,options = list(title="Europe Sales by Year",width=1000,height=500))
    return(line3)
    
    
  })
  
  #Japan
  
  
  output$line_JP1<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    sales_year_JP<-as.data.frame(aggregate(vgsales_publisher$JP_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    colnames(sales_year_JP)<-c("Year","total_sales")
    sales_year_JP<-sales_year_JP[-nrow(sales_year_JP),]
    line4<-gvisLineChart(sales_year_JP,options = list(title="Japan Sales by Year",width=1000,height=500))
    return(line4)
    
    
  })
  
  output$line_OTH1<-renderGvis({
    
    vgsales_publisher<-vgsales_publisher()
    sales_year_Others<-as.data.frame(aggregate(vgsales_publisher$Other_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    colnames(sales_year_Others)<-c("Year","total_sales")
    sales_year_Others<-sales_year_Others[-nrow(sales_year_Others),]
    line5<-gvisLineChart(sales_year_Others,options = list(title="Other Countries Sales by Year",width=1000,height=500))
    return(line5)
    
    
  })
  
  
  output$line_comparison<-renderGvis({
    
    year_country_sales<-year_country_sales()
    year_country_sales<-year_country_sales[-nrow(year_country_sales),]
    line<-gvisLineChart(year_country_sales,"Year",c("NA","EU","JP","Others"), options = list(title="Sales by countries per year Comparison",width=1000,height=500))
    return(line)
    
    
  })
  
  
  
 
  
  
  #Sales Forecasting
  
  # Predict Global Sales 
  
  # ARIMA approach 
  
  
  output$forecastArima<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    sales_year_global<-as.data.frame(aggregate(vgsales_publisher$Global_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    sales_year_global<- sales_year_global[-nrow( sales_year_global),]
    colnames(sales_year_global)<-c("Year","total_sales")
    sales_year_global$Year<-as.numeric(as.character(sales_year_global$Year))
    
    #Create a time series
    ts_sales<-ts(sales_year_global$total_sales,start = 1983)
    ARIMAfit<-auto.arima(ts_sales,approximation = FALSE,trace = FALSE)
    
    n.ahead=5
    
    fore<-forecast(ARIMAfit,h=n.ahead)
    predicted<-as.numeric(fore$mean)
    upper<-fore$upper[,"95%"]
    lower<-fore$lower[,"95%"]
    trend<-as.numeric(fore$fitted)
    

    
    output<-data.frame(actual=c(sales_year_global$total_sales,rep(NA,n.ahead)),
                       trend=c(trend,rep(NA,n.ahead)),
                       predicted=c(rep(NA,nrow(sales_year_global)),predicted),
                       lower=c(rep(NA,nrow(sales_year_global)),lower),
                       upper=c(rep(NA,nrow(sales_year_global)),upper),
                       year=c(sales_year_global$Year,max(sales_year_global$Year)+(1:n.ahead)))
   
    
    
    plot(output$year, output$actual, type = "l",lwd=2, col = "black", main = "Prediction of 5 Years Global Video Game Sales using ARIMA method", 
         xlab = "Time", ylab = " Total Sales in millions", xlim = range(output$year))
    grid()
    lines(output$year, output$trend, col = "red",lwd=2,lty=4)
    lines(output$year, output$predicted, col = "blue",lwd=2)
    lines(output$year, output$lower, col = "brown",lwd=2)
    lines(output$year, output$upper, col = "brown",lwd=2)
    legend("topleft", col = c("black", "red", "blue","brown"), lty = c(1,4,1,1), legend = c("Actual", "Trend", "Forecast", "Lower/Upper Bound"),cex=0.75)
    
    
  })
  
  output$ArimaPredictedTable<-renderTable({
    
    vgsales_publisher<-vgsales_publisher()
    sales_year_global<-as.data.frame(aggregate(vgsales_publisher$Global_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    sales_year_global<- sales_year_global[-nrow( sales_year_global),]
    colnames(sales_year_global)<-c("Year","total_sales")
    sales_year_global$Year<-as.numeric(as.character(sales_year_global$Year))
    
    #Create a time series
    ts_sales<-ts(sales_year_global$total_sales,start = 1983)
    ARIMAfit<-auto.arima(ts_sales,approximation = FALSE,trace = FALSE)
    
    n.ahead=5
    
    fore<-forecast(ARIMAfit,h=n.ahead)
    predicted<-as.numeric(fore$mean)
    trend<-as.numeric(fore$fitted)
    
    
    
    output<-data.frame(Year=c(sales_year_global$Year,max(sales_year_global$Year)+(1:n.ahead)),
                       Actual=c(sales_year_global$total_sales,rep(NA,n.ahead)),
                       Trend=c(trend,rep(NA,n.ahead)),
                       Predicted=c(rep(NA,nrow(sales_year_global)),predicted)
                       
                      )
    
    output$Year<-as.yearmon(output$Year)
    output_10years<-tail(output,n=10)
    row.names(output_10years)<-NULL
    return(output_10years)
    
    
    
  })
  
  #Plot ACF and PACF for residuals of ARIMA model to ensure that no more information is left to extract
 
   output$ResidualsPlot<-renderPlot({
    
    vgsales_publisher<-vgsales_publisher()
    sales_year_global<-as.data.frame(aggregate(vgsales_publisher$Global_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
    sales_year_global<- sales_year_global[-nrow( sales_year_global),]
    colnames(sales_year_global)<-c("Year","total_sales")
    sales_year_global$Year<-as.numeric(as.character(sales_year_global$Year))
    
    #Create a time series
    ts_sales<-ts(sales_year_global$total_sales,start = 1983)
    ARIMAfit<-auto.arima(ts_sales,approximation = FALSE,trace = FALSE)
    
   
    
    par(mfrow=c(1,2))
    acf(ts(ARIMAfit$residuals),main="ACF Residuals")
    pacf(ts(ARIMAfit$residuals),main="PACF Residuals")
    
    
  })
  
   
   #ETS Approach
  
   output$forecastETS<-renderPlot({
     
     vgsales_publisher<-vgsales_publisher()
     sales_year_global<-as.data.frame(aggregate(vgsales_publisher$Global_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
     sales_year_global<- sales_year_global[-nrow( sales_year_global),]
     colnames(sales_year_global)<-c("Year","total_sales")
     sales_year_global$Year<-as.numeric(as.character(sales_year_global$Year))
     
     #Create a time series
     ts_sales<-ts(sales_year_global$total_sales,start = 1983)
     ETSfit<-ets(ts_sales)
     
     n.ahead=5
     
     fore<-forecast(ETSfit,h=n.ahead)
     predicted<-as.numeric(fore$mean)
     trend<-as.numeric(fore$fitted)
     
     
     
     output<-data.frame(actual=c(sales_year_global$total_sales,rep(NA,n.ahead)),
                        trend=c(trend,rep(NA,n.ahead)),
                        predicted=c(rep(NA,nrow(sales_year_global)),predicted),
                        year=c(sales_year_global$Year,max(sales_year_global$Year)+(1:n.ahead)))
     
     
     
     plot(output$year, output$actual, type = "l",lwd=2, col = "black", main = "Prediction of 5 Years Global Video Game Sales using ETS method", 
          xlab = "Time", ylab = " Total Sales in millions", xlim = range(output$year))
     grid()
     lines(output$year, output$trend, col = "red",lwd=2,lty=4)
     lines(output$year, output$predicted, col = "blue",lwd=2)
     legend("topleft", col = c("black", "red", "blue"), lty = c(1,4,1), legend = c("Actual", "Trend", "Forecast"),cex=0.75)
     
     
   })
   
   output$ETSPredictedTable<-renderTable({
     
     vgsales_publisher<-vgsales_publisher()
     sales_year_global<-as.data.frame(aggregate(vgsales_publisher$Global_Sales,by=list(Year=vgsales_publisher$Year),FUN=sum))
     sales_year_global<- sales_year_global[-nrow( sales_year_global),]
     colnames(sales_year_global)<-c("Year","total_sales")
     sales_year_global$Year<-as.numeric(as.character(sales_year_global$Year))
     
     #Create a time series
     ts_sales<-ts(sales_year_global$total_sales,start = 1983)
     ETSfit<-ets(ts_sales)
     
     n.ahead=5
     
     fore<-forecast(ETSfit,h=n.ahead)
     predicted<-as.numeric(fore$mean)
     trend<-as.numeric(fore$fitted)
     
     
     output<-data.frame(Year=c(sales_year_global$Year,max(sales_year_global$Year)+(1:n.ahead)),
                        Actual=c(sales_year_global$total_sales,rep(NA,n.ahead)),
                        Trend=c(trend,rep(NA,n.ahead)),
                        Predicted=c(rep(NA,nrow(sales_year_global)),predicted)
                        
     )
     
     output$Year<-as.yearmon(output$Year)
     output_10years<-tail(output,n=10)
     row.names(output_10years)<-NULL
     return(output_10years)
     
     
     
   })
  
  
}