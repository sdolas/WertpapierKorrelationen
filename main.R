if(!require("librarian")){install.packages("librarian")}
librarian::shelf("plotly", "ggplot2", "gapminder", "quantmod", "gifski",  
                 "reshape2", "tidyr", "runner", "gridExtra", "gganimate")


cor.ts <- function(tsdata, window=30, skip=30){
  #' Calculate correlation
  #'
  #' Given a timeseries calculate the continuous correlation within a specific window
  #' 
  #' @param tsdata The time.series (e.g xts or zoo)
  #' @param window The window-size to get the correlation
  #' @param jump Days to skip after window sized cor calculated
  #'
  #' @return 
  #' 
  
  ix <- seq(index(tsdata[-(0:window),])[1], tail(index(tsdata[-(0:window),]),1), skip)
  
  cors <- lapply(ix, function(x){
    tsdata[x+seq(-window,0),] |> cor() |> melt()
  })
  
  cors <- do.call(rbind, lapply(cors, function(x) x)) |> 
    mutate(date = rep(ix, each=ncol(tsdata)^2))
  
  cors
} 


# gewünschte Tickersymbole
tickers <- c("EURUSD=X", "EURJPY=X", "EURGBP=X", "EURCAD=X", "EURSEK=X", 
             "EURCHF=X", "EURHUF=X", "EURRUB=X", "EURZAR=X", "EURMYR=X")
tickers_clean <- lapply(tickers, function(x){gsub("\\^", "", x)}) |> unlist()

# load data from yahoo finance in a merged data.frame
data <- do.call(cbind, lapply(tickers, function(x){
  getSymbols(Symbol=x, auto.assign = F, warnings = F)[,4]
})) |> na.omit() |> `colnames<-`(tickers_clean)


# get total correlation
cor_melted <- cor(data) |> reshape2::melt()

# monthly correlation
months <- unique(format(index(data), "%Y-%m"))
monthly_cor_melted <- lapply(months, function(x) {
  month_data <- data[format(index(data), "%Y-%m") == x, ]
  melt(cor(month_data))
})
monthly_cor_melted <- do.call(rbind, lapply(monthly_cor_melted, function(x) x )) |> 
  mutate(month = rep(months, each=length(monthly_cor_melted[[1]]$Var1)))



# use cor.ts function
plotdata <- cor.ts(data, window = 90, skip=14)

p <- ggplot(plotdata, aes(Var1, Var2, fill=value, frame=date)) +
  geom_tile()+
  scale_fill_gradient2(high = "black", mid = "grey", low = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

fig <- ggplotly(p) |> 
  animation_opts(frame=150, redraw = T) |> 
  animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")  |> 
  animation_slider(currentvalue = list(prefix = "date ", font = list(color="red")))
fig
anim_save("EURcor.gif", fig)
