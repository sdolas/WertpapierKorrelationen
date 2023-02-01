if(!require("librarian")){install.packages("librarian")}
librarian::shelf("plotly", "ggplot2", "gapminder", "quantmod", "gifski",  
                 "reshape2", "tidyr", "runner", "gridExtra", "gganimate")


cor.ts <- function(tsdata, window=30, skip=30, method="pearson"){
  #' Calculate correlation
  #'
  #' Berechne Korrelation in einem bestimmten Zeitfenster für die übergebenen Wertpapiere
  #' 
  #' @param tsdata Die Zeitreihe (zB xts oder zoo)
  #' @param window größe des Zeitfensters für die Berechnung der Korrelation
  #' @param jump übersprungene Tage nach jedem berechneten Zeitfenster
  #' @param method "pearson" (standard), "kendall" oder "spearman"
  #'
  #' @return 
  #' Output als 4-spaltige Data.frame
  #' Var1
  #' Var2
  #' value: Korrelation (standardmäßig Pearson) zwischen Var1 und Var2
  #' date: Datum
  #' 
  
  ix <- seq(index(tsdata[-(0:window),])[1], tail(index(tsdata[-(0:window),]),1), skip)
  
  cors <- lapply(ix, function(x){
    tsdata[x+seq(-window,0),] |> cor(method=method) |> melt()
  })
  
  cors <- do.call(rbind, lapply(cors, function(x) x)) |> 
    mutate(date = rep(ix, each=ncol(tsdata)^2))
  
  cors
} 

# gewünschte Tickersymbole
tickers <- c("EURUSD=X", "EURJPY=X", "EURGBP=X", "EURCAD=X", "EURSEK=X", 
             "EURCHF=X", "EURHUF=X", "EURRUB=X", "EURZAR=X", "EURMYR=X")
tickers_clean <- lapply(tickers, function(x){gsub("\\=X", "", x)}) |> unlist()

# Lade Daten von yahoo Finance in eine data.frame
data <- do.call(cbind, lapply(tickers, function(x){
  getSymbols(Symbol=x, auto.assign = F, warnings = F)[,4]
})) |> na.omit() |> `colnames<-`(tickers_clean)


# Nutze cor.ts Funktion um Korrelation zu berechnen
plotdata <- cor.ts(data, window = 90, skip=14)

# Erstelle interaktiven Plot mit ggplotly
p <- ggplot(plotdata, aes(Var1, Var2, fill=value, frame=date)) +
  geom_tile()+
  scale_fill_gradient2(high = "black", mid = "grey", low = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  labs(x="", y="")

fig <- ggplotly(p) |> 
  animation_opts(frame=150, redraw = T) |> 
  animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")  |> 
  animation_slider(currentvalue = list(prefix = "date ", font = list(color="red")))
fig

