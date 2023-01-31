if(!require("librarian")){install.packages("librarian")}
librarian::shelf("plotly", "ggplot2", "gapminder", "quantmod", "reshape2", "tidyr")

tickers <- c("^GSPC", "^DJI", "^IXIC", "^NYA", "^XAX", "^RUT", "^FTSE", "IMOEX.ME", "^N225", "^GDAXI")
tickers_clean <- lapply(tickers, function(x){gsub("\\^", "", x)}) |> unlist()

data <- do.call(cbind, lapply(tickers, function(x){
  getSymbols(Symbol=x, auto.assign = F, warnings = F)[,4]
})) |> na.omit() |> `colnames<-`(tickers_clean)


cor_melted <- cor(data) |> reshape2::melt()

# Berechne Monatskorrelationen
months <- unique(format(index(data), "%Y-%m"))
monthly_cor_melted <- lapply(months, function(x) {
  month_data <- data[format(index(data), "%Y-%m") == x, ]
  melt(cor(month_data))
})

# verbinde zu einer dataframe
monthly_cor_melted <- do.call(rbind, lapply(monthly_cor_melted, function(x) x )) |> 
  mutate(month = rep(months, each=length(monthly_cor_melted[[1]]$Var1)))



p <- ggplot(monthly_cor_melted, aes(Var1, Var2, fill = value, frame=month)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", mid = "white", low = "blue", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

fig <- ggplotly(p) %>% 
  animation_opts(
    1000, redraw = T
  ) %>% 
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Month ", font = list(color="red"))
  )

fig


