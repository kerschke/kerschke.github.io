library(ggplot2)

df = data.frame(
  cites = c(12, 35, 62, 76, 145),
  year = 2015:2019
)

df2 = lapply(BBmisc::seq_row(df), function(i) {
  data.frame(cites = rep(df$cites[i], df$cites[i]), year = rep(df$year[i], df$cites[i]))
})
df2 = do.call(rbind, df2)

g = ggplot() + geom_bar(data = df2, mapping = aes(year), alpha = 0.4, fill = "white")
g = g + theme_minimal()
g = g + xlab("Year")
g = g + ylab("# Citations")
g = g + geom_text(data = df, mapping = aes(x = year, y = cites + 3, label = cites), size = 5L, color = "white")
g = g + ggtitle("Citations per Year", sub = "(source: Google Scholar; last updated: July 22, 2019)")
g = g + theme(
  plot.background = element_rect(fill = "#262A33"),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_blank(),
  axis.text = element_text(size = 14L, color = "white"),
  axis.title = element_text(size = 16L, color = "white"),
  plot.subtitle = element_text(hjust = 0.5, size = 10, color = "white"),
  plot.title = element_text(size = 16L, hjust = 0.5, color = "white")#, face = "bold")
)
g

ggsave(filename = "assets/images/cites.pdf", plot = g, device = cairo_pdf, width = 10L, height = 7L)
