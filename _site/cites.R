library(ggplot2)
library(scholar)

id = "xQKB_kEAAAAJ"
pubs = scholar:::get_publications(id)
# prof = scholar:::get_profile(id)
cites = sapply(as.character(pubs$pubid), function(pub) scholar:::get_article_cite_history(id, pub))

years = unique(unlist(cites[1,]))
cites.per.year = sapply(years, function(year) {
  sum(unlist(apply(cites, 2, function(x) x$cites[x$year == year])))
})
sum(cites.per.year)

df = data.frame(
  cites = cites.per.year,
  year = years
)

df2 = lapply(BBmisc::seq_row(df), function(i) {
  data.frame(cites = rep(df$cites[i], df$cites[i]), year = rep(df$year[i], df$cites[i]))
})
df2 = do.call(rbind, df2)

g = ggplot() + geom_bar(data = df2, mapping = aes(year), alpha = 0.6, fill = "white")
g = g + theme_minimal()
g = g + xlab("Year")
g = g + ylab("# Citations")
g = g + scale_x_continuous(breaks = sort(unique(df2$year)))
g = g + geom_text(data = df, mapping = aes(x = year, y = cites + 4, label = cites), size = 6L, color = "white")
g = g + ggtitle("Citations per Year", sub = "(source: Google Scholar; last updated: March 08, 2021)")
g = g + theme(
  plot.background = element_rect(fill = "#262A33"),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_blank(),
  axis.text = element_text(size = 16L, color = "white"),
  axis.title = element_text(size = 18L, color = "white"),
  plot.subtitle = element_text(hjust = 0.5, size = 14, color = "white"),
  plot.title = element_text(size = 18L, hjust = 0.5, color = "white")#, face = "bold")
)
g

ggsave(filename = "assets/images/cites.pdf", plot = g, device = cairo_pdf, width = 10L, height = 7L)
ggsave(filename = "assets/images/cites.png", plot = g, width = 10L, height = 7L)
