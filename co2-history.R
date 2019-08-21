# source links:
#
# 800k-1800: https://media.nature.com/original/nature-assets/nature/journal/v453/n7193/extref/nature06949-s2.xls
# 1800-1850: https://www1.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/law/law_co2.txt (75 Year Smoothed)
# 1850-1958: http://data.giss.nasa.gov/modelforce/ghgases/Fig1A.ext.txt
# 1959-today: ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_annmean_mlo.txt
#
# see also:
# - https://www.sealevel.info/co2_and_ch4.html
# - https://www.nature.com/articles/nature06949


# install.packages('needs')
# library(needs)
needs(dplyr, ggplot2, readr, grid, extrafont) # replace with `library(...)` if you don't like needs

font_import()
loadfonts(device = "postscript")

co2 <- read_tsv('co2.tsv')
family <- 'Lato'  # change to whatever font you like :)

wrp <- function(year) {
  ifelse(year < 1900, (2019-year)^warp, (2019-1900)^warp + (1900-year)*0.005)
}

geom_text_annotation <- function(label, yr, origin, offset) {
  structure(list(label=label, year=yr, origin=origin, offset=offset), class = "text_annotation")
}


warp <- 0.15

ticks <- c(-8e5,-6e5,-4e5,-3e5,-2e5,-1e5,-4e4,-2e4,-1e4,-5e3,-2e3, 0,1000,1500,1900, 2019)
labels <- sapply(ticks, function(y) {
  if (y == -8e5) {
    '800 thsd.\nyears ago\n(kya)'
  } else if (y < -2000) {
    paste0(-y/1000, 'kya')
  } else {
    y 
  }
})

ggplot_add.text_annotation <- function(object, plot, object_name) {
  numLines <- length(strsplit(object$label, '\n')[[1]] )
  l1 <- annotate('text',
                 label=object$label,
                 x=wrp(object$year)+0.02,
                 y=object$origin+object$offset,
                 size=3.7,
                 family=family,
                 lineheight = 0.8,
                 vjust=ifelse(object$offset<0,1,-1.5+numLines*0.32),
                 hjust = 0)
  l2 <- geom_segment(aes(x=wrp(object$year),
                         xend=wrp(object$year),
                         y=object$origin + object$offset + ifelse(object$origin<0,-4,+4),
                         yend=object$origin),
                     size=0.2,
                     linetype=2,
                     color='#aaaaaa')
  plot$layers <- append(plot$layers, l1)
  plot$layers <- append(plot$layers, l2)
  plot
}


fig <- co2 %>% 
  mutate(year.ago=wrp(year)) %>%
  ggplot(aes(x=year.ago, y=co2ppm)) +
  geom_line(size=0.7, color='#1d81a2') +
  scale_x_reverse(breaks=wrp(ticks), labels=labels) +
  xlab('years ago') +
  theme_minimal() +
  theme(text=element_text(size=12, family=family),
        plot.caption = element_text(lineheight = 1.2, color='#555555')) +
  xlab(NULL) +
  ylab('CO2 concentration in parts per million') +
  geom_text_annotation('First signs of\nNeanderthals', -4e5,284,20) +
  geom_text_annotation('First signs\nof modern\nhumans', -2e5, 251,10) +
  geom_text_annotation('Start of\nIce Age', -115e3, 273,10) +
  geom_text_annotation('End of\nIce Age', -11.7e3, 241,40) + 
  geom_text_annotation('First\ncivilization\n(Sumer)', -4500, 264,20) +
  geom_text_annotation('Ancient\nEgypt', -3e3, 265,-20) +
  geom_text_annotation('Roman\nEmpire', -37, 278, 10) +
  geom_text_annotation('First use of\ngunpowder\nin war', 1221, 280, 30) +
  geom_text_annotation('Industrial\nRevolution', 1800, 280, -50) +
  geom_text_annotation('Invention of\ndynamite', 1867, 290, 25) +
  geom_text_annotation('World\nWar I', 1919, 300,-40) +
  geom_text_annotation('World\nWar II', 1945, 308,-20) +
  geom_text_annotation('World\nWide\nWeb', 1989, 350,-20) +
  geom_text_annotation('Bitcoin', 2009, 386,-20) +
  annotate('text', wrp(-8e5), 400,
           hjust=0, vjust=2,size=8, parse=T,
           family=family, lineheight=1,
           label='bold("CO2 concentration in atmosphere\nover last 800,000 years")') +
  labs(lineheight=1.4,color='#999999',caption='Time is warped using sqrt scale before 1900 for readability. Graphic: Gregor Aisch, vis4.net\nSource: NOAA (1959-today, 1800-1850), NASA (1850-1958), Monnin et al., Petit et al., Siegenthaler et al., Luethi et al. (pre 1800)') 
  
print(fig)

ggsave('co2-history.png', fig, device='png', width=14,height=7, dpi=300, scale=1)

cairo_pdf("co2-history.pdf", width=14, height = 7)
print(fig)
dev.off()

