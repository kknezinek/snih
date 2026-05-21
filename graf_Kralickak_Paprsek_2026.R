library(dplyr)
library(tidyr)
library(lubridate)


library(readxl)

df <- read_excel("C:/Users/knezinek/Documents/kraličák a paprsek.xlsx")

ts_data <- df %>%
  pivot_longer(
    cols = matches("^VAL[0-9]{2}$"),
    names_to = "DAY",
    values_to = "VALUE"
  )



ts_data <- ts_data %>%
  mutate(
    DAY = as.numeric(gsub("VAL", "", DAY)),
    
    DATE = as.Date(
      paste(YEAR, MONTH, DAY, sep = "-"),
      format = "%Y-%m-%d"
    )
  ) %>%
  filter(!is.na(DATE))

papr <- ts_data %>%
  filter(EG_GH_ID == "O4PAPR01")

kral <- ts_data %>%
  filter(EG_GH_ID == "O7KRAL01")
library(ggplot2)

ggplot(ts_data, aes(DATE, VALUE, color = EG_GH_ID)) +
  geom_line()

# přejmenování stanic
ts_data <- ts_data %>%
  mutate(
    STANICE = recode(
      EG_GH_ID,
      "O4PAPR01" = "Paprsek",
      "O7KRAL01" = "Kralický Sněžník"
    )
  )

# interaktivní graf
library(plotly)
library(dplyr)

# datum expedičního měření
exp_date <- as.Date("2026-03-03")   # uprav dle potřeby


p <- plot_ly(
  ts_data,
  x = ~DATE,
  y = ~VALUE,
  color = ~STANICE,
  colors = c("#14387F", "#4F81BD"),
  type = "scatter",
  mode = "lines"
) %>%
  layout(
    template = "plotly_white",
    
    font = list(color = "#14387F"),
    
    xaxis = list(
      title = "Datum",
      titlefont = list(color = "#14387F"),
      tickfont = list(color = "#14387F")
    ),
    
    yaxis = list(
      title = "Výška sněhu [cm]",
      titlefont = list(color = "#14387F"),
      tickfont = list(color = "#14387F")
    ),
    
    legend = list(
      title = list(text = "Stanice"),
      font = list(color = "#14387F")
    ),
    
    # červená vertikální čára
    shapes = list(
      list(
        type = "line",
        x0 = exp_date,
        x1 = exp_date,
        y0 = 0,
        y1 = 1,
        yref = "paper",
        line = list(
          color = "#E73331",
          width = 3,
          dash = "dash"
        )
      )
    ),
    
    # popisek
    annotations = list(
      list(
        x = exp_date,
        y = 1,
        yref = "paper",
        text = "Expediční měření",
        showarrow = TRUE,
        arrowhead = 2,
        ax = -120,
        ay = 10,
        font = list(
          color = "#E73331",
          size = 12
        ),
        arrowcolor = "#E73331"
      )
    )
  )

p



library(htmlwidgets)

saveWidget(
  p,
  "graf.html",
  selfcontained = TRUE
)
