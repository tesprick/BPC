rm(list = ls())
pacman::p_load(pacman, tidyverse, lubridate, fredr, extrafont, plotly, ggplot2, ggthemes, scales)

bpc_colors <- c("#3C608A", "#E43E47", "#333638", "#D3D8D6", "#2E4465", "#3687E7",
                "#321420", "#5E233B", "#F87FAB", "#EEC044", "#F6FBC2", "#DDAFEC", "#000000")

# Load data ====
setwd("C:/Users/esprick/OneDrive - Bipartisan Policy Center/Documents/R")

### Load UI handbook and FRED data
df <- read_csv("data/hbook.csv")
a12 <- read_csv("data/a12.csv")
a12$year <- ymd(sprintf("%d-01-01", a12$year))
b2 <- read_csv("data/b2.csv")
b2$year <- ymd(sprintf("%d-01-01", b2$year))
avgwks_unemp <- fredr(series_id = "UEMPMEAN", observation_start = as.Date("1960-01-01"))

### Create table of recession dates
recessions.df = read.table(textConnection(
  "Peak, Trough
  1857-06-01, 1858-12-01
  1860-10-01, 1861-06-01
  1865-04-01, 1867-12-01
  1869-06-01, 1870-12-01
  1873-10-01, 1879-03-01
  1882-03-01, 1885-05-01
  1887-03-01, 1888-04-01
  1890-07-01, 1891-05-01
  1893-01-01, 1894-06-01
  1895-12-01, 1897-06-01
  1899-06-01, 1900-12-01
  1902-09-01, 1904-08-01
  1907-05-01, 1908-06-01
  1910-01-01, 1912-01-01
  1913-01-01, 1914-12-01
  1918-08-01, 1919-03-01
  1920-01-01, 1921-07-01
  1923-05-01, 1924-07-01
  1926-10-01, 1927-11-01
  1929-08-01, 1933-03-01
  1937-05-01, 1938-06-01
  1945-02-01, 1945-10-01
  1948-11-01, 1949-10-01
  1953-07-01, 1954-05-01
  1957-08-01, 1958-04-01
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01
  2020-02-01, 2020-04-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

### Transform years to date format.

df$year <- ymd(sprintf("%d-01-01", df$year))

### Trim recession data
recessions.trim = subset(recessions.df, Peak >= "1960-01-01")

### Add state UI duration tibble

duration <- tibble(year = seq(as.Date("2011-01-01"), as.Date("2022-01-01"), "years"),
                    low = c(26, 22.17, 21.33, 20.14, 19.14, 16.00, 16.86, 18.25, 17.43, 16.40, 17.50, 17.00))

# Graph ====

## AHCM ====

ahcm <- ggplot() +
  geom_rect(data=subset(recessions.trim), aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf),
            fill=bpc_colors[4], alpha=0.4) +
  geom_line(data = b2, aes(x = year, y = ahcm,
                            col = bpc_colors[1]), size = 1.1) +
  geom_hline(yintercept = 1) +
  annotate("text", x = as.Date("2009-01-01"), y = 1.05, label = "Minimum Adequate Solvency", size = 3.5, family = "Faustina", fontface = "bold") +
  labs(title = "U.S. Average High Cost Multiple",
       caption = "Graph created by BPC using DOL data.") +
  theme_clean() +
  theme(plot.title = element_text(family = "Faustina"),
        plot.caption = element_text(family = "Faustina"),
        text = element_text(family="Faustina"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(color = "white")) +
  scale_x_date(breaks = as.Date(c("1960-01-01", "1970-01-01", "1980-01-01", "1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")), date_labels = "%Y") +
  scale_color_identity()


## Max duration and avg weeks unemployed ====

fig2 <- avgwks_unemp %>% 
  ggplot() +
  geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf),
            fill = bpc_colors[4], alpha=0.4) +
  geom_hline(yintercept = 26, col = bpc_colors[2]) +
  geom_text(x = as.Date("1995-01-01"), y = 27,
            label = "Baseline UI Duration: 26 Weeks", family = "Faustina", size = 3, col = bpc_colors[2], check_overlap = T) +
  geom_line(data = duration, aes(x = year, y = low, col = bpc_colors[10])) +
  geom_text(x = as.Date("2012-01-01"), y = 14.5, label = "Avg. Duration Among \n States Offering <26 Weeks", family = "Faustina", size = 3, col = bpc_colors[10], check_overlap = T) +
  geom_line(aes(x = date, y = value, col = bpc_colors[1]), size = 1.1) +
  labs(title = "Average Weeks Per Unemployment Spell and Differences in Maximum UI Duration",
       caption = "Graph created by BPC using DOL data.") +
  theme_clean() +
  theme(plot.title = element_text(family = "Faustina"),
        plot.caption = element_text(family = "Faustina"),
        text = element_text(family="Faustina"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(color = "white")) +
  scale_x_date(breaks = as.Date(c("1960-01-01", "1970-01-01", "1980-01-01", "1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")), date_labels = "%Y") +
  ylim(0, NA) +
  scale_color_identity()
  
  

## Recipiency rate ====

a12 <- a12 %>% filter(year < "2020-01-01")

fig3 <- ggplot() +
  geom_rect(data=subset(recessions.trim), aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf),
            fill=bpc_colors[4], alpha=0.4) +
  geom_line(data = a12, aes(x = year, y = ins_unemp,
                            col = bpc_colors[1]), size = 1.1) +
  labs(title = "UI Recipiency Rate (Annual)",
       caption = "Graph created by BPC using DOL data.") +
  theme_clean() +
  theme(plot.title = element_text(family = "Faustina"),
        plot.caption = element_text(family = "Faustina"),
        text = element_text(family="Faustina"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(color = "white")) +
  scale_color_identity() +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 60)) +
  scale_x_date(breaks = as.Date(c("1960-01-01", "1970-01-01", "1980-01-01", "1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")), date_labels = "%Y")

# SAVE =====

ggsave("UIDecline1.png", plot = ahcm, width = 11, height = 8, dpi = "retina", type = "cairo-png")
ggsave("UIDecline2.png", plot = fig2, width = 11, height = 8, dpi = "retina", type = "cairo-png")
ggsave("UIDecline3.png", plot = fig3, width = 11, height = 8, dpi = "retina", type = "cairo-png")

dev.off()