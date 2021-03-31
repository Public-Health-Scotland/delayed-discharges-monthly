#########################################################################
# Name of file - 04_knit-summary.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Original Date - December 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Knit markdown document to create summary
#########################################################################


### 0 - Load setup environment ----

source(here::here("code", "00_setup-environment.R"))

source(here("functions", "pub_date.R"))
source(here("functions", "edit_alt_text.R"))


### 1 - Create summary chart ----

chart_data <- 
  
  # Read in trend file
  read_rds(here("trend", 
                paste0(format(end_month, "%Y-%m"), "_trend.rds"))) %>%
  
  # Remove code 100's and select latest 25 months only
  filter(delay_reason_1 != 100) %>%
  filter(between(census_date, start_month - months(24), end_month)) %>%
  
  # Aggregate
  group_by(census_date) %>%
  summarise(month = as.factor(format(max(census_date), "%b %y")),
            ave_bed_days = round_half_up(sum(bed_days) / 
                                           days_in_month(max(census_date)), 0),
            .groups = "drop")

chart <-
  chart_data %>%
  ggplot(aes(x = month, y = ave_bed_days, group = 1)) +
  geom_line(color = "#3f3685", size = 1) +
  geom_point(color = "#3f3685", size = 2) +
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#d9d9d9"),
        axis.title.y = element_text(size = 10, angle = 0, 
                                    hjust = 0.5, vjust = 0.5),
        axis.text = element_text(size = 10),
        axis.text.x = 
          element_text(
            color = c("black", 
                      rep(c("transparent", "transparent", "black"), 8))),
        axis.line = element_line(),
        axis.title.x = element_blank()) +
  scale_y_continuous(
    limits = 
      c(0, ceiling(max(chart_data$ave_bed_days) / 200) * 200),
    breaks = 
      seq(0, ceiling(max(chart_data$ave_bed_days) / 200) * 200, 
          by = 200),
    expand = c(0, 0)) +
  ylab(str_wrap("Average number of beds occupied per day", width = 8))

# Save chart to output folder
ggsave(here("output", pub_date(start_month), 
            paste0(pub_date(start_month), "_twitter-chart.png")), 
       plot = chart,
       width = 6.8, height = 3.5, 
       device = "png", dpi = 600)


### 2 - Edit alt text ----

# TO DO: The following line of code will create and open a text file
# containing alt text for the chart just created. This text file should
# be edited to reflect chart, saved and closed.

print(chart)
edit_alt_text(start_month)


### 3 - Knit markdown document ----

render(
  input = here("markdown", "summary.Rmd"),
  output_file = here("output", pub_date(start_month),
                     paste0(pub_date(start_month), "_summary.docx"))
)


### END OF SCRIPT ###