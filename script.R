#load packages
pacman::p_load(tidyverse, lubridate, gridExtra, ggpubr, patchwork, gt, gtExtras, dplyr, purrr)

#read csv
data<-read.csv("full_data.csv")

#selecting country
data<-data%>%filter(location %in% c("Portugal", "Czechia", "Sweden", "Greece", "Jordan"))

#transforming to year/month/date
data$date<-ymd(data$date)

#creating breaks each month
data$month<-as.Date(cut(data$date, breaks = 'month'))

#selecting variables
data<-data%>%select(month, location, new_cases, new_deaths, total_cases, total_deaths)

#uniting country and date together
data<-data%>%unite(col = "country_date", location, month, sep = "/")

#getting new cases for each month
df_new_cases<-data%>%mutate(id=consecutive_id(country_date))%>%
  reframe(value=sum(new_cases), .by = c(id, country_date))%>%
  select(-id)

#adding new cases each month for total
df_total_cases<-data%>%mutate(id=consecutive_id(country_date))%>%
  reframe(value=(total_cases), .by = c(id, country_date))%>%
  select(-id)%>%
  group_by(country_date)%>%
  summarise_all(last)

#getting new deaths for each month
df_new_deaths<-data%>%mutate(id=consecutive_id(country_date))%>%
  reframe(value=sum(new_deaths), .by = c(id, country_date))%>%
  select(-id)

#adding new deaths each month for total
df_total_deaths<-data%>%mutate(id=consecutive_id(country_date))%>%
  reframe(value=(total_deaths), .by = c(id, country_date))%>%
  select(-id)%>%
  group_by(country_date)%>%
  summarise_all(last)

#merging dataframes together
news<-merge(df_new_cases,df_new_deaths,by="country_date")
totals<-merge(df_total_cases, df_total_deaths, by="country_date")

final_df<-merge(news, totals, by="country_date")%>%rename("new cases" = value.x.x, "new deaths" = value.y.x, "total cases"= value.x.y, "total deaths"= value.y.y)%>%
  separate(col = "country_date", into = c("country", "month"), sep = "/")

final_df$month<-ymd(final_df$month)

final_df$month<-as.Date(cut(final_df$month, breaks = 'month'))

smpl<-final_df%>%group_by(country)%>%slice_sample(n=2)
gt(smpl)%>%gt_theme_nytimes()

#function to get each country into its own dataframe
ind_country <- function(x) {
  final_df %>% filter(country == x)
}

countries <- c("Portugal", "Czechia", "Sweden", "Greece", "Jordan")

map(countries, ~{
  country_data <- ind_country(.x)
  assign(.x, country_data, envir = .GlobalEnv)
})

gt(final_df%>%group_by(country)%>%
     summarise('Total cases'=max(`total cases`), 'Total deaths'=max(`total deaths`), 'Mean cases' = mean(`new cases`),'Mean deaths'=mean(`new deaths`)))%>%
  gt_theme_nytimes()

options(scipen = 999)

ggplot(final_df, aes(x=country, y=`total cases`, fill = country))+
  geom_boxplot()+
  labs(
    title = "Distribution of COVID-19 Total Cases by Country",
    x = "Country",
    y = "Total Cases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

ggplot(final_df, aes(x=country, y=`total deaths`, fill = country))+
  geom_boxplot()+
  labs(
    title = "Distribution of COVID-19 Total Deaths by Country",
    x = "Country",
    y = "Total Deaths"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

ggplot(final_df, aes(x=country, y=`new cases`, fill = country))+
  geom_boxplot()+
  labs(
    title = "Distribution of COVID-19 New Cases by Country",
    x = "Country",
    y = "New Cases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

ggplot(final_df, aes(x=country, y=`new deaths`, fill = country))+
  geom_boxplot()+
  labs(
    title = "Distribution of COVID-19 New Deaths by Country",
    x = "Country",
    y = "New Deaths"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

line_graph<-function(n){
  final_df%>%filter(country %in% c(n))%>%
    ggplot(aes(x=month, y=`new cases`))+
    geom_line(color="blue", linewidth=2)+
    geom_point()+
    scale_x_date(date_labels = "%b-%Y", breaks = "1 month",expand = c(0,0))+
    theme(axis.text.x = element_text(size=7, angle = 90))+
    labs(x="Month", y= "New cases")+
    geom_point(color="blue", size=0.5)+
    ggtitle(n)
}

plot_graph<-function(n){
  final_df%>%filter(country %in% c(n))%>%
    ggplot(aes(x=month, y=`new deaths`))+
    geom_line(color="red", linewidth=2)+
    geom_point()+
    scale_x_date(date_labels = "%b-%Y", breaks = "1 month",expand = c(0,0))+
    theme(axis.text.x = element_text(size=7, angle = 90))+
    labs(x="Month", y= "New deaths")+
    geom_point(color="red", size=0.5)+
    ggtitle(n)
}

walk(countries, ~ {
  assign(paste0("line_", .x), line_graph(.x), envir = .GlobalEnv)
  assign(paste0("plot_", .x), plot_graph(.x), envir = .GlobalEnv)
})

# Patchwork - Displaying the plots for each country
walk(countries, ~ {
  print(get(paste0("line_", .x)) / get(paste0("plot_", .x)))
})

combined_plots <- list()

# Generate and store combined plots in the global environment
walk(countries, ~ {
  # Get the generated plots
  line_plot <- line_graph(.x)
  plot_plot <- plot_graph(.x)
  
  # Combine them using patchwork
  combined_plot <- line_plot / plot_plot
  
  # Assign each combined plot to the global environment
  assign(paste0("combined_", .x), combined_plot, envir = .GlobalEnv)
  
  # Store in a list for easy access
  combined_plots[[.x]] <- combined_plot
})

# Assign the list to the global environment
assign("all_combined_plots", combined_plots, envir = .GlobalEnv)

plot_metric <- function(metric, title) {
  ggplot(final_df, aes(x = month, y = .data[[metric]], colour = country, group = country)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    scale_x_date(date_labels = "%b-%Y", breaks = "1 month", expand = c(0, 0)) +
    theme(axis.text.x = element_text(size = 7, angle = 90)) +
    labs(x = "Date", y = metric) +
    ggtitle(title) +
    if (metric == "new cases") {
      scale_y_continuous(breaks = seq(0, 1500000, 100000))
    }
}

# Generate plots
plot_new_cases <- plot_metric("new cases", "New Cases Each Month")
plot_new_deaths <- plot_metric("new deaths", "New Deaths Each Month")

# Display the plots
plot_new_cases
plot_new_deaths