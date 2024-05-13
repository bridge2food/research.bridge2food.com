
### Country of Residence

residence <- pbff %>%
  group_by(Country) %>%
  summarize(count=n(), .groups = 'drop')
residence$country_name <- as_factor(residence$Country)
write_csv(residence, "data_csv/demographics/residence.csv")

residence_panel <- pbff %>%
  group_by(panel_country) %>%
  summarize(count=n(), .groups = 'drop')
residence_panel$country_name <- as_factor(residence_panel$panel_country)
write_csv(residence_panel, "data_csv/demographics/residence_panel.csv")

residence_plot <- plot_ly(residence, labels = ~country_name, values = ~count, type = 'pie',
                          textinfo = 'label+percent',
                          insidetextorientation = 'radial',
                          marker = list(colors = colors_9_alt)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(residence_plot, "images/demographics/residence_plot.png", scale = 8)

### Gender

# Count the number of occurrences of each category
gender_dist <- as.data.frame(table(as_factor(pbff$Sex))) %>%
  write_csv("data_csv/demographics/gender_dist.csv")
gender_dist_panel <- as.data.frame(table(pbff$panel_sex)) %>%
  write_csv("data_csv/demographics/gender_dist_panel.csv")

gender_plot <- plot_ly(gender_dist, labels = ~Var1, values = ~Freq, type = 'pie',
                       textinfo = 'label+percent',
                       insidetextorientation = 'radial',
                       marker = list(colors = colors_3)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(gender_plot, "images/demographics/gender_plot.png", scale = 8)

### Age

age_panel <- pbff %>%
  group_by(panel_age) %>%
  summarize(count=n(), .groups = 'drop')
write_csv(age_panel, "data_csv/demographics/age_panel.csv")

age_plot <- plot_ly(x=pbff$panel_age, type = "histogram",
                    marker = list(color = colors_1)) %>%
  layout(xaxis = list(title='Age'),
         yaxis = list(title='Count'),
         bargap = 0.1) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(age_plot, "images/demographics/age_plot.png", scale = 8)

### Age groups

age_groups_data <- pbff %>%
  group_by(age_groups_2) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / nrow(pbff)) * 100)

age_groups_plot <- plot_ly(age_groups_data, x = ~as_factor(age_groups_2), y = ~percentage, type = 'bar',
                           text = ~paste0(round(percentage, 1), "%"),
                           hoverinfo = 'text',
                           marker = list(color = colors_6)) %>%
  layout(xaxis = list(title = ''),
         yaxis = list(title = ''),
         bargap = 0.2) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(age_groups_plot, "images/demographics/age_groups_plot.png", scale = 8)

### Age groups by country

age_country <- pbff %>%
  mutate(Country = as_factor(Country)) %>%
  mutate(age_groups_2 = as_factor(age_groups_2)) %>%
  group_by(Country, age_groups_2) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(Country) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

age_countries_total <- pbff %>%
  mutate(age_groups_2 = as_factor(age_groups_2)) %>%
  group_by(age_groups_2) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(Country = "Total",  # Label this group as 'Overall'
         percentage = count / sum(count) * 100) %>%
  ungroup()

age_country <- bind_rows(age_country, age_countries_total)

age_country_plot <- plot_ly(age_country, x = ~percentage, y = ~Country, 
                             type = 'bar', color = ~age_groups_2, colors = colors_5, orientation = 'h',
                             text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                             hoverinfo = 'text',  # Display text and x value on hover
                             textposition = 'inside',
                             texttemplate = '%{text}',
                             insidetextfont = list(color = 'white'),
                             insidetextanchor = 'middle',
                             marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = ''),
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(age_country_plot, "images/demographics/age_country_plot.png", scale = 8)

### Weight

# Exclude observations with weight above 250 or below 10
pbff_weight <- pbff %>%
  filter(Weight < 200 & Weight > 15)
# Convert to integer
# pbff_weight$Weight <- as.numeric(pbff_weight$Weight) %>%
 #  as.integer()

weight_plot <- plot_ly(data = pbff_weight, x = ~as_factor(Weight), color = ~as_factor(Sex), type = 'histogram',
                       colors = colors_3,
                       marker = list(line = list(color = 'white', width = 0.2)),
                       opacity = 0.7) %>%  # Correct attribute for normalization
  layout(barmode = "overlay",  # Overlay the histograms
         xaxis = list(title = "Weight"),
         yaxis = list(title = "Count")) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(weight_plot, "images/demographics/weight_plot.png", scale = 8)

### Height

# Exclude observations from chart with height above 300 and under 100
pbff_height <- pbff %>%
  filter(Height < 250 & Height > 100)

height_plot <- plot_ly(data = pbff_height, x = ~as_factor(Height), color = ~as_factor(Sex), type = 'histogram',
                       colors = colors_3,
                       marker = list(line = list(color = 'white', width = 0.2)),
                       opacity = 0.7) %>%  # Correct attribute for normalization
  layout(barmode = "overlay",  # Overlay the histograms
         xaxis = list(title = "Height"),
         yaxis = list(title = "Count")) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(height_plot, "images/demographics/height_plot.png", scale = 8)


### Education level

edu_data <- pbff %>%
  count(edu_desc) %>%
  mutate(percentage = n / sum(n) * 100)

edu_plot <- plot_ly(edu_data, x = ~edu_desc, y = ~percentage, type = "bar",
                    text = ~paste(round(percentage, 1), "%"),  # Display percentage on the bar
                    hovertext = ~paste(n, "<br>", round(percentage, 1), "%"),
                    hoverinfo = "text",  # Show custom hover text
                    marker = list(color = colors_5, line = list(color = 'white', width = 2))) %>%
  layout(xaxis = list(title = ""),
         yaxis = list(title = ""),
         bargap = 0.2) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(edu_plot, "images/demographics/edu_plot.png", scale = 8)

### SES

ses_data <- pbff %>%
  count(ses_desc) %>%
  mutate(percentage = n / sum(n) * 100)

ses_plot <- plot_ly(ses_data, x = ~ses_desc, y = ~percentage, type = "bar",
                    text = ~paste(round(percentage, 1), "%"),  # Display percentage on the bar
                    hovertext = ~paste(n, "<br>", round(percentage, 1), "%"),
                    hoverinfo = "text",  # Show custom hover text
                    marker = list(color = colors_5, line = list(color = 'rgba(255,255,255,1)', width = 2))) %>%
  layout(xaxis = list(title = ""),
         yaxis = list(title = ""),
         bargap = 0.2) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(ses_plot, "images/demographics/ses_plot.png", scale = 8)

### Nationality


### Urban, suburban, or rural

area_data <- pbff %>%
  count(area_desc) %>%
  mutate(percentage = n / sum(n) * 100)

area_plot <- plot_ly(area_data, x = ~area_desc, y = ~percentage, type = "bar",
                     text = ~paste(round(percentage, 1), "%"),  # Display percentage on the bar
                     hovertext = ~paste(n, "<br>", round(percentage, 1), "%"),
                     hoverinfo = "text",  # Show custom hover text
                     marker = list(color = colors_3, line = list(color = 'white)', width = 1))) %>%
  layout(xaxis = list(title = ""),
         yaxis = list(title = ""),
         bargap = 0.2) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(area_plot, "images/demographics/area_plot.png", scale = 8)

### Household size

household_data <- pbff %>%
  mutate(nbmembers_int = as.integer(nbmembers)) %>%  # Ensure integer values
  count(nbmembers_int) %>%
  mutate(percentage = n / sum(n) * 100)  # Calculate percentage

household_plot <- plot_ly(household_data, x = ~nbmembers_int, y = ~percentage, type = 'bar',
                          text = ~paste(round(percentage, 1), "%"),  # Text on bars
                          hoverinfo = 'text',
                          hovertext = ~paste("Count: ", n, "<br>Percentage: ", round(percentage, 1), "%"),
                          marker = list(color = colors_5, line = list(color = 'white)', width = 1))) %>%
  layout(xaxis = list(title = ""),
         yaxis = list(title = ""),
         bargap = 0.2) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(household_plot, "images/demographics/household_plot.png", scale = 8)

### Children

children_data <- pbff %>%
  mutate(nbchildren_int = as.integer(nbchildren)) %>%  # Ensure integer values
  count(nbchildren_int) %>%
  mutate(percentage = n / sum(n) * 100)  # Calculate percentage

children_plot <- plot_ly(children_data, x = ~nbchildren_int, y = ~percentage, type = 'bar',
                         text = ~paste(round(percentage, 1), "%"),  # Text on bars
                         hoverinfo = 'text',
                         hovertext = ~paste("Count: ", n, "<br>Percentage: ", round(percentage, 1), "%"),
                         marker = list(color = colors_6, line = list(color = 'white)', width = 1))) %>%
  layout(xaxis = list(title = ""),
         yaxis = list(title = ""),
         bargap = 0.2) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(children_plot, "images/demographics/children_plot.png", scale = 8)

### Responsible shopper in household?

householdresponsible_dist <- as.data.frame(table(as_factor(pbff$householdresponsible)))

householdresponsible_plot <- plot_ly(householdresponsible_dist, labels = ~Var1, values = ~Freq, type = 'pie',
                                     textinfo = 'label+percent',
                                     insidetextorientation = 'radial',
                                     marker = list(colors = colors_3)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(householdresponsible_plot, "images/demographics/householdresponsible_plot.png", scale = 8)

### Ferment

# Do you ferment?

ferment_dist <- as.data.frame(table(as_factor(pbff$q42)))

ferment_plot <- plot_ly(ferment_dist, labels = ~Var1, values = ~Freq, type = 'pie',
                       textinfo = 'label+percent',
                       insidetextorientation = 'radial',
                       marker = list(colors = colors_2)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(ferment_plot, "images/demographics/ferment_plot.png", scale = 8)

# If yes, what products?

fermenters <- pbff %>%
  filter(q42 == 1) %>%
  select(q43_1:q43_other)

sum_ferment_prod <- colSums(fermenters == 1, na.rm = T)

# Extract labels from the factors for chart labels and clean them
ferment_prod_labels <- sapply(fermenters, function(x) {
  label <- attr(x, "label")
  # remove unwanted strings
  label_cleaned <- str_replace_all(label, "\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("What kind of product\\(s\\) do you ferment\\?", "") %>%
    str_replace_all("\\(", "\n(")
})
    
ferment_prod_plot <- plot_ly(labels = ferment_prod_labels, values = sum_ferment_prod, type = 'pie',
        textinfo = 'label+percent',
        insidetextorientation = 'radial',
        hole = 0.2,
        marker = list(colors = colors_10_alt)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE) %>%
  layout(showlegend = F)
save_image(ferment_prod_plot, "images/demographics/ferment_prod_plot.png", scale = 8, width = 950)





