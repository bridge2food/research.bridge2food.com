

### Dietary lifestyle

diet_lifestyle <- pbff %>%
  group_by(diet) %>%
  summarize(count=n(), .groups = 'drop')
diet_lifestyle$diet_name <- as_factor(diet_lifestyle$diet)

diet_lifestyle_plot <- plot_ly(diet_lifestyle, labels = ~diet_name, values = ~count, type = 'pie',
                               textinfo = 'label+percent',
                               insidetextorientation = 'radial',
                               texttemplate = "%{label}: %{percent:.1%}",
                               marker = list(colors = colors_5)) %>%
  layout(legend = list(traceorder = 'reversed')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
diet_lifestyle_plot_print <- diet_lifestyle_plot %>%
  style(textposition = 'inside')
save_image(diet_lifestyle_plot_print, "images/diet/diet_lifestyle_plot.png", scale = 8, width = 750)

### Diet by country

diet_country <- pbff %>%
  mutate(Country = as_factor(Country)) %>%
  mutate(diet = as_factor(diet)) %>%
  group_by(Country, diet) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(Country) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

diet_countries_total <- pbff %>%
  mutate(diet = as_factor(diet)) %>%
  group_by(diet) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(Country = "Total",  # Label this group as 'Overall'
         percentage = count / sum(count) * 100) %>%
  ungroup()

diet_country <- bind_rows(diet_country, diet_countries_total) %>%
  arrange(Country == "Total", percentage)

diet_country_plot <- plot_ly(diet_country, x = ~percentage, y = ~Country, 
                             type = 'bar', color = ~diet, colors = colors_5, orientation = 'h',
                             text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                             hoverinfo = 'text',
                             hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                             meta = ~diet,
                             textposition = 'none',
                             texttemplate = '%{text}',
                             insidetextfont = list(color = 'white'),
                             insidetextanchor = 'middle',
                             marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
diet_country_plot_print <- diet_country_plot %>%
  style(textposition = 'inside')
save_image(diet_country_plot_print, "images/diet/diet_country_plot.png", scale = 8, width = 800)

### Diet by age group

diet_age <- pbff %>%
  group_by(age_groups_2, diet) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(age_groups_2) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

diet_age_plot <- plot_ly(diet_age, x = ~percentage, y = ~as_factor(age_groups_2), 
                         type = 'bar', color = ~as_factor(diet), colors = colors_5, orientation = 'h',
                         text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                         hoverinfo = 'text',
                         hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                         meta = ~as_factor(diet),
                         textposition = 'none',
                         texttemplate = '%{text}',
                         insidetextfont = list(color = 'white'),
                         insidetextanchor = 'middle',
                         marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = ''),
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
diet_age_plot_print <- diet_age_plot %>%
  style(textposition = 'inside')
save_image(diet_age_plot_print, "images/diet/diet_age_plot.png", scale = 8, width = 800)

### Diet by education level

diet_edu <- pbff %>%
  group_by(edu_desc, diet) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(edu_desc) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

diet_edu_plot <- plot_ly(diet_edu, x = ~percentage, y = ~edu_desc, 
                         type = 'bar', color = ~as_factor(diet), colors = colors_5, orientation = 'h',
                         text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                         hoverinfo = 'text',
                         hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                         meta = ~as_factor(diet),
                         textposition = 'none',
                         texttemplate = '%{text}',
                         insidetextfont = list(color = 'white'),
                         insidetextanchor = 'middle',
                         marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = ''),
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
diet_edu_plot_print <- diet_edu_plot %>%
  style(textposition = 'inside')
save_image(diet_edu_plot_print, "images/diet/diet_edu_plot.png", scale = 8, width = 800)

### Diet by SES

diet_ses <- pbff %>%
  group_by(ses_desc, diet) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(ses_desc) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

diet_ses_plot <- plot_ly(diet_ses, x = ~percentage, y = ~ses_desc, 
                         type = 'bar', color = ~as_factor(diet), colors = colors_5, orientation = 'h',
                         text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                         hoverinfo = 'text',  # Display text and x value on hover
                         hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                         meta = ~as_factor(diet),
                         textposition = 'none',
                         texttemplate = '%{text}',
                         insidetextfont = list(color = 'white'),
                         insidetextanchor = 'middle',
                         marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = ''),
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
diet_ses_plot_print <- diet_ses_plot %>%
  style(textposition = 'inside')
save_image(diet_ses_plot_print, "images/diet/diet_ses_plot.png", scale = 8, width = 800)

### Diet reasons - importance ranking

dietimp <- pbff %>%
  mutate(across(c(starts_with("dietimp_"), "diet"), as_factor))

# Pivot data for plotting
dietimp_long <- dietimp %>%
  pivot_longer(
    cols = starts_with("dietimp_"),
    names_to = "importance_rank",
    values_to = "factor"
  ) %>%
  mutate(importance_rank = factor(str_extract(importance_rank, "\\d+"), levels = as.character(1:5))) %>%
  filter(importance_rank %in% levels(importance_rank))  # Ensure filtering is correct

# Group by 'importance_rank' and 'factor' to count occurrences and calculate percentages
dietimp_summary <- dietimp_long %>%
  group_by(factor, importance_rank) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(factor) %>%
  mutate(percentage = count / sum(count) * 100,
         # Only show percentages inside the bar for ranks 1 and 2
         text_label = ifelse(importance_rank %in% c('1', '2'), paste0(round(percentage, 1), "%"), "")) %>%
  ungroup() %>%
  arrange(importance_rank, percentage)

# Create the horizontal stacked bar chart with percentages
dietimp_plot <- plot_ly(data = dietimp_summary, x = ~percentage, y = ~factor,
                        type = 'bar', color = ~importance_rank,
                        colors = rev(colors_5),
                        orientation = 'h',
                        text = ~paste0(round(percentage, 1), "%"),
                        textposition = 'none',
                        hoverinfo = 'text',
                        hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>Rank %{meta}<extra></extra>",
                        meta = ~importance_rank,
                        insidetextanchor = 'middle',
                        insidetextfont = list(color = 'white'),
                        marker = list(line = list(color = 'white', width = 0))) %>%
  layout(margin = list(pad=4), yaxis = list(title = '', categoryorder = 'trace'),
         xaxis = list(title = '', dtick = 20),
         barmode = 'stack',
         margin = list(l = 150),  # Adjust left margin to fit labels
         legend = list(title = list(text = 'Rank'), traceorder = 'normal')) %>%  # Adding a title to the legend
  config(displayModeBar = FALSE, displaylogo = FALSE)
dietimp_plot_print <- dietimp_plot %>%
  style(textposition = 'inside')
save_image(dietimp_plot_print, "images/diet/dietimp_plot.png", scale = 8, width = 800)

### Importance by diet

dietimp <- pbff %>%
  mutate(across(c(starts_with("dietimp_"), "diet"), as_factor))

# Calculate diet type frequencies and order them by prevalence
diet_prevalence <- dietimp %>%
  count(diet) %>%
  arrange(desc(n)) %>%
  pull(diet)

# Function to generate plot for each diet type
plot_for_diet <- function(diet_type, show_legend) {
  diet_data <- dietimp %>%
    filter(diet == diet_type) %>%
    pivot_longer(
      cols = starts_with("dietimp_"),
      names_to = "importance_rank",
      values_to = "factor"
    ) %>%
    mutate(importance_rank = factor(str_extract(importance_rank, "\\d+"), levels = as.character(1:10))) %>%
    filter(importance_rank %in% as.character(1:5)) %>%
    group_by(importance_rank, factor) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(importance_rank) %>%
    mutate(percentage = round(100 * count / sum(count), 1)) %>%
    ungroup()
  
  plot_ly(diet_data, x = ~importance_rank, y = ~percentage, type = 'bar', color = ~factor,
          colors = colors_10_alt, showlegend = show_legend,
          hovertemplate = "<b>%{y}%</b> (%{meta})<br>Count: %{customdata}<extra></extra>",
          meta = ~factor, customdata = ~count) %>%
    layout(margin = list(pad=4), yaxis = list(title = paste0(diet_type)),
           xaxis = list(title = 'Importance Rank', type = 'category'))
}

# Create plots for each diet type in order of prevalence and control legend display
diet_plots <- map2(diet_prevalence, seq_along(diet_prevalence), ~plot_for_diet(.x, .y == 1))
dietimp_groups_plot <- subplot(diet_plots, nrows = length(diet_plots), shareX = TRUE, shareY = FALSE, titleX = TRUE, titleY = TRUE) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dietimp_groups_plot_print <- dietimp_groups_plot %>%
  style(textposition = 'inside')
save_image(dietimp_groups_plot_print, "images/diet/dietimp_groups_plot.png", scale = 8, width = 1200, height = 800)


### Diet time

diet_time <- pbff %>%
  count(diettime) %>%
  mutate(percentage = n/sum(n)*100)

diet_time_plot <- plot_ly(diet_time, x = ~as_factor(diettime), y = ~percentage, type = "bar",
                          text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                          hoverinfo = 'text',  # Display text and x value on hover
                          textposition = 'none',
                          texttemplate = '%{text}',
                          insidetextfont = list(color = 'white'),
                          marker = list(color = colors_5, line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%
  layout(margin = list(pad=4), yaxis = list(title = ""),
         xaxis = list(title = ""),
         barmode = 'stack',  # Stack bars to mimic histogram appearance
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
diet_time_plot_print <- diet_time_plot %>%
  style(textposition = 'inside')
save_image(diet_time_plot_print, "images/diet/diet_time_plot.png", scale = 8, width = 800)

### Diet change - which diet did you switch from?

prev_diet <- pbff %>%
  filter(diettime < 5) %>%
  count(previousdiet) %>%
  mutate(percentage = n / sum(n) * 100)

prev_diet_plot <- plot_ly(prev_diet, x = ~as_factor(previousdiet), y = ~percentage, type = "bar",
                          text = ~paste0(round(percentage, 1), "%"),  # Display percentage on the bar
                          textposition = 'none',
                          hovertext = ~paste(n, "<br>", round(percentage, 1), "%"),
                          hoverinfo = "text",  # Show custom hover text
                          marker = list(color = colors_5, line = list(color = 'white', width = 2))) %>%
  layout(margin = list(pad=4), xaxis = list(title = ""),
         yaxis = list(title = ""),
         bargap = 0.2) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
prev_diet_plot_print <- prev_diet_plot %>%
  style(textposition = 'inside')
save_image(prev_diet_plot_print, "images/diet/prev_diet_plot.png", scale = 8, width = 800)


### From omnivore - What was the main reason for the switch?

prev_diet_omn <- pbff %>%
  filter(diettime < 5) %>%
  filter(previousdiet==1) %>%
  count(reasonforswitch) %>%
  mutate(percentage = n/sum(n)*100)

pdo_num_factors <- length(unique(prev_diet_omn$reasonforswitch))
pdo_colors <- rep(colors_4, length.out = pdo_num_factors)

reasonforswitch_from_omn_plot <- plot_ly(prev_diet_omn, x = ~as_factor(reasonforswitch), y = ~percentage, type = "bar",
                                         text = ~paste0(round(percentage, 1), "%"),  # Display percentage on the bar
                                         textposition = 'none',
                                         hovertext = ~paste(n, "<br>", round(percentage, 1), "%"),
                                         hoverinfo = "text",  # Show custom hover text
                                         marker = list(color = pdo_colors, line = list(color = 'white', width = 2))) %>%
  layout(margin = list(pad=4), xaxis = list(title = ""),
         yaxis = list(title = ""),
         bargap = 0.2) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
reasonforswitch_from_omn_plot_print <- reasonforswitch_from_omn_plot %>%
  style(textposition = 'inside')
save_image(reasonforswitch_from_omn_plot_print, "images/diet/reasonforswitch_from_omn_plot.png", scale = 8, width = 800)

### Frequency of consumption

fc_data <- pbff %>%
  select(starts_with("FC_"))

for (col_name in names(fc_data)) {
  label <- attr(fc_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(fc_data)[names(fc_data) == col_name] <- label
  }
}

fc_data_long <- fc_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
fc_data_long$value_desc <- as_factor(fc_data_long$value)
fc_data_long$variable <- fc_data_long$variable %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("Which statement best describes how frequently you have consumed the following foods in the last 12 months\\?", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  trimws()

# Aggregate and calculate percentages
fc_data_agg <- fc_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
fc_plot <- plot_ly(fc_data_agg, x = ~percentage, y = ~variable,
                   type = 'bar', color = ~value_desc, colors = colors_7,
                   orientation = 'h',
                   text = ~paste0(round(percentage, 1), "%"),
                   hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                   meta = ~value_desc,
                   textposition = 'none',
                   insidetextanchor = 'middle',
                   insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace")) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
fc_plot_print <- fc_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.225, xanchor = 'auto', y = 1.1, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(fc_plot_print, "images/diet/fc_plot.png", scale = 8, width = 1200, height = 800)


#### By Country (Dropdown menu) for FC data

fc_data_dd <- pbff %>%
  select(starts_with("FC_"))

for (col_name in names(fc_data_dd)) {
  label <- attr(fc_data_dd[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(fc_data_dd)[names(fc_data_dd) == col_name] <- label
  }
}

fc_data_dd_long <- fc_data_dd %>%
  mutate(id = row_number(), Country = as_factor(pbff$Country)) %>%
  pivot_longer(cols = -c(id, Country), names_to = "variable", values_to = "value")

# Get labels and clean them
fc_data_dd_long$value_desc <- as_factor(fc_data_dd_long$value)
fc_data_dd_long$variable <- fc_data_dd_long$variable %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("Which statement best describes how frequently you have consumed the following foods in the last 12 months\\?", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  trimws()

# Function to aggregate and calculate percentages
aggregate_data <- function(data) {
  data %>%
    group_by(variable, value, value_desc, Country) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(variable, Country) %>%
    mutate(percentage = count / sum(count) * 100,
           text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
    arrange(desc(value), desc(percentage))
}

# Aggregate the data
fc_data_dd_agg <- aggregate_data(fc_data_dd_long)

# Summarize data for 'All'
fc_all_data <- fc_data_dd_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), ""),
         Country = "All")

fc_data_combined <- bind_rows(fc_data_dd_agg, fc_all_data) %>%
  arrange(desc(value), desc(percentage))

# List of countries including "All"
countries <- c("All", levels(fc_data_dd_long$Country))

# Create traces for each country including "All"
traces <- list()
for (i in seq_along(countries)) {
  country <- countries[i]
  country_data <- fc_data_combined %>% filter(Country == country)
  
  for (value_desc in levels(fc_data_dd_long$value_desc)) {
    value_data <- country_data %>% filter(value_desc == !!value_desc)
    if (nrow(value_data) == 0) next
    
    value_index <- (match(value_desc, levels(fc_data_dd_long$value_desc)) - 1) %% length(colors_7) + 1
    color <- colors_7[value_index]
    
    trace <- list(
      x = value_data$percentage,
      y = value_data$variable,
      type = 'bar',
      orientation = 'h',
      name = as.character(value_desc),
      marker = list(color = color),
      text = value_data$text_label,
      textposition = 'none',
      insidetextanchor = 'middle',
      insidetextfont = list(color = 'white'),
      hoverinfo = 'text',
      hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
      meta = value_data$value_desc,
      visible = ifelse(i == 1, TRUE, FALSE) # Only the first trace is visible initially
    )
    
    traces <- append(traces, list(trace))
  }
}

# Create dropdown buttons for each country including "All"
dropdown_buttons <- lapply(seq_along(countries), function(i) {
  list(
    method = "update",
    args = list(list(visible = rep(i == seq_along(countries), each = length(levels(fc_data_dd_long$value_desc))))),
    label = countries[i]
  )
})

# Create the plot
fc_plot_dd <- plot_ly()

for (trace in traces) {
  fc_plot_dd <- add_trace(fc_plot_dd, x = trace$x, y = trace$y, type = trace$type, orientation = trace$orientation,
                          marker = trace$marker, name = trace$name, text = trace$text, textposition = trace$textposition,
                          insidetextanchor = trace$insidetextanchor, insidetextfont = trace$insidetextfont,
                          hoverinfo = trace$hoverinfo, hovertemplate = trace$hovertemplate, meta = trace$meta,
                          visible = trace$visible)
}

fc_plot_dd <- fc_plot_dd %>%
  layout(
    barmode = 'stack',
    xaxis = list(title = ""),
    yaxis = list(title = "", categoryorder = "trace"),
    updatemenus = list(list(
      active = 0,
      buttons = dropdown_buttons,
      x = 0.5, # Center horizontally
      xanchor = 'center', # Anchor to the center
      y = 1.2, # Place above the plot
      yanchor = 'top' # Anchor to the top
    )),
    margin = list(pad = 4)
  ) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)


### Delta meat consumption

# Convert to factor
dm <- pbff
if (!inherits(dm$deltameatconsumpt, "factor")) {
  dm$deltameat <- as_factor(dm$deltameatconsumpt)
}

# Add a level for NA
deltameat_levels_with_na <- c(levels(dm$deltameat), "NA")
dm$deltameat <- factor(dm$deltameat, levels = deltameat_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dm$deltameat[is.na(dm$deltameat)] <- "NA"

dm <- dm %>%
  filter(deltameat != "NA") %>%
  count(deltameat) %>%
  mutate(percentage = n/sum(n)*100)

deltameat_plot <- plot_ly(dm, x = ~as_factor(deltameat), y = ~percentage, type = "bar",
                          text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                          hoverinfo = 'text',  # Display text and x value on hover
                          textposition = 'none',
                          texttemplate = '%{text}',
                          insidetextfont = list(color = 'white'),
                          marker = list(color = colors_5, line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%
  layout(margin = list(pad=4), yaxis = list(title = ""),
         xaxis = list(title = ""),
         barmode = 'stack',  # Stack bars to mimic histogram appearance
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
deltameat_plot_print <- deltameat_plot %>%
  style(textposition = 'inside')
save_image(deltameat_plot_print, "images/diet/deltameat_plot.png", scale = 8, width = 800)

### Delta meat consumption by diet 

# Convert to factor
dm_diet <- pbff
if (!inherits(dm_diet$deltameatconsumpt, "factor")) {
  dm_diet$deltameat <- as_factor(dm_diet$deltameatconsumpt)
}

# Add a level for NA
deltameat_levels_with_na <- c(levels(dm_diet$deltameat), "NA")
dm_diet$deltameat <- factor(dm_diet$deltameat, levels = deltameat_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dm_diet$deltameat[is.na(dm_diet$deltameat)] <- "NA"

dm_omn <- dm_diet %>%
  filter(deltameat != "NA") %>%
  filter(diet == 1) %>%
  count(deltameat) %>%
  mutate(percentage = n/sum(n)*100)

dm_flx <- dm_diet %>%
  filter(deltameat != "NA") %>%
  filter(diet == 2) %>%
  count(deltameat) %>%
  mutate(percentage = n/sum(n)*100)

dm_omn <- dm_omn %>%
  mutate(diet_group = "Omnivore")
dm_flx <- dm_flx %>%
  mutate(diet_group = "Flexitarian")

dm_diet_data <- bind_rows(dm_omn, dm_flx)

dm_diet_plot <- plot_ly(dm_diet_data, x = ~as_factor(deltameat), y = ~percentage,
                         type = "bar", color = ~diet_group, colors = colors_2,
                         text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                         hoverinfo = 'text',  # Display text and x value on hover
                         textposition = 'none',
                         texttemplate = '%{text}',
                         insidetextfont = list(color = 'white')) %>%
  layout(barmode = 'group',  # Group bars side-by-side
         margin = list(pad = 4), yaxis = list(title = ""),
         xaxis = list(title = ""),
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dm_diet_plot_print <- dm_diet_plot %>%
  style(textposition = 'inside')
save_image(dm_diet_plot_print, "images/diet/dm_diet_plot.png", scale = 8, width = 800)


### Delta meat by country

# Convert to factor
dm_country <- pbff
if (!inherits(dm_country$deltameatconsumpt, "factor")) {
  dm_country$deltameat <- as_factor(dm_country$deltameatconsumpt)
}

# Add a level for NA
dm_country_levels_with_na <- c(levels(dm_country$deltameat), "NA")
dm_country$deltameat <- factor(dm_country$deltameat, levels = dm_country_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dm_country$deltameat[is.na(dm_country$deltameat)] <- "NA"

dm_countries <- dm_country %>%
  filter(deltameat != "NA") %>%
  mutate(Country = as_factor(Country)) %>%
  mutate(deltameat = as_factor(deltameat)) %>%
  group_by(Country, deltameat) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(Country) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dm_countries_total <- dm_country %>%
  filter(deltameat != "NA") %>%
  mutate(deltameat = as_factor(deltameat)) %>%
  group_by(deltameat) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(Country = "Total", 
         percentage = count / sum(count) * 100) %>%
  ungroup()

dm_countries <- bind_rows(dm_countries, dm_countries_total) %>%
  mutate(Country = fct_relevel(Country, "Total", after = 0)) %>%
  arrange(Country) %>%
  arrange(Country == "Total", percentage)

dm_countries_plot <- plot_ly(dm_countries, x = ~percentage, y = ~Country, 
                             type = 'bar', color = ~deltameat, colors = rev(colors_5), orientation = 'h',
                             text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                             hoverinfo = 'text',
                             hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                             meta = ~deltameat,
                             textposition = 'none',
                             texttemplate = '%{text}',
                             insidetextfont = list(color = 'white'),
                             insidetextanchor = 'middle',
                             marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dm_countries_plot_print <- dm_countries_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = 0.5, xanchor = 'auto', y = 1.13, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dm_countries_plot_print, "images/diet/dm_countries_plot.png", scale = 8, width = 800)


### Delta meat by education level

# Convert to factor
dm_edu <- pbff
if (!inherits(dm_edu$deltameatconsumpt, "factor")) {
  dm_edu$deltameat <- as_factor(dm_edu$deltameatconsumpt)
}

# Add a level for NA
dm_edu_levels_with_na <- c(levels(dm_edu$deltameat), "NA")
dm_edu$deltameat <- factor(dm_edu$deltameat, levels = dm_edu_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dm_edu$deltameat[is.na(dm_edu$deltameat)] <- "NA"

dm_edu_levels <- dm_edu %>%
  filter(deltameat != "NA") %>%
  mutate(edu_desc = as_factor(edu_desc)) %>%
  mutate(deltameat = as_factor(deltameat)) %>%
  group_by(edu_desc, deltameat) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(edu_desc) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dm_edu_levels_total <- dm_edu %>%
  filter(deltameat != "NA") %>%
  mutate(deltameat = as_factor(deltameat)) %>%
  group_by(deltameat) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(edu_desc = "Total", 
         percentage = count / sum(count) * 100) %>%
  ungroup()

dm_edu_levels <- bind_rows(dm_edu_levels, dm_edu_levels_total) %>%
  mutate(edu_desc = fct_relevel(edu_desc, "Total", after = 0)) %>%
  arrange(edu_desc == "Total")

dm_edu_levels_plot <- plot_ly(dm_edu_levels, x = ~percentage, y = ~edu_desc, 
                              type = 'bar', color = ~deltameat, colors = rev(colors_5), orientation = 'h',
                              text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                              hoverinfo = 'text',
                              hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                              meta = ~deltameat,
                              textposition = 'none',
                              texttemplate = '%{text}',
                              insidetextfont = list(color = 'white'),
                              insidetextanchor = 'middle',
                              marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dm_edu_levels_plot_print <- dm_edu_levels_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = 0.5, xanchor = 'auto', y = 1.13, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dm_edu_levels_plot_print, "images/diet/dm_edu_levels_plot.png", scale = 8, width = 800)


### Delta meat by age

# Convert to factor
dm_age <- pbff
if (!inherits(dm_age$deltameatconsumpt, "factor")) {
  dm_age$deltameat <- as_factor(dm_age$deltameatconsumpt)
}

# Add a level for NA
dm_age_groups_with_na <- c(levels(dm_age$deltameat), "NA")
dm_age$deltameat <- factor(dm_age$deltameat, levels = dm_age_groups_with_na, exclude = NULL)

# Replace NA values with "NA"
dm_age$deltameat[is.na(dm_age$deltameat)] <- "NA"

dm_age_groups <- dm_age %>%
  filter(deltameat != "NA") %>%
  mutate(age_groups_2 = as_factor(age_groups_2)) %>%
  mutate(deltameat = as_factor(deltameat)) %>%
  group_by(age_groups_2, deltameat) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(age_groups_2) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dm_age_groups_total <- dm_age %>%
  filter(deltameat != "NA") %>%
  mutate(deltameat = as_factor(deltameat)) %>%
  group_by(deltameat) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(age_groups_2 = "Total", 
         percentage = count / sum(count) * 100) %>%
  ungroup()

dm_age_groups <- bind_rows(dm_age_groups, dm_age_groups_total) %>%
  mutate(age_groups_2 = fct_relevel(age_groups_2, "Total", after = 0)) %>%
  arrange(age_groups_2) %>%
  arrange(age_groups_2 == "Total")

dm_age_groups_plot <- plot_ly(dm_age_groups, x = ~percentage, y = ~age_groups_2, 
                              type = 'bar', color = ~deltameat, colors = rev(colors_5), orientation = 'h',
                              text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                              hoverinfo = 'text',
                              hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                              meta = ~deltameat,
                              textposition = 'none',
                              texttemplate = '%{text}',
                              insidetextfont = list(color = 'white'),
                              insidetextanchor = 'middle',
                              marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dm_age_groups_plot_print <- dm_age_groups_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = 0.5, xanchor = 'auto', y = 1.13, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dm_age_groups_plot_print, "images/diet/dm_age_groups_plot.png", scale = 8, width = 800)


### Delta meat by ses

# Convert to factor
dm_ses <- pbff
if (!inherits(dm_ses$deltameatconsumpt, "factor")) {
  dm_ses$deltameat <- as_factor(dm_ses$deltameatconsumpt)
}

# Add a level for NA
dm_ses_with_na <- c(levels(dm_ses$deltameat), "NA")
dm_ses$deltameat <- factor(dm_ses$deltameat, levels = dm_ses_with_na, exclude = NULL)

# Replace NA values with "NA"
dm_ses$deltameat[is.na(dm_ses$deltameat)] <- "NA"

dm_ses_levels <- dm_ses %>%
  filter(deltameat != "NA") %>%
  mutate(ses_desc = as_factor(ses_desc)) %>%
  mutate(deltameat = as_factor(deltameat)) %>%
  group_by(ses_desc, deltameat) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(ses_desc) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dm_ses_levels_total <- dm_ses %>%
  filter(deltameat != "NA") %>%
  mutate(deltameat = as_factor(deltameat)) %>%
  group_by(deltameat) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(ses_desc = "Total", 
         percentage = count / sum(count) * 100) %>%
  ungroup()

dm_ses_levels <- bind_rows(dm_ses_levels, dm_ses_levels_total) %>%
  mutate(ses_desc = fct_relevel(ses_desc, "Total", after = 0)) %>%
  arrange(ses_desc == "Total")

dm_ses_plot <- plot_ly(dm_ses_levels, x = ~percentage, y = ~ses_desc, 
                       type = 'bar', color = ~deltameat, colors = rev(colors_5), orientation = 'h',
                       text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                       hoverinfo = 'text',
                       hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                       meta = ~deltameat,
                       textposition = 'none',
                       texttemplate = '%{text}',
                       insidetextfont = list(color = 'white'),
                       insidetextanchor = 'middle',
                       marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dm_ses_plot_print <- dm_ses_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = 0.5, xanchor = 'auto', y = 1.13, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dm_ses_plot_print, "images/diet/dm_ses_plot.png", scale = 8, width = 800)



### Intent to change consumption - Dairy

# Dairy
dd_int <- pbff
# Convert to factor
if (!inherits(dd_int$q23_1, "factor")) {
  dd_int$deltadairy_int <- as_factor(dd_int$q23_1)
}

# Add a level for NA
deltadairy_levels_with_na <- c(levels(dd_int$deltadairy_int), "NA")
dd_int$deltadairy_int <- factor(dd_int$deltadairy_int, levels = deltadairy_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dd_int$deltadairy_int[is.na(dd_int$deltadairy_int)] <- "NA"

dd_int <- dd_int %>%
  filter(deltadairy_int != "NA") %>%
  count(deltadairy_int) %>%
  mutate(percentage = n/sum(n)*100)

dd_int_plot <- plot_ly(dd_int, x = ~as_factor(deltadairy_int), y = ~percentage, type = "bar",
                               text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                               hoverinfo = 'text',  # Display text and x value on hover
                               textposition = 'none',
                               texttemplate = '%{text}',
                               insidetextfont = list(color = 'white'),
                               marker = list(color = rev(colors_5), line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%
  layout(margin = list(pad=4), yaxis = list(title = ""),
         xaxis = list(title = ""),
         barmode = 'stack',  # Stack bars to mimic histogram appearance
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dd_int_plot_print <- dd_int_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = 0.5, xanchor = 'auto', y = 1.13, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dd_int_plot_print, "images/diet/dd_int_plot.png", scale = 8, width = 800)

### Delta dairy int by diet - Omnivores and flexitarians

dd_int1 <- pbff
# Convert to factor
if (!inherits(dd_int1$q23_1, "factor")) {
  dd_int1$deltadairy_int <- as_factor(dd_int1$q23_1)
}

# Add a level for NA
deltadairy_levels_with_na <- c(levels(dd_int1$deltadairy_int), "NA")
dd_int1$deltadairy_int <- factor(dd_int1$deltadairy_int, levels = deltadairy_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dd_int1$deltadairy_int[is.na(dd_int1$deltadairy_int)] <- "NA"

dd_int_omn <- dd_int1 %>%
  filter(deltadairy_int != "NA") %>%
  filter(diet == 1) %>% # filter for omnivores
  count(deltadairy_int) %>%
  mutate(percentage = n/sum(n)*100)

dd_int_flx <- dd_int1 %>%
  filter(deltadairy_int != "NA") %>%
  filter(diet == 2) %>% # filter for omnivores
  count(deltadairy_int) %>%
  mutate(percentage = n/sum(n)*100)

# Add diet group information to both dataframes
dd_int_omn <- dd_int_omn %>%
  mutate(diet_group = "Omnivore")
dd_int_flx <- dd_int_flx %>%
  mutate(diet_group = "Flexitarian")

# Combine both dataframes
dd_int_diet_data <- bind_rows(dd_int_omn, dd_int_flx)

# Create a side-by-side bar plot
dd_int_diet_plot <- plot_ly(dd_int_diet_data, x = ~as_factor(deltadairy_int), y = ~percentage,
                            type = "bar", color = ~diet_group, colors = colors_2,
                            text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                            hoverinfo = 'text',  # Display text and x value on hover
                            textposition = 'none',
                            texttemplate = '%{text}',
                            insidetextfont = list(color = 'white')) %>%
  layout(barmode = 'group',  # Group bars side-by-side
         margin = list(pad = 4), yaxis = list(title = ""),
         xaxis = list(title = ""),
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dd_int_diet_plot_print <- dd_int_diet_plot %>%
  style(textposition = 'inside')
save_image(dd_int_diet_plot_print, "images/diet/dd_int_diet_plot.png", scale = 8, width = 800)


### Delta dairy int by country

# Convert to factor
dd_int_country <- pbff
if (!inherits(dd_int_country$q23_1, "factor")) {
  dd_int_country$dd_int <- as_factor(dd_int_country$q23_1)
}

# Add a level for NA
dd_int_country_levels_with_na <- c(levels(dd_int_country$dd_int), "NA")
dd_int_country$dd_int <- factor(dd_int_country$dd_int, levels = dd_int_country_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dd_int_country$dd_int[is.na(dd_int_country$dd_int)] <- "NA"

dd_int_countries <- dd_int_country %>%
  filter(dd_int != "NA") %>%
  mutate(Country = as_factor(Country)) %>%
  mutate(dd_int = as_factor(dd_int)) %>%
  group_by(Country, dd_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(Country) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dd_int_countries_total <- dd_int_country %>%
  filter(dd_int != "NA") %>%
  mutate(dd_int = as_factor(dd_int)) %>%
  group_by(dd_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(Country = "Total",  # Label this group as 'Overall'
         percentage = count / sum(count) * 100) %>%
  ungroup()

dd_int_countries <- bind_rows(dd_int_countries, dd_int_countries_total) %>%
  mutate(Country = fct_relevel(Country, "Total", after = 0)) %>%
  arrange(Country) %>%
  arrange(Country == "Total", percentage)

dd_int_countries_plot <- plot_ly(dd_int_countries, x = ~percentage, y = ~Country, 
                                 type = 'bar', color = ~dd_int, colors = rev(colors_5), orientation = 'h',
                                 text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                                 hoverinfo = 'text',
                                 hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                                 meta = ~dd_int,
                                 textposition = 'none',
                                 texttemplate = '%{text}',
                                 insidetextfont = list(color = 'white'),
                                 insidetextanchor = 'middle',
                                 marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dd_int_countries_plot_print <- dd_int_countries_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.05, xanchor = 'auto', y = 1.19, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dd_int_countries_plot_print, "images/diet/dd_int_countries_plot.png", scale = 8, width = 800)



### Delta dairy int by age

# Convert to factor
dd_int_age <- pbff
if (!inherits(dd_int_age$q23_1, "factor")) {
  dd_int_age$dd_int <- as_factor(dd_int_age$q23_1)
}

# Add a level for NA
dd_int_age_groups_with_na <- c(levels(dd_int_age$dd_int), "NA")
dd_int_age$dd_int <- factor(dd_int_age$dd_int, levels = dd_int_age_groups_with_na, exclude = NULL)

# Replace NA values with "NA"
dd_int_age$dd_int[is.na(dd_int_age$dd_int)] <- "NA"

dd_int_age_groups <- dd_int_age %>%
  filter(dd_int != "NA") %>%
  mutate(age_groups_2 = as_factor(age_groups_2)) %>%
  mutate(dd_int = as_factor(dd_int)) %>%
  group_by(age_groups_2, dd_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(age_groups_2) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dd_int_age_groups_total <- dd_int_age %>%
  filter(dd_int != "NA") %>%
  mutate(dd_int = as_factor(dd_int)) %>%
  group_by(dd_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(age_groups_2 = "Total", 
         percentage = count / sum(count) * 100) %>%
  ungroup()

dd_int_age_groups <- bind_rows(dd_int_age_groups, dd_int_age_groups_total) %>%
  mutate(age_groups_2 = fct_relevel(age_groups_2, "Total", after = 0)) %>%
  arrange(age_groups_2) %>%
  arrange(age_groups_2 == "Total")

dd_int_age_groups_plot <- plot_ly(dd_int_age_groups, x = ~percentage, y = ~age_groups_2, 
                                  type = 'bar', color = ~dd_int, colors = rev(colors_5), orientation = 'h',
                                  text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                                  hoverinfo = 'text',
                                  hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                                  meta = ~dd_int,
                                  textposition = 'none',
                                  texttemplate = '%{text}',
                                  insidetextfont = list(color = 'white'),
                                  insidetextanchor = 'middle',
                                  marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dd_int_age_groups_plot_print <- dd_int_age_groups_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.005, xanchor = 'auto', y = 1.19, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dd_int_age_groups_plot_print, "images/diet/dd_int_age_groups_plot.png", scale = 8, width = 800)


### Delta dairy int by education level

# Convert to factor
dd_int_edu <- pbff
if (!inherits(dd_int_edu$q23_1, "factor")) {
  dd_int_edu$dd_int <- as_factor(dd_int_edu$q23_1)
}

# Add a level for NA
dd_int_edu_levels_with_na <- c(levels(dd_int_edu$dd_int), "NA")
dd_int_edu$dd_int <- factor(dd_int_edu$dd_int, levels = dd_int_edu_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dd_int_edu$dd_int[is.na(dd_int_edu$dd_int)] <- "NA"

dd_int_edu_levels <- dd_int_edu %>%
  filter(dd_int != "NA") %>%
  mutate(edu_desc = as_factor(edu_desc)) %>%
  mutate(dd_int = as_factor(dd_int)) %>%
  group_by(edu_desc, dd_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(edu_desc) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dd_int_edu_levels_total <- dd_int_edu %>%
  filter(dd_int != "NA") %>%
  mutate(dd_int = as_factor(dd_int)) %>%
  group_by(dd_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(edu_desc = "Total", 
         percentage = count / sum(count) * 100) %>%
  ungroup()

dd_int_edu_levels <- bind_rows(dd_int_edu_levels, dd_int_edu_levels_total) %>%
  mutate(edu_desc = fct_relevel(edu_desc, "Total", after = 0)) %>%
  arrange(edu_desc == "Total")

dd_int_edu_levels_plot <- plot_ly(dd_int_edu_levels, x = ~percentage, y = ~edu_desc, 
                                  type = 'bar', color = ~dd_int, colors = rev(colors_5), orientation = 'h',
                                  text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                                  hoverinfo = 'text',
                                  hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                                  meta = ~dd_int,
                                  textposition = 'none',
                                  texttemplate = '%{text}',
                                  insidetextfont = list(color = 'white'),
                                  insidetextanchor = 'middle',
                                  marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dd_int_edu_levels_plot_print <- dd_int_edu_levels_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.15, xanchor = 'auto', y = 1.19, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dd_int_edu_levels_plot_print, "images/diet/dd_int_edu_levels_plot.png", scale = 8, width = 800)


### Delta dairy int by ses

# Convert to factor
dd_int_ses <- pbff
if (!inherits(dd_int_ses$q23_1, "factor")) {
  dd_int_ses$dd_int <- as_factor(dd_int_ses$q23_1)
}

# Add a level for NA
dd_int_ses_with_na <- c(levels(dd_int_ses$dd_int), "NA")
dd_int_ses$dd_int <- factor(dd_int_ses$dd_int, levels = dd_int_ses_with_na, exclude = NULL)

# Replace NA values with "NA"
dd_int_ses$dd_int[is.na(dd_int_ses$dd_int)] <- "NA"

dd_int_ses_levels <- dd_int_ses %>%
  filter(dd_int != "NA") %>%
  mutate(ses_desc = as_factor(ses_desc)) %>%
  mutate(dd_int = as_factor(dd_int)) %>%
  group_by(ses_desc, dd_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(ses_desc) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dd_int_ses_levels_total <- dd_int_ses %>%
  filter(dd_int != "NA") %>%
  mutate(dd_int = as_factor(dd_int)) %>%
  group_by(dd_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(ses_desc = "Total", 
         percentage = count / sum(count) * 100) %>%
  ungroup()

dd_int_ses_levels <- bind_rows(dd_int_ses_levels, dd_int_ses_levels_total) %>%
  mutate(ses_desc = fct_relevel(ses_desc, "Total", after = 0)) %>%
  arrange(ses_desc == "Total")

dd_int_ses_plot <- plot_ly(dd_int_ses_levels, x = ~percentage, y = ~ses_desc, 
                           type = 'bar', color = ~dd_int, colors = rev(colors_5), orientation = 'h',
                           text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                           hoverinfo = 'text',
                           hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                           meta = ~dd_int,
                           textposition = 'none',
                           texttemplate = '%{text}',
                           insidetextfont = list(color = 'white'),
                           insidetextanchor = 'middle',
                           marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dd_int_ses_plot_print <- dd_int_ses_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.2, xanchor = 'auto', y = 1.19, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dd_int_ses_plot_print, "images/diet/dd_int_ses_plot.png", scale = 8, width = 800)



# Intention to change - Meat

# Meat
dm_int <- pbff
# Convert to factor
if (!inherits(dm_int$q23_2, "factor")) {
  dm_int$deltameat_int <- as_factor(dm_int$q23_2)
}

# Add a level for NA
deltameat_levels_with_na <- c(levels(dm_int$deltameat_int), "NA")
dm_int$deltameat_int <- factor(dm_int$deltameat_int, levels = deltameat_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dm_int$deltameat_int[is.na(dm_int$deltameat_int)] <- "NA"

dm_int <- dm_int %>%
  filter(deltameat_int != "NA") %>%
  count(deltameat_int) %>%
  mutate(percentage = n/sum(n)*100)

dm_int_plot <- plot_ly(dm_int, x = ~as_factor(deltameat_int), y = ~percentage, type = "bar",
                       text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                       hoverinfo = 'text',  # Display text and x value on hover
                       textposition = 'none',
                       texttemplate = '%{text}',
                       insidetextfont = list(color = 'white'),
                       marker = list(color = rev(colors_5), line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%
  layout(margin = list(pad=4), yaxis = list(title = ""),
         xaxis = list(title = ""),
         barmode = 'stack',  # Stack bars to mimic histogram appearance
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dm_int_plot_print <- dm_int_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = 0.5, xanchor = 'auto', y = 1.13, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dm_int_plot_print, "images/diet/dm_int_plot.png", scale = 8, width = 800)


### Delta meat int by diet - Omnivores and flexitarians

dm_int1 <- pbff
# Convert to factor
if (!inherits(dm_int1$q23_2, "factor")) {
  dm_int1$deltameat_int <- as_factor(dm_int1$q23_2)
}

# Add a level for NA
deltameat_levels_with_na <- c(levels(dm_int1$deltameat_int), "NA")
dm_int1$deltameat_int <- factor(dm_int1$deltameat_int, levels = deltameat_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dm_int1$deltameat_int[is.na(dm_int1$deltameat_int)] <- "NA"

dm_int_omn <- dm_int1 %>%
  filter(deltameat_int != "NA") %>%
  filter(diet == 1) %>% # filter for omnivores
  count(deltameat_int) %>%
  mutate(percentage = n/sum(n)*100)

dm_int_flx <- dm_int1 %>%
  filter(deltameat_int != "NA") %>%
  filter(diet == 2) %>% # filter for omnivores
  count(deltameat_int) %>%
  mutate(percentage = n/sum(n)*100)

# Add diet group information to both dataframes
dm_int_omn <- dm_int_omn %>%
  mutate(diet_group = "Omnivore")
dm_int_flx <- dm_int_flx %>%
  mutate(diet_group = "Flexitarian")

# Combine both dataframes
dm_int_diet_data <- bind_rows(dm_int_omn, dm_int_flx)

# Create a side-by-side bar plot
dm_int_diet_plot <- plot_ly(dm_int_diet_data, x = ~as_factor(deltameat_int), y = ~percentage,
                         type = "bar", color = ~diet_group, colors = colors_2,
                         text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                         hoverinfo = 'text',  # Display text and x value on hover
                         textposition = 'none',
                         texttemplate = '%{text}',
                         insidetextfont = list(color = 'white')) %>%
  layout(barmode = 'group',  # Group bars side-by-side
         margin = list(pad = 4), yaxis = list(title = ""),
         xaxis = list(title = ""),
         margin = list(b = 100)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dm_int_diet_plot_print <- dm_int_diet_plot %>%
  style(textposition = 'inside')
save_image(dm_int_diet_plot_print, "images/diet/dm_int_diet_plot.png", scale = 8, width = 800)


### Delta meat int by country

# Convert to factor
dm_int_country <- pbff
if (!inherits(dm_int_country$q23_2, "factor")) {
  dm_int_country$dm_int <- as_factor(dm_int_country$q23_2)
}

# Add a level for NA
dm_int_country_levels_with_na <- c(levels(dm_int_country$dm_int), "NA")
dm_int_country$dm_int <- factor(dm_int_country$dm_int, levels = dm_int_country_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dm_int_country$dm_int[is.na(dm_int_country$dm_int)] <- "NA"

dm_int_countries <- dm_int_country %>%
  filter(dm_int != "NA") %>%
  mutate(Country = as_factor(Country)) %>%
  mutate(dm_int = as_factor(dm_int)) %>%
  group_by(Country, dm_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(Country) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dm_int_countries_total <- dm_int_country %>%
  filter(dm_int != "NA") %>%
  mutate(dm_int = as_factor(dm_int)) %>%
  group_by(dm_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(Country = "Total",  # Label this group as 'Overall'
         percentage = count / sum(count) * 100) %>%
  ungroup()

dm_int_countries <- bind_rows(dm_int_countries, dm_int_countries_total) %>%
  mutate(Country = fct_relevel(Country, "Total", after = 0)) %>%
  arrange(Country) %>%
  arrange(Country == "Total", percentage)

dm_int_countries_plot <- plot_ly(dm_int_countries, x = ~percentage, y = ~Country, 
                                 type = 'bar', color = ~dm_int, colors = rev(colors_5), orientation = 'h',
                                 text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                                 hoverinfo = 'text',
                                 hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                                 meta = ~dm_int,
                                 textposition = 'none',
                                 texttemplate = '%{text}',
                                 insidetextfont = list(color = 'white'),
                                 insidetextanchor = 'middle',
                                 marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dm_int_countries_plot_print <- dm_int_countries_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.05, xanchor = 'auto', y = 1.19, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dm_int_countries_plot_print, "images/diet/dm_int_countries_plot.png", scale = 8, width = 800)



### Delta meat int by age

# Convert to factor
dm_int_age <- pbff
if (!inherits(dm_int_age$q23_2, "factor")) {
  dm_int_age$dm_int <- as_factor(dm_int_age$q23_2)
}

# Add a level for NA
dm_int_age_groups_with_na <- c(levels(dm_int_age$dm_int), "NA")
dm_int_age$dm_int <- factor(dm_int_age$dm_int, levels = dm_int_age_groups_with_na, exclude = NULL)

# Replace NA values with "NA"
dm_int_age$dm_int[is.na(dm_int_age$dm_int)] <- "NA"

dm_int_age_groups <- dm_int_age %>%
  filter(dm_int != "NA") %>%
  mutate(age_groups_2 = as_factor(age_groups_2)) %>%
  mutate(dm_int = as_factor(dm_int)) %>%
  group_by(age_groups_2, dm_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(age_groups_2) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dm_int_age_groups_total <- dm_int_age %>%
  filter(dm_int != "NA") %>%
  mutate(dm_int = as_factor(dm_int)) %>%
  group_by(dm_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(age_groups_2 = "Total", 
         percentage = count / sum(count) * 100) %>%
  ungroup()

dm_int_age_groups <- bind_rows(dm_int_age_groups, dm_int_age_groups_total) %>%
  mutate(age_groups_2 = fct_relevel(age_groups_2, "Total", after = 0)) %>%
  arrange(age_groups_2) %>%
  arrange(age_groups_2 == "Total")

dm_int_age_groups_plot <- plot_ly(dm_int_age_groups, x = ~percentage, y = ~age_groups_2, 
                                  type = 'bar', color = ~dm_int, colors = rev(colors_5), orientation = 'h',
                                  text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                                  hoverinfo = 'text',
                                  hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                                  meta = ~dm_int,
                                  textposition = 'none',
                                  texttemplate = '%{text}',
                                  insidetextfont = list(color = 'white'),
                                  insidetextanchor = 'middle',
                                  marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dm_int_age_groups_plot_print <- dm_int_age_groups_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.005, xanchor = 'auto', y = 1.19, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dm_int_age_groups_plot_print, "images/diet/dm_int_age_groups_plot.png", scale = 8, width = 800)


### Delta meat int by education level

# Convert to factor
dm_int_edu <- pbff
if (!inherits(dm_int_edu$q23_2, "factor")) {
  dm_int_edu$dm_int <- as_factor(dm_int_edu$q23_2)
}

# Add a level for NA
dm_int_edu_levels_with_na <- c(levels(dm_int_edu$dm_int), "NA")
dm_int_edu$dm_int <- factor(dm_int_edu$dm_int, levels = dm_int_edu_levels_with_na, exclude = NULL)

# Replace NA values with "NA"
dm_int_edu$dm_int[is.na(dm_int_edu$dm_int)] <- "NA"

dm_int_edu_levels <- dm_int_edu %>%
  filter(dm_int != "NA") %>%
  mutate(edu_desc = as_factor(edu_desc)) %>%
  mutate(dm_int = as_factor(dm_int)) %>%
  group_by(edu_desc, dm_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(edu_desc) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dm_int_edu_levels_total <- dm_int_edu %>%
  filter(dm_int != "NA") %>%
  mutate(dm_int = as_factor(dm_int)) %>%
  group_by(dm_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(edu_desc = "Total", 
         percentage = count / sum(count) * 100) %>%
  ungroup()

dm_int_edu_levels <- bind_rows(dm_int_edu_levels, dm_int_edu_levels_total) %>%
  mutate(edu_desc = fct_relevel(edu_desc, "Total", after = 0)) %>%
  arrange(edu_desc == "Total")

dm_int_edu_levels_plot <- plot_ly(dm_int_edu_levels, x = ~percentage, y = ~edu_desc, 
                                  type = 'bar', color = ~dm_int, colors = rev(colors_5), orientation = 'h',
                                  text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                                  hoverinfo = 'text',
                                  hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                                  meta = ~dm_int,
                                  textposition = 'none',
                                  texttemplate = '%{text}',
                                  insidetextfont = list(color = 'white'),
                                  insidetextanchor = 'middle',
                                  marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dm_int_edu_levels_plot_print <- dm_int_edu_levels_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.15, xanchor = 'auto', y = 1.19, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dm_int_edu_levels_plot_print, "images/diet/dm_int_edu_levels_plot.png", scale = 8, width = 800)


### Delta meat int by ses

# Convert to factor
dm_int_ses <- pbff
if (!inherits(dm_int_ses$q23_2, "factor")) {
  dm_int_ses$dm_int <- as_factor(dm_int_ses$q23_2)
}

# Add a level for NA
dm_int_ses_with_na <- c(levels(dm_int_ses$dm_int), "NA")
dm_int_ses$dm_int <- factor(dm_int_ses$dm_int, levels = dm_int_ses_with_na, exclude = NULL)

# Replace NA values with "NA"
dm_int_ses$dm_int[is.na(dm_int_ses$dm_int)] <- "NA"

dm_int_ses_levels <- dm_int_ses %>%
  filter(dm_int != "NA") %>%
  mutate(ses_desc = as_factor(ses_desc)) %>%
  mutate(dm_int = as_factor(dm_int)) %>%
  group_by(ses_desc, dm_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(ses_desc) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

dm_int_ses_levels_total <- dm_int_ses %>%
  filter(dm_int != "NA") %>%
  mutate(dm_int = as_factor(dm_int)) %>%
  group_by(dm_int) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(ses_desc = "Total", 
         percentage = count / sum(count) * 100) %>%
  ungroup()

dm_int_ses_levels <- bind_rows(dm_int_ses_levels, dm_int_ses_levels_total) %>%
  mutate(ses_desc = fct_relevel(ses_desc, "Total", after = 0)) %>%
  arrange(ses_desc == "Total")

dm_int_ses_plot <- plot_ly(dm_int_ses_levels, x = ~percentage, y = ~ses_desc, 
                           type = 'bar', color = ~dm_int, colors = rev(colors_5), orientation = 'h',
                           text = ~paste0(round(percentage, 1), "%"),  # Text to display inside bars
                           hoverinfo = 'text',
                           hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                           meta = ~dm_int,
                           textposition = 'none',
                           texttemplate = '%{text}',
                           insidetextfont = list(color = 'white'),
                           insidetextanchor = 'middle',
                           marker = list(line = list(color = 'rgba(0,0,0,0)', width = 1))) %>%  # Ensure text is inside
  layout(margin = list(pad=4), barmode = 'stack',  # Stack bars
         xaxis = list(title = ''),  # X-axis label
         yaxis = list(title = '', categoryorder = 'trace'),
         margin = list(b = 100),
         legend = list(traceorder = 'normal')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
dm_int_ses_plot_print <- dm_int_ses_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.2, xanchor = 'auto', y = 1.19, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(dm_int_ses_plot_print, "images/diet/dm_int_ses_plot.png", scale = 8, width = 800)



### Frequency of PBFF consumption

fc1_data <- pbff %>%
  select(starts_with("FC1_"))

for (col_name in names(fc1_data)) {
  label <- attr(fc1_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(fc1_data)[names(fc1_data) == col_name] <- label
  }
}

fc1_data_long <- fc1_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
fc1_data_long$value_desc <- as_factor(fc1_data_long$value)
fc1_data_long$variable <- fc1_data_long$variable %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_remove("Please indicate.*") %>%
  str_replace_all("Plant-Based Fermented dairy alternatives \\(e.g., ", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\)", "") %>%
  str_replace_all(", with live organisms", "") %>%
  str_replace_all("Plant-Based Yoghurt", "Plant-Based Fermented yoghurt") %>%
  str_replace_all("Plant-Based Ice cream", "Plant-Based Fermented ice cream") %>%
  str_replace_all("Plant-Based Soft/Firm", "Plant-Based Fermented soft/firm") %>%
  trimws()

# Aggregate and calculate percentages
fc1_data_agg <- fc1_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '8', '9') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
fc1_plot <- plot_ly(fc1_data_agg, x = ~percentage, y = ~variable,
                    type = 'bar', color = ~value_desc, colors = colors_9_alt,
                    orientation = 'h',
                    text = ~paste0(round(percentage, 1), "%"),
                    hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                    meta = ~value_desc,
                    textposition = 'none',
                    insidetextanchor = 'middle',
                    insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace")) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
fc1_plot_print <- fc1_plot %>%
  style(textposition = 'inside') %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.6, xanchor = 'auto', y = 1.35, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(fc1_plot_print, "images/diet/fc1_plot.png", scale = 8, width = 800)


#### By Country (Dropdown menu) for FC1 data

fc1_data_dd <- pbff %>%
  select(starts_with("FC1_"))

for (col_name in names(fc1_data_dd)) {
  label <- attr(fc1_data_dd[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(fc1_data_dd)[names(fc1_data_dd) == col_name] <- label
  }
}

fc1_data_dd_long <- fc1_data_dd %>%
  mutate(id = row_number(), Country = as_factor(pbff$Country)) %>%
  pivot_longer(cols = -c(id, Country), names_to = "variable", values_to = "value")

# Get labels and clean them
fc1_data_dd_long$value_desc <- as_factor(fc1_data_dd_long$value)
fc1_data_dd_long$variable <- fc1_data_dd_long$variable %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_remove("Please indicate.*") %>%
  str_replace_all("Plant-Based Fermented dairy alternatives \\(e.g., ", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\)", "") %>%
  str_replace_all(", with live organisms", "") %>%
  str_replace_all("Plant-Based Yoghurt", "Plant-Based Fermented yoghurt") %>%
  str_replace_all("Plant-Based Ice cream", "Plant-Based Fermented ice cream") %>%
  str_replace_all("Plant-Based Soft/Firm", "Plant-Based Fermented soft/firm") %>%
  trimws()

# Function to aggregate and calculate percentages
aggregate_data <- function(data) {
  data %>%
    group_by(variable, value, value_desc, Country) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(variable, Country) %>%
    mutate(percentage = count / sum(count) * 100,
           text_label = ifelse(value %in% c('1', '2', '8', '9') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
    arrange(desc(value), desc(percentage))
}

# Aggregate the data
fc1_data_dd_agg <- aggregate_data(fc1_data_dd_long)

# Summarize data for 'All'
fc1_all_data <- fc1_data_dd_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '8', '9') & percentage > 5, paste0(round(percentage, 1), "%"), ""),
         Country = "All")

fc1_data_combined <- bind_rows(fc1_data_dd_agg, fc1_all_data) %>%
  arrange(desc(value), desc(percentage))

# List of countries including "All"
countries <- c("All", levels(fc1_data_dd_long$Country))

# Create traces for each country including "All"
traces <- list()
for (i in seq_along(countries)) {
  country <- countries[i]
  country_data <- fc1_data_combined %>% filter(Country == country)
  
  for (value_desc in levels(fc1_data_dd_long$value_desc)) {
    value_data <- country_data %>% filter(value_desc == !!value_desc)
    if (nrow(value_data) == 0) next
    
    value_index <- (match(value_desc, levels(fc1_data_dd_long$value_desc)) - 1) %% length(colors_9_alt) + 1
    color <- colors_9_alt[value_index]
    
    trace <- list(
      x = value_data$percentage,
      y = value_data$variable,
      type = 'bar',
      orientation = 'h',
      name = as.character(value_desc),
      marker = list(color = color),
      text = value_data$text_label,
      textposition = 'none',
      insidetextanchor = 'middle',
      insidetextfont = list(color = 'white'),
      hoverinfo = 'text',
      hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
      meta = value_data$value_desc,
      visible = ifelse(i == 1, TRUE, FALSE) # Only the first trace is visible initially
    )
    
    traces <- append(traces, list(trace))
  }
}

# Create dropdown buttons for each country including "All"
dropdown_buttons <- lapply(seq_along(countries), function(i) {
  list(
    method = "update",
    args = list(list(visible = rep(i == seq_along(countries), each = length(levels(fc1_data_dd_long$value_desc))))),
    label = countries[i]
  )
})

# Create the plot
fc1_plot_dd <- plot_ly()

for (trace in traces) {
  fc1_plot_dd <- add_trace(fc1_plot_dd, x = trace$x, y = trace$y, type = trace$type, orientation = trace$orientation,
                           marker = trace$marker, name = trace$name, text = trace$text, textposition = trace$textposition,
                           insidetextanchor = trace$insidetextanchor, insidetextfont = trace$insidetextfont,
                           hoverinfo = trace$hoverinfo, hovertemplate = trace$hovertemplate, meta = trace$meta,
                           visible = trace$visible)
}

fc1_plot_dd <- fc1_plot_dd %>%
  layout(
    barmode = 'stack',
    xaxis = list(title = ""),
    yaxis = list(title = "", categoryorder = "trace"),
    updatemenus = list(list(
      active = 0,
      buttons = dropdown_buttons,
      x = 0.5, # Center horizontally
      xanchor = 'center', # Anchor to the center
      y = 1.2, # Place above the plot
      yanchor = 'top' # Anchor to the top
    )),
    margin = list(pad = 4)
  ) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)



