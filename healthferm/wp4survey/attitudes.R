
### Food choice motives

fcm_data <- pbff %>%
  select(starts_with("FCM_"))

for (col_name in names(fcm_data)) {
  label <- attr(fcm_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(fcm_data)[names(fcm_data) == col_name] <- label
  }
}

fcm_data_long <- fcm_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
fcm_data_long$value_desc <- as_factor(fcm_data_long$value)
fcm_data_long$variable <- fcm_data_long$variable %>%
  str_remove_all("\\(.*?\\)") %>%  # Remove any text in parentheses
  str_replace_all("\\[|\\]", "") %>%  # Remove the square brackets
  str_replace_all("It.*", "") %>%
  str_replace_all("Is", "") %>%
  str_replace_all("is", "") %>%
  trimws()

# Aggregate and calculate percentages
fcm_data_agg <- fcm_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
fcm_plot <- plot_ly(fcm_data_agg, x = ~percentage, y = ~variable,
                    type = 'bar', color = ~value_desc, colors = colors_7,
                    orientation = 'h',
                    text = ~text_label,
                    hoverinfo = 'text',
                    hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                    meta = ~value_desc,
                    textposition = 'inside',
                    insidetextanchor = 'middle',
                    insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4),
         barmode = 'stack',
         xaxis = list(title = "Percentage"),
         yaxis = list(title = "", categoryorder = "trace")) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
fcm_plot_print <- fcm_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.2, xanchor = 'auto', y = 1.1, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(fcm_plot_print, "images/attitudes/fcm_plot.png", scale = 8, width = 1200, height = 800)

# # Convert FCM variables to factors with labels
# fcm <- pbff %>%
#   mutate(across(starts_with("FCM_"), as_factor))
# 
# # Create a new data frame to store labels and maintain the original order
# fcm_labels <- data.frame(
#   food_choice_motive = names(fcm)[grepl("FCM_", names(fcm))],
#   label = sapply(fcm[grepl("FCM_", names(fcm))], function(x) attr(x, "label"))
# )
# 
# # Extract text within square brackets and remove text in parentheses
# fcm_labels$label <- str_extract(fcm_labels$label, "\\[(.*?)\\]") %>% 
#   str_remove_all("\\(.*?\\)") %>%  # Remove any text in parentheses
#   str_replace_all("\\[|\\]", "") %>%  # Remove the square brackets
#   str_replace_all("is", "") %>%
#   str_replace_all("Is", "") 
# 
# # Now, pivot the original data to long format
# fcm_long <- fcm %>%
#   pivot_longer(
#     cols = starts_with("FCM_"),
#     names_to = "food_choice_motive",
#     values_to = "agreement_level"
#   )
# 
# # Join the labels with the long data
# fcm_long <- fcm_long %>%
#   left_join(fcm_labels, by = "food_choice_motive") %>%
#   select(-food_choice_motive) %>%
#   rename(food_choice_motive = label)
# 
# # Convert food_choice_motive to a factor with the levels ordered as in fcm_labels
# fcm_long$food_choice_motive <- factor(fcm_long$food_choice_motive, levels = fcm_labels$label)
# 
# # Group by motive and agreement level and count frequencies
# fcm_agreement <- fcm_long %>%
#   group_by(food_choice_motive, agreement_level) %>%
#   summarise(count = n(), .groups = 'drop') 
# 
# # Plot using Plotly with horizontal bars and proper labels
# fcm_plot <- plot_ly(data = fcm_agreement, y = ~food_choice_motive, x = ~count, type = 'bar',
#                     color = ~agreement_level, colors = colors_7, orientation = 'h') %>%
#   layout(margin = list(pad=4), xaxis = list(title = ''),
#          yaxis = list(title = '', tickangle = -45, automargin = TRUE),
#          barmode = 'stack') %>%
#   config(displayModeBar = FALSE, displaylogo = FALSE)

### Values

values_data <- pbff %>%
  select(starts_with("V_"))

for (col_name in names(values_data)) {
  label <- attr(values_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(values_data)[names(values_data) == col_name] <- label
  }
}

values_data_long <- values_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
values_data_long$value_desc <- as_factor(values_data_long$value)
values_data_long$variable <- values_data_long$variable %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  trimws()

# Aggregate and calculate percentages
values_data_agg <- values_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('0', '1', '7', '8') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
values_plot <- plot_ly(values_data_agg, x = ~percentage, y = ~variable,
                       type = 'bar', color = ~value_desc, colors = colors_9_alt,
                       orientation = 'h',
                       text = ~text_label,
                       hoverinfo = 'text',
                       hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                       meta = ~value_desc,
                       textposition = 'inside',
                       insidetextanchor = 'middle',
                       insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace")) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
values_plot_print <- values_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = 0, xanchor = 'auto', y = 1.075, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(values_plot_print, "images/attitudes/values_plot.png", scale = 8, width = 1200, height = 800)

# values_plots_list <- list()
# 
# # Loop through the column names and create a histogram for each
# for (i in 1:10) {
#   variable_name <- paste("V", i, sep="_")
#   plot_name <- paste(variable_name, "plot", sep="_")
#   
#   # Extract the label for the current variable
#   var_label <- attr(pbff[[variable_name]], "label")
#   if (is.null(var_label)) var_label <- variable_name  # Use the variable name if no label is found
#   
#   # Clean the label
#   var_label <- var_label %>%
#     str_replace_all("\\[", "") %>%
#     str_replace_all("\\]", "") %>%
#     str_replace_all("Please.*", "") %>%
#     str_replace_all("\\([^\\)]*\\)", "")
#   
#   # Create histogram using plot_ly
#   plot <- pbff %>%
#     plot_ly(x = as_factor(.[[variable_name]]), type = "histogram", name = var_label) %>%
#     # layout(margin = list(pad=4), title = var_label) %>%
#     config(displayModeBar = FALSE, displaylogo = FALSE)
#   
#   # Store both plot and label in a sub-list within the main list
#   values_plots_list[[plot_name]] <- list(plot = plot, label = var_label)
# }
# 
# # To access the plot for FC_3:
#  values_plots_list$V_9_plot$plot
# 
# # To access the label for FC_3:
#  values_plots_list$V_3_plot$label
# 
# # Combine plots into a single subplot grid
# 
# num_rows <- 5
# 
# # Create the subplot
# values_combined_plot <- subplot(
#   lapply(names(values_plots_list), function(x) values_plots_list[[x]]$plot),
#   nrows = num_rows, # ncols = num_cols,
#   shareX = TRUE, shareY = TRUE,
#   titleX = F,  # Share x-axis title
#   titleY = F,  # Share y-axis title
#   margin = 0.05   # Set margins to prevent overlap
# ) 
# values_combined_plot

### Attitudes and familiarity

## pbf

att_data <- pbff %>%
  select(starts_with("ATT_")) %>%
  select(-ATT_PBFF)

for (col_name in names(att_data)) {
  label <- attr(att_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(att_data)[names(att_data) == col_name] <- label
  }
}

att_data_long <- att_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
att_data_long$value_desc <- as_factor(att_data_long$value)
att_data_long$variable <- att_data_long$variable %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  trimws() %>%
  str_wrap(width = 42)

# Aggregate and calculate percentages
att_data_agg <- att_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
att_pbf_plot <- plot_ly(att_data_agg, x = ~percentage, y = ~variable,
                    type = 'bar', color = ~value_desc, colors = colors_7,
                    orientation = 'h',
                    text = ~text_label,
                    hoverinfo = 'text',
                    hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                    meta = ~value_desc,
                    textposition = 'inside',
                    insidetextanchor = 'middle',
                    insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "Percentage"), yaxis = list(title = "", categoryorder = "trace")) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
att_pbf_plot_print <- att_pbf_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.215, xanchor = 'auto', y = 1.1, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(att_pbf_plot_print, "images/attitudes/att_pbf_plot.png", scale = 8, width = 1200, height = 800)

# # Load and prepare the factors
# att <- pbff %>%
#   select(starts_with("ATT_")) %>%
#   select(-ATT_PBFF) %>%
#   mutate(across(starts_with("ATT_"), ~as_factor(.)))
# 
# # Extract labels from each factor
# att_labels <- sapply(att, function(x) attr(x, "label"))
# 
# # Pivot the data to a long format and count occurrences
# att_counts <- att %>%
#   pivot_longer(cols = starts_with("ATT_"), names_to = "variable", values_to = "value") %>%
#   count(variable, value)
# 
# # Calculate total counts for percentage calculation
# total_att_counts <- att_counts %>%
#   group_by(variable) %>%
#   summarise(total = sum(n))
# 
# # Join total counts back to the counts for percentage calculation
# att_counts_with_total <- att_counts %>%
#   left_join(total_att_counts, by = "variable")
# 
# # Calculate percentages
# att_counts_with_percentages <- att_counts_with_total %>%
#   mutate(percentage = n / total * 100)
# 
# # Spread the counts data to wide format
# att_counts_wide <- att_counts_with_total %>%
#   select(-total) %>%
#   spread(key = variable, value = n, fill = 0)
# 
# # Spread the percentages data to wide format
# att_percentages_wide <- att_counts_with_percentages %>%
#   select(-total, -n) %>%
#   spread(key = variable, value = percentage, fill = 0)
# 
# # Apply labels to the columns of both dataframes
# colnames(att_counts_wide)[-1] <- sapply(colnames(att_counts_wide)[-1], function(var) att_labels[[var]])
# colnames(att_percentages_wide)[-1] <- sapply(colnames(att_percentages_wide)[-1], function(var) att_labels[[var]])
# 
# # Convert back to long format
# att_percentages_long <- att_percentages_wide %>%
#   pivot_longer(cols = -value, names_to = "variable", values_to = "percentage")
# 
# att_percentages_long$variable <- att_percentages_long$variable %>%
#   str_replace_all("\\[", "") %>%
#   str_replace_all("\\]", "") %>%
#   str_replace_all("Please.*", "") %>%
#   str_replace_all("\\([^\\)]*\\)", "") %>%
#   str_replace_all("\\.", "")
# 
# # Create a horizontal bar chart
# att_pbf_plot <- plot_ly(data = att_percentages_long, x = ~percentage, y = ~variable,
#                          type = 'bar', orientation = 'h',
#                          color = ~value, colors = colors_7, text = ~paste(round(percentage, 2), "%"),
#                          hoverinfo = 'text') %>%
#   layout(margin = list(pad=4), yaxis = list(title = "", categoryorder = "trace"),
#          xaxis = list(title = "Percentage"),
#          barmode = 'stack',
#          legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = 'center', yanchor = 'top')) %>%
#   config(displayModeBar = FALSE, displaylogo = FALSE)


## pbff

att0_data <- pbff %>%
  select(starts_with("ATT0_"))

for (col_name in names(att0_data)) {
  label <- attr(att0_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(att0_data)[names(att0_data) == col_name] <- label
  }
}

att0_data_long <- att0_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
att0_data_long$value_desc <- as_factor(att0_data_long$value)
att0_data_long$variable <- att0_data_long$variable %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
att0_data_agg <- att0_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
att_pbff_plot <- plot_ly(att0_data_agg, x = ~percentage, y = ~variable,
                         type = 'bar', color = ~value_desc, colors = colors_7,
                         orientation = 'h',
                         text = ~text_label,
                         hoverinfo = 'text',
                         hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                         meta = ~value_desc,
                         textposition = 'inside',
                         insidetextanchor = 'middle',
                         insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "Percentage"), yaxis = list(title = "", categoryorder = "trace")) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
att_pbff_plot_print <- att_pbff_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.4, xanchor = 'auto', y = 1.1, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(att_pbff_plot_print, "images/attitudes/att_pbff_plot.png", scale = 8, width = 1200, height = 800)

### Willingness to try

wtpatt_data <- pbff %>%
  select(starts_with("WTPATT_"))

for (col_name in names(wtpatt_data)) {
  label <- attr(wtpatt_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(wtpatt_data)[names(wtpatt_data) == col_name] <- label
  }
}

wtpatt_data_long <- wtpatt_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
wtpatt_data_long$value_desc <- as_factor(wtpatt_data_long$value)
wtpatt_data_long$variable <- wtpatt_data_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would be willing to try a product that is prepared through ", "") %>%
  str_replace_all("I would be willing to try a product that is prepared though ", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
wtpatt_data_agg <- wtpatt_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
wtt_plot <- plot_ly(wtpatt_data_agg, x = ~variable, y = ~percentage,
                         type = 'bar', color = ~value_desc, colors = colors_7,
                         orientation = 'v',
                         text = ~text_label,
                         hoverinfo = 'text',
                         hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                         meta = ~value_desc,
                         textposition = 'inside',
                         insidetextanchor = 'middle',
                         insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(wtt_plot, "images/attitudes/wtt_plot.png", scale = 8)


### WTT by diet

# Extract columns starting with "WTPATT_" and include "diet"
wtpatt_diet <- pbff %>%
  select(diet, starts_with("WTPATT_"))

# Rename columns starting with "WTPATT_" only, leaving "diet" intact
for (col_name in names(wtpatt_diet)[-1]) {  # Exclude "diet" from renaming
  label <- attr(wtpatt_diet[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(wtpatt_diet)[names(wtpatt_diet) == col_name] <- label
  }
}

# Step 1: Aggregate data to calculate mean values per country and variable
wtt_mean_diet <- wtpatt_diet %>%
  group_by(diet) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -diet, names_to = "variable", values_to = "mean_value")

# Clean labels for variables
wtt_mean_diet$variable <- wtt_mean_diet$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would be willing to try a product that is prepared through ", "") %>%
  str_replace_all("I would be willing to try a product that is prepared though ", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Step 2: Plot horizontal bars representing mean values per variable grouped by country
wtt_diet_plot <- plot_ly(wtt_mean_diet, x = ~mean_value, y = ~as_factor(diet),
                         type = 'bar', orientation = 'h',
                         color = ~variable, colors = colors_3,
                         text = ~round(mean_value, 1),
                         hoverinfo = 'text',
                         hovertemplate = "<b>%{y}</b><br>%{meta}<br>%{x:.1f}<extra></extra>",
                         meta = ~variable,
                         textposition = 'inside',
                         insidetextfont = list(color = 'white')) %>%
  layout(xaxis = list(title = "Mean Value, 7-Point Scale"), yaxis = list(title = ""), barmode = 'group', margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(wtt_diet_plot, "images/attitudes/wtt_diet_plot.png", scale = 8)


### WTT by country

# Extract columns starting with "WTPATT_" and include "Country"
wtpatt_countries <- pbff %>%
  select(Country, starts_with("WTPATT_"))

# Rename columns starting with "WTPATT_" only, leaving "Country" intact
for (col_name in names(wtpatt_countries)[-1]) {  # Exclude "Country" from renaming
  label <- attr(wtpatt_countries[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(wtpatt_countries)[names(wtpatt_countries) == col_name] <- label
  }
}

# Step 1: Aggregate data to calculate mean values per country and variable
wtt_mean_countries <- wtpatt_countries %>%
  group_by(Country) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -Country, names_to = "variable", values_to = "mean_value")

# Clean labels for variables
wtt_mean_countries$variable <- wtt_mean_countries$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would be willing to try a product that is prepared through ", "") %>%
  str_replace_all("I would be willing to try a product that is prepared though ", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Step 2: Calculate total mean values per country for ordering
wtt_country_order <- wtt_mean_countries %>%
  group_by(Country) %>%
  summarise(total_mean = sum(mean_value, na.rm = TRUE)) %>%
  arrange(desc(total_mean))

# Join the total mean values to retain the correct ordering
wtt_mean_countries <- wtt_mean_countries %>%
  left_join(wtt_country_order, by = "Country") %>%
  arrange(total_mean)

# Step 3: Plot horizontal bars representing mean values per variable grouped by country
wtt_countries_plot <- plot_ly(wtt_mean_countries, x = ~mean_value, y = ~as_factor(Country),
                     type = 'bar', orientation = 'h',
                     color = ~variable, colors = colors_3,
                     text = ~round(mean_value, 1),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{y}</b><br>%{meta}<br>%{x:.1f}<extra></extra>",
                     meta = ~variable,
                     textposition = 'inside',
                     insidetextfont = list(color = 'white')) %>%
  layout(xaxis = list(title = "Mean Value, 7-Point Scale"), yaxis = list(title = "", categoryorder = 'trace'), barmode = 'group', margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(wtt_countries_plot, "images/attitudes/wtt_countries_plot.png", scale = 8)

### WTT by age

# Extract columns starting with "WTPATT_" and include "age_groups_2"
wtpatt_age <- pbff %>%
  select(age_groups_2, starts_with("WTPATT_"))

# Rename columns starting with "WTPATT_" only, leaving "age_groups_2" intact
for (col_name in names(wtpatt_age)[-1]) {  # Exclude "age_groups_2" from renaming
  label <- attr(wtpatt_age[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(wtpatt_age)[names(wtpatt_age) == col_name] <- label
  }
}

# Step 1: Aggregate data to calculate mean values per country and variable
wtt_mean_age <- wtpatt_age %>%
  group_by(age_groups_2) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -age_groups_2, names_to = "variable", values_to = "mean_value")

# Clean labels for variables
wtt_mean_age$variable <- wtt_mean_age$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would be willing to try a product that is prepared through ", "") %>%
  str_replace_all("I would be willing to try a product that is prepared though ", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Step 2: Plot horizontal bars representing mean values per variable grouped by country
wtt_age_plot <- plot_ly(wtt_mean_age, x = ~mean_value, y = ~as_factor(age_groups_2),
                        type = 'bar', orientation = 'h',
                        color = ~variable, colors = colors_3,
                        text = ~round(mean_value, 1),
                        hoverinfo = 'text',
                        hovertemplate = "<b>%{y}</b><br>%{meta}<br>%{x:.1f}<extra></extra>",
                        meta = ~variable,
                        textposition = 'inside',
                        insidetextfont = list(color = 'white')) %>%
  layout(xaxis = list(title = "Mean Value, 7-Point Scale"), yaxis = list(title = ""), barmode = 'group', margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(wtt_age_plot, "images/attitudes/wtt_age_plot.png", scale = 8)


### WTT by education level

# Extract columns starting with "WTPATT_" and include "edu"
wtpatt_edu <- pbff %>%
  select(edu_desc, starts_with("WTPATT_"))

# Rename columns starting with "WTPATT_" only, leaving "edu" intact
for (col_name in names(wtpatt_edu)[-1]) {  # Exclude "edu_desc" from renaming
  label <- attr(wtpatt_edu[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(wtpatt_edu)[names(wtpatt_edu) == col_name] <- label
  }
}

# Step 1: Aggregate data to calculate mean values per country and variable
wtt_mean_edu <- wtpatt_edu %>%
  group_by(edu_desc) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -edu_desc, names_to = "variable", values_to = "mean_value")

# Clean labels for variables
wtt_mean_edu$variable <- wtt_mean_edu$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would be willing to try a product that is prepared through ", "") %>%
  str_replace_all("I would be willing to try a product that is prepared though ", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Step 2: Plot horizontal bars representing mean values per variable grouped by country
wtt_edu_plot <- plot_ly(wtt_mean_edu, x = ~mean_value, y = ~as_factor(edu_desc),
                        type = 'bar', orientation = 'h',
                        color = ~variable, colors = colors_3,
                        text = ~round(mean_value, 1),
                        hoverinfo = 'text',
                        hovertemplate = "<b>%{y}</b><br>%{meta}<br>%{x:.1f}<extra></extra>",
                        meta = ~variable,
                        textposition = 'inside',
                        insidetextfont = list(color = 'white')) %>%
  layout(xaxis = list(title = "Mean Value, 7-Point Scale"), yaxis = list(title = ""), barmode = 'group', margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(wtt_edu_plot, "images/attitudes/wtt_edu_plot.png", scale = 8)

### WTT by ses

# Extract columns starting with "WTPATT_" and include "ses"
wtpatt_ses <- pbff %>%
  select(ses_desc, starts_with("WTPATT_"))

# Rename columns starting with "WTPATT_" only, leaving "ses" intact
for (col_name in names(wtpatt_ses)[-1]) {  # Exclude "ses_desc" from renaming
  label <- attr(wtpatt_ses[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(wtpatt_ses)[names(wtpatt_ses) == col_name] <- label
  }
}

# Step 1: Aggregate data to calculate mean values per country and variable
wtt_mean_ses <- wtpatt_ses %>%
  group_by(ses_desc) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -ses_desc, names_to = "variable", values_to = "mean_value")

# Clean labels for variables
wtt_mean_ses$variable <- wtt_mean_ses$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would be willing to try a product that is prepared through ", "") %>%
  str_replace_all("I would be willing to try a product that is prepared though ", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Step 2: Plot horizontal bars representing mean values per variable grouped by country
wtt_ses_plot <- plot_ly(wtt_mean_ses, x = ~mean_value, y = ~as_factor(ses_desc),
                        type = 'bar', orientation = 'h',
                        color = ~variable, colors = colors_3,
                        text = ~round(mean_value, 1),
                        hoverinfo = 'text',
                        hovertemplate = "<b>%{y}</b><br>%{meta}<br>%{x:.1f}<extra></extra>",
                        meta = ~variable,
                        textposition = 'inside',
                        insidetextfont = list(color = 'white')) %>%
  layout(xaxis = list(title = "Mean Value, 7-Point Scale"), yaxis = list(title = ""), barmode = 'group', margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(wtt_ses_plot, "images/attitudes/wtt_ses_plot.png", scale = 8)


### Familiarity with raw materials

att1_data <- pbff %>%
  select(starts_with("ATT1_"))

for (col_name in names(att1_data)) {
  label <- attr(att1_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(att1_data)[names(att1_data) == col_name] <- label
  }
}

att1_data_long <- att1_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
att1_data_long$value_desc <- as_factor(att1_data_long$value)
att1_data_long$variable <- att1_data_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  trimws() %>%
  str_wrap(width = 30)

# Aggregate and calculate percentages
att1_data_agg <- att1_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c(1:4) & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
att1_plot <- plot_ly(att1_data_agg, x = ~percentage, y = ~variable,
                    type = 'bar', color = ~value_desc, colors = colors_4,
                    orientation = 'h',
                    text = ~text_label,
                    hoverinfo = 'text',
                    hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                    meta = ~value_desc,
                    textposition = 'inside',
                    insidetextanchor = 'middle',
                    insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
att1_plot_print <- att1_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.025, xanchor = 'auto', y = 1.1, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(att1_plot_print, "images/attitudes/att1_plot.png", scale = 8, width = 1200, height = 800)


#### By Country (Individual country plots)

att1_data_co <- pbff %>%
  select(Country, starts_with("ATT1_"))

# Convert Country to factor
att1_data_co$Country <- as_factor(att1_data_co$Country)

# Loop through columns and rename them based on their label attribute, excluding the Country column
for (col_name in names(att1_data_co)) {
  if (col_name != "Country") {
    label <- attr(att1_data_co[[col_name]], "label")
    if (!is.null(label) && label != "") {
      names(att1_data_co)[names(att1_data_co) == col_name] <- label
    }
  }
}

att1_data_co_long <- att1_data_co %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -c(id, Country), names_to = "variable", values_to = "value")

# Get labels and clean them
att1_data_co_long$value_desc <- as_factor(att1_data_co_long$value)
att1_data_co_long$variable <- att1_data_co_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  trimws() %>%
  str_wrap(width = 30)

# Aggregate and calculate percentages
att1_data_co_agg <- att1_data_co_long %>%
  group_by(Country, variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Country, variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c(1:4) & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(Country, desc(value), desc(percentage))

# Initialize list to store plots
att1_co_plot <- list()

# Generate plots for each country
unique_countries <- levels(att1_data_co$Country)

for (country in unique_countries) {
  country_data <- att1_data_co_agg %>% filter(Country == country)
  
  plot <- plot_ly(country_data, x = ~percentage, y = ~variable,
                  type = 'bar', color = ~value_desc, colors = colors_4,
                  orientation = 'h',
                  text = ~text_label,
                  hoverinfo = 'text',
                  hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                  meta = ~value_desc,
                  textposition = 'inside',
                  insidetextanchor = 'middle',
                  insidetextfont = list(color = 'white')) %>%
    layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
    config(displayModeBar = FALSE, displaylogo = FALSE)
  
  att1_co_plot[[as.character(country)]] <- plot
}

# Access the plot for a specific country
# att1_co_plot["Italy"]


#### By Country (Dropdown menu)

att1_data_dd <- pbff %>%
  select(starts_with("ATT1_"))

for (col_name in names(att1_data_dd)) {
  label <- attr(att1_data_dd[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(att1_data_dd)[names(att1_data_dd) == col_name] <- label
  }
}

att1_data_dd_long <- att1_data_dd %>%
  mutate(id = row_number(), Country = as_factor(pbff$Country)) %>%
  pivot_longer(cols = -c(id, Country), names_to = "variable", values_to = "value")

# Get labels and clean them
att1_data_dd_long$value_desc <- as_factor(att1_data_dd_long$value)
att1_data_dd_long$variable <- att1_data_dd_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  trimws() %>%
  str_wrap(width = 30)

# Create a function to aggregate and calculate percentages
aggregate_data <- function(data) {
  data %>%
    group_by(variable, value, value_desc) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(variable) %>%
    mutate(percentage = count / sum(count) * 100,
           text_label = ifelse(value %in% c(1:4) & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
    arrange(desc(value), desc(percentage))
}

# List of countries including "All"
countries <- c("All", levels(att1_data_dd_long$Country))

# Create traces for each country including "All"
traces <- list()
for (i in seq_along(countries)) {
  country <- countries[i]
  if (country == "All") {
    country_data <- att1_data_dd_long
  } else {
    country_data <- att1_data_dd_long %>% filter(Country == country)
  }
  country_data_agg <- aggregate_data(country_data)
  
  value_levels <- levels(att1_data_dd_long$value_desc)
  
  for (value_desc in value_levels) {
    value_data <- country_data_agg %>% filter(value_desc == !!value_desc)
    if (nrow(value_data) == 0) next
    
    value_index <- (match(value_desc, value_levels) - 1) %% length(colors_4) + 1
    color <- colors_4[value_index]
    
    trace <- list(
      x = value_data$percentage,
      y = value_data$variable,
      type = 'bar',
      orientation = 'h',
      name = as.character(value_desc),
      marker = list(color = color),
      text = value_data$text_label,
      textposition = 'inside',
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
    args = list(list(visible = rep(i == seq_along(countries), each = length(levels(att1_data_dd_long$value_desc))))),
    label = countries[i]
  )
})

# Create the plot
att1_plot_dd <- plot_ly()

for (trace in traces) {
  att1_plot_dd <- add_trace(att1_plot_dd, x = trace$x, y = trace$y, type = trace$type, orientation = trace$orientation,
                            marker = trace$marker, name = trace$name, text = trace$text, textposition = trace$textposition,
                            insidetextanchor = trace$insidetextanchor, insidetextfont = trace$insidetextfont,
                            hoverinfo = trace$hoverinfo, hovertemplate = trace$hovertemplate, meta = trace$meta,
                            visible = trace$visible)
}

att1_plot_dd <- att1_plot_dd %>%
  layout(
    barmode = 'stack',
    xaxis = list(title = ""),
    yaxis = list(title = "", categoryorder = "trace"),
    updatemenus = list(list(
      active = 0,
      buttons = dropdown_buttons,
      x = 0.5,
      xanchor = 'center',
      y = 1.2,
      yanchor = 'top'
    )),
    margin = list(pad = 4)
  ) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)


### Culture

att2_data <- pbff %>%
  select(starts_with("ATT2_"))

for (col_name in names(att2_data)) {
  label <- attr(att2_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(att2_data)[names(att2_data) == col_name] <- label
  }
}

att2_data_long <- att2_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
att2_data_long$value_desc <- as_factor(att2_data_long$value)
att2_data_long$variable <- att2_data_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  trimws() %>%
  str_wrap(width = 30)

# Aggregate and calculate percentages
att2_data_agg <- att2_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
att2_plot <- plot_ly(att2_data_agg, x = ~percentage, y = ~variable,
                     type = 'bar', color = ~value_desc, colors = colors_7,
                     orientation = 'h',
                     text = ~text_label,
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'inside',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
att2_plot_print <- att2_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.325, xanchor = 'auto', y = 1.075, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(att2_plot_print, "images/attitudes/att2_plot.png", scale = 8, width = 1200, height = 800)


#### By Country (Dropdown menu)

att2_data_dd <- pbff %>%
  select(starts_with("ATT2_"))

for (col_name in names(att2_data_dd)) {
  label <- attr(att2_data_dd[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(att2_data_dd)[names(att2_data_dd) == col_name] <- label
  }
}

att2_data_dd_long <- att2_data_dd %>%
  mutate(id = row_number(), Country = as_factor(pbff$Country)) %>%
  pivot_longer(cols = -c(id, Country), names_to = "variable", values_to = "value")

# Get labels and clean them
att2_data_dd_long$value_desc <- as_factor(att2_data_dd_long$value)
att2_data_dd_long$variable <- att2_data_dd_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  trimws() %>%
  str_wrap(width = 30)

# Create a function to aggregate and calculate percentages
aggregate_data <- function(data) {
  data %>%
    group_by(variable, value, value_desc) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(variable) %>%
    mutate(percentage = count / sum(count) * 100,
           text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
    arrange(desc(value), desc(percentage))
}

# List of countries including "All"
countries <- c("All", levels(att2_data_dd_long$Country))

# Create traces for each country including "All"
traces <- list()
for (i in seq_along(countries)) {
  country <- countries[i]
  if (country == "All") {
    country_data <- att2_data_dd_long
  } else {
    country_data <- att2_data_dd_long %>% filter(Country == country)
  }
  country_data_agg <- aggregate_data(country_data)
  
  value_levels <- levels(att2_data_dd_long$value_desc)
  
  for (value_desc in value_levels) {
    value_data <- country_data_agg %>% filter(value_desc == !!value_desc)
    if (nrow(value_data) == 0) next
    
    value_index <- (match(value_desc, value_levels) - 1) %% length(colors_7) + 1
    color <- colors_7[value_index]
    
    trace <- list(
      x = value_data$percentage,
      y = value_data$variable,
      type = 'bar',
      orientation = 'h',
      name = as.character(value_desc),
      marker = list(color = color),
      text = value_data$text_label,
      textposition = 'inside',
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
    args = list(list(visible = rep(i == seq_along(countries), each = length(levels(att2_data_dd_long$value_desc))))),
    label = countries[i]
  )
})

# Create the plot
att2_plot_dd <- plot_ly()

for (trace in traces) {
  att2_plot_dd <- add_trace(att2_plot_dd, x = trace$x, y = trace$y, type = trace$type, orientation = trace$orientation,
                            marker = trace$marker, name = trace$name, text = trace$text, textposition = trace$textposition,
                            insidetextanchor = trace$insidetextanchor, insidetextfont = trace$insidetextfont,
                            hoverinfo = trace$hoverinfo, hovertemplate = trace$hovertemplate, meta = trace$meta,
                            visible = trace$visible)
}

att2_plot_dd <- att2_plot_dd %>%
  layout(
    barmode = 'stack',
    xaxis = list(title = ""),
    yaxis = list(title = "", categoryorder = "trace"),
    updatemenus = list(list(
      active = 0,
      buttons = dropdown_buttons,
      x = 0.5,
      xanchor = 'center',
      y = 1.2,
      yanchor = 'top'
    )),
    margin = list(pad = 4)
  ) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)


### WTT Drinks

att4_data <- pbff %>%
  select(starts_with("ATT4_"))

for (col_name in names(att4_data)) {
  label <- attr(att4_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(att4_data)[names(att4_data) == col_name] <- label
  }
}

att4_data_long <- att4_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
att4_data_long$value_desc <- as_factor(att4_data_long$value)
att4_data_long$variable <- att4_data_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would.*", "") %>%
  trimws() %>%
  str_wrap(width = 30)

# Aggregate and calculate percentages
att4_data_agg <- att4_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
att4_plot <- plot_ly(att4_data_agg, x = ~percentage, y = ~variable,
                     type = 'bar', color = ~value_desc, colors = colors_7,
                     orientation = 'h',
                     text = ~text_label,
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'inside',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
att4_plot_print <- att4_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.1, xanchor = 'auto', y = 1.1, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(att4_plot_print, "images/attitudes/att4_plot.png", scale = 8, width = 1200, height = 800)

### WTT Yogurt Alternative

att5_data <- pbff %>%
  select(starts_with("ATT5_"))

for (col_name in names(att5_data)) {
  label <- attr(att5_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(att5_data)[names(att5_data) == col_name] <- label
  }
}

att5_data_long <- att5_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
att5_data_long$value_desc <- as_factor(att5_data_long$value)
att5_data_long$variable <- att5_data_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would.*", "") %>%
  trimws() %>%
  str_wrap(width = 30)

# Aggregate and calculate percentages
att5_data_agg <- att5_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
att5_plot <- plot_ly(att5_data_agg, x = ~percentage, y = ~variable,
                     type = 'bar', color = ~value_desc, colors = colors_7,
                     orientation = 'h',
                     text = ~text_label,
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'inside',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
att5_plot_print <- att5_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.1, xanchor = 'auto', y = 1.1, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(att5_plot_print, "images/attitudes/att5_plot.png", scale = 8, width = 1200, height = 800)

### WTT Meat Alternative

att6_data <- pbff %>%
  select(starts_with("ATT6_"))

for (col_name in names(att6_data)) {
  label <- attr(att6_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(att6_data)[names(att6_data) == col_name] <- label
  }
}

att6_data_long <- att6_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
att6_data_long$value_desc <- as_factor(att6_data_long$value)
att6_data_long$variable <- att6_data_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would.*", "") %>%
  trimws() %>%
  str_wrap(width = 30)

# Aggregate and calculate percentages
att6_data_agg <- att6_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
att6_plot <- plot_ly(att6_data_agg, x = ~percentage, y = ~variable,
                     type = 'bar', color = ~value_desc, colors = colors_7,
                     orientation = 'h',
                     text = ~text_label,
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'inside',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
att6_plot_print <- att6_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.1, xanchor = 'auto', y = 1.1, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(att6_plot_print, "images/attitudes/att6_plot.png", scale = 8, width = 1200, height = 800)

### WTT Protein-enriched bread

att7_data <- pbff %>%
  select(starts_with("ATT7_"))

for (col_name in names(att7_data)) {
  label <- attr(att7_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(att7_data)[names(att7_data) == col_name] <- label
  }
}

att7_data_long <- att7_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
att7_data_long$value_desc <- as_factor(att7_data_long$value)
att7_data_long$variable <- att7_data_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would.*", "") %>%
  trimws() %>%
  str_wrap(width = 30)

# Aggregate and calculate percentages
att7_data_agg <- att7_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
att7_plot <- plot_ly(att7_data_agg, x = ~percentage, y = ~variable,
                     type = 'bar', color = ~value_desc, colors = colors_7,
                     orientation = 'h',
                     text = ~text_label,
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'inside',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
att7_plot_print <- att7_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.1, xanchor = 'auto', y = 1.1, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(att7_plot_print, "images/attitudes/att7_plot.png", scale = 8, width = 1200, height = 800)

### Trust

tru_data <- pbff %>%
  select(starts_with("TRU_"))

for (col_name in names(tru_data)) {
  label <- attr(tru_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(tru_data)[names(tru_data) == col_name] <- label
  }
}

tru_data_long <- tru_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
tru_data_long$value_desc <- as_factor(tru_data_long$value)
tru_data_long$variable <- tru_data_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would.*", "") %>%
  trimws() %>%
  str_wrap(width = 42)

# Aggregate and calculate percentages
tru_data_agg <- tru_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
tru_plot <- plot_ly(tru_data_agg, x = ~percentage, y = ~variable,
                    type = 'bar', color = ~value_desc, colors = colors_7,
                    orientation = 'h',
                    text = ~text_label,
                    hoverinfo = 'text',
                    hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                    meta = ~value_desc,
                    textposition = 'inside',
                    insidetextanchor = 'middle',
                    insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
tru_plot_print <- tru_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.2, xanchor = 'auto', y = 1.1, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(tru_plot_print, "images/attitudes/tru_plot.png", scale = 8, width = 1200, height = 800)


#### By Country (Dropdown menu)

tru_data_dd <- pbff %>%
  select(starts_with("TRU_"))

for (col_name in names(tru_data_dd)) {
  label <- attr(tru_data_dd[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(tru_data_dd)[names(tru_data_dd) == col_name] <- label
  }
}

tru_data_dd_long <- tru_data_dd %>%
  mutate(id = row_number(), Country = as_factor(pbff$Country)) %>%
  pivot_longer(cols = -c(id, Country), names_to = "variable", values_to = "value")

# Get labels and clean them
tru_data_dd_long$value_desc <- as_factor(tru_data_dd_long$value)
tru_data_dd_long$variable <- tru_data_dd_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would.*", "") %>%
  trimws() %>%
  str_wrap(width = 42)

# Create a function to aggregate and calculate percentages
aggregate_data <- function(data) {
  data %>%
    group_by(variable, value, value_desc) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(variable) %>%
    mutate(percentage = count / sum(count) * 100,
           text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
    arrange(desc(value), desc(percentage))
}

# List of countries including "All"
countries <- c("All", levels(tru_data_dd_long$Country))

# Create traces for each country including "All"
traces <- list()
for (i in seq_along(countries)) {
  country <- countries[i]
  if (country == "All") {
    country_data <- tru_data_dd_long
  } else {
    country_data <- tru_data_dd_long %>% filter(Country == country)
  }
  country_data_agg <- aggregate_data(country_data)
  
  value_levels <- levels(tru_data_dd_long$value_desc)
  
  for (value_desc in value_levels) {
    value_data <- country_data_agg %>% filter(value_desc == !!value_desc)
    if (nrow(value_data) == 0) next
    
    value_index <- (match(value_desc, value_levels) - 1) %% length(colors_7) + 1
    color <- colors_7[value_index]
    
    trace <- list(
      x = value_data$percentage,
      y = value_data$variable,
      type = 'bar',
      orientation = 'h',
      name = as.character(value_desc),
      marker = list(color = color),
      text = value_data$text_label,
      textposition = 'inside',
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
    args = list(list(visible = rep(i == seq_along(countries), each = length(levels(tru_data_dd_long$value_desc))))),
    label = countries[i]
  )
})

# Create the plot
tru_plot_dd <- plot_ly()

for (trace in traces) {
  tru_plot_dd <- add_trace(tru_plot_dd, x = trace$x, y = trace$y, type = trace$type, orientation = trace$orientation,
                           marker = trace$marker, name = trace$name, text = trace$text, textposition = trace$textposition,
                           insidetextanchor = trace$insidetextanchor, insidetextfont = trace$insidetextfont,
                           hoverinfo = trace$hoverinfo, hovertemplate = trace$hovertemplate, meta = trace$meta,
                           visible = trace$visible)
}

tru_plot_dd <- tru_plot_dd %>%
  layout(
    barmode = 'stack',
    xaxis = list(title = ""),
    yaxis = list(title = "", categoryorder = "trace"),
    updatemenus = list(list(
      active = 0,
      buttons = dropdown_buttons,
      x = 0.5,
      xanchor = 'center',
      y = 1.2,
      yanchor = 'top'
    )),
    margin = list(pad = 4)
  ) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)


### Barriers

barriers_data <- pbff %>%
  select(starts_with("barriers_"))

for (col_name in names(barriers_data)) {
  label <- attr(barriers_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(barriers_data)[names(barriers_data) == col_name] <- label
  }
}

barriers_data_long <- barriers_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
barriers_data_long$value_desc <- as_factor(barriers_data_long$value)
barriers_data_long$variable <- barriers_data_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
barriers_data_agg <- barriers_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
barriers_plot <- plot_ly(barriers_data_agg, x = ~percentage, y = ~variable,
                         type = 'bar', color = ~value_desc, colors = colors_7,
                         orientation = 'h',
                         text = ~text_label,
                         hoverinfo = 'text',
                         hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                         meta = ~value_desc,
                         textposition = 'inside',
                         insidetextanchor = 'middle',
                         insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
barriers_plot_print <- barriers_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.375, xanchor = 'auto', y = 1.075, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(barriers_plot_print, "images/attitudes/barriers_plot.png", scale = 8, width = 1200, height = 1000)


#### By Country (Dropdown menu)

barriers_data_dd <- pbff %>%
  select(starts_with("barriers_"))

for (col_name in names(barriers_data_dd)) {
  label <- attr(barriers_data_dd[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(barriers_data_dd)[names(barriers_data_dd) == col_name] <- label
  }
}

barriers_data_dd_long <- barriers_data_dd %>%
  mutate(id = row_number(), Country = as_factor(pbff$Country)) %>%
  pivot_longer(cols = -c(id, Country), names_to = "variable", values_to = "value")

# Get labels and clean them
barriers_data_dd_long$value_desc <- as_factor(barriers_data_dd_long$value)
barriers_data_dd_long$variable <- barriers_data_dd_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  trimws() %>%
  str_wrap(width = 60)

# Create a function to aggregate and calculate percentages
aggregate_data <- function(data) {
  data %>%
    group_by(variable, value, value_desc) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(variable) %>%
    mutate(percentage = count / sum(count) * 100,
           text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
    arrange(desc(value), desc(percentage))
}

# List of countries including "All"
countries <- c("All", levels(barriers_data_dd_long$Country))

# Create traces for each country including "All"
traces <- list()
for (i in seq_along(countries)) {
  country <- countries[i]
  if (country == "All") {
    country_data <- barriers_data_dd_long
  } else {
    country_data <- barriers_data_dd_long %>% filter(Country == country)
  }
  country_data_agg <- aggregate_data(country_data)
  
  value_levels <- levels(barriers_data_dd_long$value_desc)
  
  for (value_desc in value_levels) {
    value_data <- country_data_agg %>% filter(value_desc == !!value_desc)
    if (nrow(value_data) == 0) next
    
    value_index <- (match(value_desc, value_levels) - 1) %% length(colors_7) + 1
    color <- colors_7[value_index]
    
    trace <- list(
      x = value_data$percentage,
      y = value_data$variable,
      type = 'bar',
      orientation = 'h',
      name = as.character(value_desc),
      marker = list(color = color),
      text = value_data$text_label,
      textposition = 'inside',
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
    args = list(list(visible = rep(i == seq_along(countries), each = length(levels(barriers_data_dd_long$value_desc))))),
    label = countries[i]
  )
})

# Create the plot
barriers_plot_dd <- plot_ly()

for (trace in traces) {
  barriers_plot_dd <- add_trace(barriers_plot_dd, x = trace$x, y = trace$y, type = trace$type, orientation = trace$orientation,
                                marker = trace$marker, name = trace$name, text = trace$text, textposition = trace$textposition,
                                insidetextanchor = trace$insidetextanchor, insidetextfont = trace$insidetextfont,
                                hoverinfo = trace$hoverinfo, hovertemplate = trace$hovertemplate, meta = trace$meta,
                                visible = trace$visible)
}

barriers_plot_dd <- barriers_plot_dd %>%
  layout(
    barmode = 'stack',
    xaxis = list(title = ""),
    yaxis = list(title = "", categoryorder = "trace"),
    updatemenus = list(list(
      active = 0,
      buttons = dropdown_buttons,
      x = 1.25,
      xanchor = 'right',
      y = 0.5,
      yanchor = 'middle'
    )),
    margin = list(pad = 4)
  ) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)



### Intentions

q36_data <- pbff %>%
  select(starts_with("q36_"))

for (col_name in names(q36_data)) {
  label <- attr(q36_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q36_data)[names(q36_data) == col_name] <- label
  }
}

q36_data_long <- q36_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q36_data_long$value_desc <- as_factor(q36_data_long$value)
q36_data_long$variable <- q36_data_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would.*", "") %>%
  trimws() %>%
  str_wrap(width = 42)

# Aggregate and calculate percentages
q36_data_agg <- q36_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1','2','6','7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
q36_plot <- plot_ly(q36_data_agg, x = ~percentage, y = ~variable,
                    type = 'bar', color = ~value_desc, colors = colors_7,
                    orientation = 'h',
                    text = ~text_label,
                    hoverinfo = 'text',
                    hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                    meta = ~value_desc,
                    textposition = 'inside',
                    insidetextanchor = 'middle',
                    insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q36_plot_print <- q36_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.25, xanchor = 'auto', y = 1.075, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(q36_plot_print, "images/attitudes/q36_plot.png", scale = 8, width = 1200, height = 800)

#### By Country (Dropdown menu)

q36_data_dd <- pbff %>%
  select(starts_with("q36_"))

for (col_name in names(q36_data_dd)) {
  label <- attr(q36_data_dd[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q36_data_dd)[names(q36_data_dd) == col_name] <- label
  }
}

q36_data_dd_long <- q36_data_dd %>%
  mutate(id = row_number(), Country = as_factor(pbff$Country)) %>%
  pivot_longer(cols = -c(id, Country), names_to = "variable", values_to = "value")

# Get labels and clean them
q36_data_dd_long$value_desc <- as_factor(q36_data_dd_long$value)
q36_data_dd_long$variable <- q36_data_dd_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("I would.*", "") %>%
  trimws() %>%
  str_wrap(width = 42)

# Create a function to aggregate and calculate percentages
aggregate_data <- function(data) {
  data %>%
    group_by(variable, value, value_desc) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(variable) %>%
    mutate(percentage = count / sum(count) * 100,
           text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
    arrange(desc(value), desc(percentage))
}

# List of countries including "All"
countries <- c("All", levels(q36_data_dd_long$Country))

# Create traces for each country including "All"
traces <- list()
for (i in seq_along(countries)) {
  country <- countries[i]
  if (country == "All") {
    country_data <- q36_data_dd_long
  } else {
    country_data <- q36_data_dd_long %>% filter(Country == country)
  }
  country_data_agg <- aggregate_data(country_data)
  
  value_levels <- levels(q36_data_dd_long$value_desc)
  
  for (value_desc in value_levels) {
    value_data <- country_data_agg %>% filter(value_desc == !!value_desc)
    if (nrow(value_data) == 0) next
    
    value_index <- (match(value_desc, value_levels) - 1) %% length(colors_7) + 1
    color <- colors_7[value_index]
    
    trace <- list(
      x = value_data$percentage,
      y = value_data$variable,
      type = 'bar',
      orientation = 'h',
      name = as.character(value_desc),
      marker = list(color = color),
      text = value_data$text_label,
      textposition = 'inside',
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
    args = list(list(visible = rep(i == seq_along(countries), each = length(levels(q36_data_dd_long$value_desc))))),
    label = countries[i]
  )
})

# Create the plot
q36_plot_dd <- plot_ly()

for (trace in traces) {
  q36_plot_dd <- add_trace(q36_plot_dd, x = trace$x, y = trace$y, type = trace$type, orientation = trace$orientation,
                           marker = trace$marker, name = trace$name, text = trace$text, textposition = trace$textposition,
                           insidetextanchor = trace$insidetextanchor, insidetextfont = trace$insidetextfont,
                           hoverinfo = trace$hoverinfo, hovertemplate = trace$hovertemplate, meta = trace$meta,
                           visible = trace$visible)
}

q36_plot_dd <- q36_plot_dd %>%
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


## Likelihood to try

q38_data <- pbff %>%
  select(starts_with("q38_"))

for (col_name in names(q38_data)) {
  label <- attr(q38_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q38_data)[names(q38_data) == col_name] <- label
  }
}

q38_data_long <- q38_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q38_data_long$value_desc <- as_factor(q38_data_long$value)
q38_data_long$variable <- q38_data_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("Imagine.*", "") %>%
  trimws() %>%
  str_wrap(width = 46)

# Aggregate and calculate percentages
q38_data_agg <- q38_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c(1:5) & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
q38_plot <- plot_ly(q38_data_agg, x = ~percentage, y = ~variable,
                    type = 'bar', color = ~value_desc, colors = colors_7,
                    orientation = 'h',
                    text = ~text_label,
                    hoverinfo = 'text',
                    hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                    meta = ~value_desc,
                    textposition = 'inside',
                    insidetextanchor = 'middle',
                    insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q38_plot_print <- q38_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = 0.085, xanchor = 'auto', y = 1.075, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(q38_plot_print, "images/attitudes/q38_plot.png", scale = 8, width = 1200, height = 800)


#### By Country (Dropdown menu)

q38_data_dd <- pbff %>%
  select(starts_with("q38_"))

for (col_name in names(q38_data_dd)) {
  label <- attr(q38_data_dd[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q38_data_dd)[names(q38_data_dd) == col_name] <- label
  }
}

q38_data_dd_long <- q38_data_dd %>%
  mutate(id = row_number(), Country = as_factor(pbff$Country)) %>%
  pivot_longer(cols = -c(id, Country), names_to = "variable", values_to = "value")

# Get labels and clean them
q38_data_dd_long$value_desc <- as_factor(q38_data_dd_long$value)
q38_data_dd_long$variable <- q38_data_dd_long$variable %>%
  str_replace_all("\u00A0", "") %>%
  str_replace_all("\\[", "") %>%
  str_replace_all("\\]", "") %>%
  str_replace_all("\\([^\\)]*\\)", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("  ", " ") %>%
  str_replace_all("Please.*", "") %>%
  str_replace_all("Imagine.*", "") %>%
  trimws() %>%
  str_wrap(width = 46)

# Create a function to aggregate and calculate percentages
aggregate_data <- function(data) {
  data %>%
    group_by(variable, value, value_desc) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(variable) %>%
    mutate(percentage = count / sum(count) * 100,
           text_label = ifelse(value %in% c(1:5) & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
    arrange(desc(value), desc(percentage))
}

# List of countries including "All"
countries <- c("All", levels(q38_data_dd_long$Country))

# Create traces for each country including "All"
traces <- list()
for (i in seq_along(countries)) {
  country <- countries[i]
  if (country == "All") {
    country_data <- q38_data_dd_long
  } else {
    country_data <- q38_data_dd_long %>% filter(Country == country)
  }
  country_data_agg <- aggregate_data(country_data)
  
  value_levels <- levels(q38_data_dd_long$value_desc)
  
  for (value_desc in value_levels) {
    value_data <- country_data_agg %>% filter(value_desc == !!value_desc)
    if (nrow(value_data) == 0) next
    
    value_index <- (match(value_desc, value_levels) - 1) %% length(colors_5) + 1
    color <- colors_5[value_index]
    
    trace <- list(
      x = value_data$percentage,
      y = value_data$variable,
      type = 'bar',
      orientation = 'h',
      name = as.character(value_desc),
      marker = list(color = color),
      text = value_data$text_label,
      textposition = 'inside',
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
    args = list(list(visible = rep(i == seq_along(countries), each = length(levels(q38_data_dd_long$value_desc))))),
    label = countries[i]
  )
})

# Create the plot
q38_plot_dd <- plot_ly()

for (trace in traces) {
  q38_plot_dd <- add_trace(q38_plot_dd, x = trace$x, y = trace$y, type = trace$type, orientation = trace$orientation,
                           marker = trace$marker, name = trace$name, text = trace$text, textposition = trace$textposition,
                           insidetextanchor = trace$insidetextanchor, insidetextfont = trace$insidetextfont,
                           hoverinfo = trace$hoverinfo, hovertemplate = trace$hovertemplate, meta = trace$meta,
                           visible = trace$visible)
}

q38_plot_dd <- q38_plot_dd %>%
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



## Price conditions

#### Meat

q39_1_data <- pbff %>%
  select(starts_with("q39_1_"))

for (col_name in names(q39_1_data)) {
  label <- attr(q39_1_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q39_1_data)[names(q39_1_data) == col_name] <- label
  }
}

q39_1_data_long <- q39_1_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q39_1_data_long$value_desc <- as_factor(q39_1_data_long$value)
q39_1_data_long$variable <- q39_1_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Plant-Based Fermented meat alternative", "") %>%
  str_replace_all("In which.*", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q39_1_data_agg <- q39_1_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
q39_1_plot <- plot_ly(q39_1_data_agg, x = ~percentage, y = ~variable,
                      type = 'bar', color = ~value_desc, colors = rev(colors_2),
                      orientation = 'h',
                      text = ~text_label,
                      hoverinfo = 'text',
                      hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                      meta = ~value_desc,
                      textposition = 'inside',
                      insidetextanchor = 'middle',
                      insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "Percentage"), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(q39_1_plot, "images/attitudes/q39_1_plot.png", scale = 8)


#### Yogurt

q39_2_data <- pbff %>%
  select(starts_with("q39_2_"))

for (col_name in names(q39_2_data)) {
  label <- attr(q39_2_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q39_2_data)[names(q39_2_data) == col_name] <- label
  }
}

q39_2_data_long <- q39_2_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q39_2_data_long$value_desc <- as_factor(q39_2_data_long$value)
q39_2_data_long$variable <- q39_2_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Plant-Based Fermented yogurt alternative", "") %>%
  str_replace_all("In which.*", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q39_2_data_agg <- q39_2_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
q39_2_plot <- plot_ly(q39_2_data_agg, x = ~percentage, y = ~variable,
                      type = 'bar', color = ~value_desc, colors = rev(colors_2),
                      orientation = 'h',
                      text = ~text_label,
                      hoverinfo = 'text',
                      hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                      meta = ~value_desc,
                      textposition = 'inside',
                      insidetextanchor = 'middle',
                      insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "Percentage"), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(q39_2_plot, "images/attitudes/q39_2_plot.png", scale = 8)

#### Milk

q39_3_data <- pbff %>%
  select(starts_with("q39_3_"))

for (col_name in names(q39_3_data)) {
  label <- attr(q39_3_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q39_3_data)[names(q39_3_data) == col_name] <- label
  }
}

q39_3_data_long <- q39_3_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q39_3_data_long$value_desc <- as_factor(q39_3_data_long$value)
q39_3_data_long$variable <- q39_3_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Plant-Based Fermented milk alternative", "") %>%
  str_replace_all("In which.*", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q39_3_data_agg <- q39_3_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
q39_3_plot <- plot_ly(q39_3_data_agg, x = ~percentage, y = ~variable,
                      type = 'bar', color = ~value_desc, colors = rev(colors_2),
                      orientation = 'h',
                      text = ~text_label,
                      hoverinfo = 'text',
                      hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                      meta = ~value_desc,
                      textposition = 'inside',
                      insidetextanchor = 'middle',
                      insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "Percentage"), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(q39_3_plot, "images/attitudes/q39_3_plot.png", scale = 8)

### Price conditions combined plot

q39_1_data_agg$category <- "Meat"
q39_2_data_agg$category <- "Yogurt"
q39_3_data_agg$category <- "Milk"

price_cond_combined_data <- bind_rows(q39_1_data_agg, q39_2_data_agg, q39_3_data_agg) %>%
  filter(value == 1)

price_cond_combined_data$variable <- price_cond_combined_data$variable %>%
  str_replace_all("If their", "") %>%
  str_replace_all("If the", "") %>%
  trimws()

price_cond_plot <- plot_ly(data = price_cond_combined_data, x = ~variable, y = ~percentage,
                type = 'bar', color = ~category, colors = rev(colors_3),
                split = ~category,
                hoverinfo = 'text',
                hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                meta = ~value_desc,
                text = ~text_label,
                textposition = 'inside',
                insidetextanchor = 'middle',
                insidetextfont = list(color = 'white')) %>%
  layout(yaxis = list(title = ""), xaxis = list(title = ""),
         barmode = 'group', margin = list(pad=4),
         legend = list(traceorder = 'reversed')) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
save_image(price_cond_plot, "images/attitudes/price_cond_plot.png", scale = 8)


### Social, Environmental and Economic Sustainability

q41_data <- pbff %>%
  select(starts_with("q41_"))

for (col_name in names(q41_data)) {
  label <- attr(q41_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q41_data)[names(q41_data) == col_name] <- label
  }
}

q41_data_long <- q41_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q41_data_long$value_desc <- as_factor(q41_data_long$value)
q41_data_long$variable <- q41_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Please.*", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  str_replace_all("\\(.*\\)", "") %>%
  str_replace_all("Plant-Based Fermented Foods ", "") %>%
  str_replace_all("The production and consumption of ", "") %>%
  str_replace_all("  ", " ") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q41_data_agg <- q41_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(desc(value), desc(percentage))

# Plot
q41_plot <- plot_ly(q41_data_agg, x = ~percentage, y = ~variable,
                    type = 'bar', color = ~value_desc, colors = colors_7,
                    orientation = 'h',
                    text = ~text_label,
                    hoverinfo = 'text',
                    hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<br>%{meta}<extra></extra>",
                    meta = ~value_desc,
                    textposition = 'inside',
                    insidetextanchor = 'middle',
                    insidetextfont = list(color = 'white')) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = ""), yaxis = list(title = "", categoryorder = "trace"), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q41_plot_print <- q41_plot %>%
  layout(margin = list(t=100),
         legend = list(orientation = 'h', x = -0.375, xanchor = 'auto', y = 1.075, yanchor='top', xref = 'paper', yref = 'container', entrywidth = 1, entrywidthmode = 'fraction', traceorder = 'normal'))
save_image(q41_plot_print, "images/attitudes/q41_plot.png", scale = 8, width = 1200, height = 800)


#### By Country (Dropdown menu)

q41_data_dd <- pbff %>%
  select(starts_with("q41_"))

for (col_name in names(q41_data_dd)) {
  label <- attr(q41_data_dd[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q41_data_dd)[names(q41_data_dd) == col_name] <- label
  }
}

q41_data_dd_long <- q41_data_dd %>%
  mutate(id = row_number(), Country = as_factor(pbff$Country)) %>%
  pivot_longer(cols = -c(id, Country), names_to = "variable", values_to = "value")

# Get labels and clean them
q41_data_dd_long$value_desc <- as_factor(q41_data_dd_long$value)
q41_data_dd_long$variable <- q41_data_dd_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Please.*", "") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  str_replace_all("\\(.*\\)", "") %>%
  str_replace_all("Plant-Based Fermented Foods ", "") %>%
  str_replace_all("The production and consumption of ", "") %>%
  str_replace_all("  ", " ") %>%
  trimws() %>%
  str_wrap(width = 60)

# Create a function to aggregate and calculate percentages
aggregate_data <- function(data) {
  data %>%
    group_by(variable, value, value_desc) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(variable) %>%
    mutate(percentage = count / sum(count) * 100,
           text_label = ifelse(value %in% c('1', '2', '6', '7') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
    arrange(desc(value), desc(percentage))
}

# List of countries including "All"
countries <- c("All", levels(q41_data_dd_long$Country))

# Create traces for each country including "All"
traces <- list()
for (i in seq_along(countries)) {
  country <- countries[i]
  if (country == "All") {
    country_data <- q41_data_dd_long
  } else {
    country_data <- q41_data_dd_long %>% filter(Country == country)
  }
  country_data_agg <- aggregate_data(country_data)
  
  value_levels <- levels(q41_data_dd_long$value_desc)
  
  for (value_desc in value_levels) {
    value_data <- country_data_agg %>% filter(value_desc == !!value_desc)
    if (nrow(value_data) == 0) next
    
    value_index <- (match(value_desc, value_levels) - 1) %% length(colors_7) + 1
    color <- colors_7[value_index]
    
    trace <- list(
      x = value_data$percentage,
      y = value_data$variable,
      type = 'bar',
      orientation = 'h',
      name = as.character(value_desc),
      marker = list(color = color),
      text = value_data$text_label,
      textposition = 'inside',
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
    args = list(list(visible = rep(i == seq_along(countries), each = length(levels(q41_data_dd_long$value_desc))))),
    label = countries[i]
  )
})

# Create the plot
q41_plot_dd <- plot_ly()

for (trace in traces) {
  q41_plot_dd <- add_trace(q41_plot_dd, x = trace$x, y = trace$y, type = trace$type, orientation = trace$orientation,
                           marker = trace$marker, name = trace$name, text = trace$text, textposition = trace$textposition,
                           insidetextanchor = trace$insidetextanchor, insidetextfont = trace$insidetextfont,
                           hoverinfo = trace$hoverinfo, hovertemplate = trace$hovertemplate, meta = trace$meta,
                           visible = trace$visible)
}

q41_plot_dd <- q41_plot_dd %>%
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


