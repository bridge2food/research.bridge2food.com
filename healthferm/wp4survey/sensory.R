
### Drinkable Yogurt

#### Appearance

q33a_data <- pbff %>%
  select(starts_with("q33a_"))

for (col_name in names(q33a_data)) {
  label <- attr(q33a_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q33a_data)[names(q33a_data) == col_name] <- label
  }
}

q33a_data_long <- q33a_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q33a_data_long$value_desc <- as_factor(q33a_data_long$value)
q33a_data_long$variable <- q33a_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Appearance", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q33a_data_agg <- q33a_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q33a_plot <- plot_ly(q33a_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar',
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_7)) %>%
  layout(margin = list(pad=4),
         barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"),
         yaxis = list(title = ""),
         margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q33a_plot_print <- q33a_plot %>%
  style(textposition = 'inside')
save_image(q33a_plot_print, "images/sensory/q33a_plot.png", scale = 8)

#### Odour

q33b_data <- pbff %>%
  select(starts_with("q33b_"))

for (col_name in names(q33b_data)) {
  label <- attr(q33b_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q33b_data)[names(q33b_data) == col_name] <- label
  }
}

q33b_data_long <- q33b_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q33b_data_long$value_desc <- as_factor(q33b_data_long$value)
q33b_data_long$variable <- q33b_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Odour", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q33b_data_agg <- q33b_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q33b_plot <- plot_ly(q33b_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar',
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_7)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q33b_plot_print <- q33b_plot %>%
  style(textposition = 'inside')
save_image(q33b_plot_print, "images/sensory/q33b_plot.png", scale = 8)

#### Taste/Flavour

q33c_data <- pbff %>%
  select(starts_with("q33c_"))

for (col_name in names(q33c_data)) {
  label <- attr(q33c_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q33c_data)[names(q33c_data) == col_name] <- label
  }
}

q33c_data_long <- q33c_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q33c_data_long$value_desc <- as_factor(q33c_data_long$value)
q33c_data_long$variable <- q33c_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Taste/flavour", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q33c_data_agg <- q33c_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q33c_plot <- plot_ly(q33c_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_8)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q33c_plot_print <- q33c_plot %>%
  style(textposition = 'inside')
save_image(q33c_plot_print, "images/sensory/q33c_plot.png", scale = 8)


#### Texture/Mouthfeel

q33d_data <- pbff %>%
  select(starts_with("q33d_"))

for (col_name in names(q33d_data)) {
  label <- attr(q33d_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q33d_data)[names(q33d_data) == col_name] <- label
  }
}

q33d_data_long <- q33d_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q33d_data_long$value_desc <- as_factor(q33d_data_long$value)
q33d_data_long$variable <- q33d_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Texture/Mouthfeel", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q33d_data_agg <- q33d_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q33d_plot <- plot_ly(q33d_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_8)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q33d_plot_print <- q33d_plot %>%
  style(textposition = 'inside')
save_image(q33d_plot_print, "images/sensory/q33d_plot.png", scale = 8)

### Yogurt

#### Appearance

q34a_data <- pbff %>%
  select(starts_with("q34a_"))

for (col_name in names(q34a_data)) {
  label <- attr(q34a_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q34a_data)[names(q34a_data) == col_name] <- label
  }
}

q34a_data_long <- q34a_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q34a_data_long$value_desc <- as_factor(q34a_data_long$value)
q34a_data_long$variable <- q34a_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Appearance", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q34a_data_agg <- q34a_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q34a_plot <- plot_ly(q34a_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_10)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q34a_plot_print <- q34a_plot %>%
  style(textposition = 'inside')
save_image(q34a_plot_print, "images/sensory/q34a_plot.png", scale = 8)


#### Odour

q34b_data <- pbff %>%
  select(starts_with("q34b_"))

for (col_name in names(q34b_data)) {
  label <- attr(q34b_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q34b_data)[names(q34b_data) == col_name] <- label
  }
}

q34b_data_long <- q34b_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q34b_data_long$value_desc <- as_factor(q34b_data_long$value)
q34b_data_long$variable <- q34b_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Odour", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q34b_data_agg <- q34b_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q34b_plot <- plot_ly(q34b_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_5)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q34b_plot_print <- q34b_plot %>%
  style(textposition = 'inside')
save_image(q34b_plot_print, "images/sensory/q34b_plot.png", scale = 8)


#### Taste/Flavour

q34c_data <- pbff %>%
  select(starts_with("q34c_"))

for (col_name in names(q34c_data)) {
  label <- attr(q34c_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q34c_data)[names(q34c_data) == col_name] <- label
  }
}

q34c_data_long <- q34c_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q34c_data_long$value_desc <- as_factor(q34c_data_long$value)
q34c_data_long$variable <- q34c_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Taste/flavour", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q34c_data_agg <- q34c_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q34c_plot <- plot_ly(q34c_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_9)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q34c_plot_print <- q34c_plot %>%
  style(textposition = 'inside')
save_image(q34c_plot_print, "images/sensory/q34c_plot.png", scale = 8)


#### Texture/Mouthfeel

q34d_data <- pbff %>%
  select(starts_with("q34d_"))

for (col_name in names(q34d_data)) {
  label <- attr(q34d_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q34d_data)[names(q34d_data) == col_name] <- label
  }
}

q34d_data_long <- q34d_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q34d_data_long$value_desc <- as_factor(q34d_data_long$value)
q34d_data_long$variable <- q34d_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Texture/Mouthfeel", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q34d_data_agg <- q34d_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q34d_plot <- plot_ly(q34d_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_10)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q34d_plot_print <- q34d_plot %>%
  style(textposition = 'inside')
save_image(q34d_plot_print, "images/sensory/q34d_plot.png", scale = 8)

### Chicken

#### Appearance

q35a_data <- pbff %>%
  select(starts_with("q35a_"))

for (col_name in names(q35a_data)) {
  label <- attr(q35a_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q35a_data)[names(q35a_data) == col_name] <- label
  }
}

q35a_data_long <- q35a_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q35a_data_long$value_desc <- as_factor(q35a_data_long$value)
q35a_data_long$variable <- q35a_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Appearance", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q35a_data_agg <- q35a_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q35a_plot <- plot_ly(q35a_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_9)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q35a_plot_print <- q35a_plot %>%
  style(textposition = 'inside')
save_image(q35a_plot_print, "images/sensory/q35a_plot.png", scale = 8)


#### Odour

q35b_data <- pbff %>%
  select(starts_with("q35b_"))

for (col_name in names(q35b_data)) {
  label <- attr(q35b_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q35b_data)[names(q35b_data) == col_name] <- label
  }
}

q35b_data_long <- q35b_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q35b_data_long$value_desc <- as_factor(q35b_data_long$value)
q35b_data_long$variable <- q35b_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Odour", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q35b_data_agg <- q35b_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q35b_plot <- plot_ly(q35b_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_7)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q35b_plot_print <- q35b_plot %>%
  style(textposition = 'inside')
save_image(q35b_plot_print, "images/sensory/q35b_plot.png", scale = 8)

#### Taste/Flavour

q35c_data <- pbff %>%
  select(starts_with("q35c_"))

for (col_name in names(q35c_data)) {
  label <- attr(q35c_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q35c_data)[names(q35c_data) == col_name] <- label
  }
}

q35c_data_long <- q35c_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q35c_data_long$value_desc <- as_factor(q35c_data_long$value)
q35c_data_long$variable <- q35c_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Taste/flavour", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q35c_data_agg <- q35c_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q35c_plot <- plot_ly(q35c_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_15)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q35c_plot_print <- q35c_plot %>%
  style(textposition = 'inside')
save_image(q35c_plot_print, "images/sensory/q35c_plot.png", scale = 8)

#### Texture/Mouthfeel

q35d_data <- pbff %>%
  select(starts_with("q35d_"))

for (col_name in names(q35d_data)) {
  label <- attr(q35d_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q35d_data)[names(q35d_data) == col_name] <- label
  }
}

q35d_data_long <- q35d_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q35d_data_long$value_desc <- as_factor(q35d_data_long$value)
q35d_data_long$variable <- q35d_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Texture/Mouthfeel", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q35d_data_agg <- q35d_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q35d_plot <- plot_ly(q35d_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_11)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q35d_plot_print <- q35a_plot %>%
  style(textposition = 'inside')
save_image(q35d_plot_print, "images/sensory/q35d_plot.png", scale = 8)

### Protein-Enriched Bread

#### Appearance

q36a_data <- pbff %>%
  select(starts_with("q36a_"))

for (col_name in names(q36a_data)) {
  label <- attr(q36a_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q36a_data)[names(q36a_data) == col_name] <- label
  }
}

q36a_data_long <- q36a_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q36a_data_long$value_desc <- as_factor(q36a_data_long$value)
q36a_data_long$variable <- q36a_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Appearance", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q36a_data_agg <- q36a_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q36a_plot <- plot_ly(q36a_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_5)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q36a_plot_print <- q36a_plot %>%
  style(textposition = 'inside')
save_image(q36a_plot_print, "images/sensory/q36a_plot.png", scale = 8)

#### Odour

q36b_data <- pbff %>%
  select(starts_with("q36b_"))

for (col_name in names(q36b_data)) {
  label <- attr(q36b_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q36b_data)[names(q36b_data) == col_name] <- label
  }
}

q36b_data_long <- q36b_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q36b_data_long$value_desc <- as_factor(q36b_data_long$value)
q36b_data_long$variable <- q36b_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Odour", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q36b_data_agg <- q36b_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q36b_plot <- plot_ly(q36b_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_5)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q36b_plot_print <- q36b_plot %>%
  style(textposition = 'inside')
save_image(q36b_plot_print, "images/sensory/q36b_plot.png", scale = 8)

#### Taste/Flavour

q36c_data <- pbff %>%
  select(starts_with("q36c_"))

for (col_name in names(q36c_data)) {
  label <- attr(q36c_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q36c_data)[names(q36c_data) == col_name] <- label
  }
}

q36c_data_long <- q36c_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q36c_data_long$value_desc <- as_factor(q36c_data_long$value)
q36c_data_long$variable <- q36c_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Taste/flavour", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q36c_data_agg <- q36c_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q36c_plot <- plot_ly(q36c_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_14)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q36c_plot_print <- q36c_plot %>%
  style(textposition = 'inside')
save_image(q36c_plot_print, "images/sensory/q36c_plot.png", scale = 8)

#### Texture/Mouthfeel

q36d_data <- pbff %>%
  select(starts_with("q36d_"))

for (col_name in names(q36d_data)) {
  label <- attr(q36d_data[[col_name]], "label")
  if (!is.null(label) && label != "") {
    names(q36d_data)[names(q36d_data) == col_name] <- label
  }
}

q36d_data_long <- q36d_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "variable", values_to = "value")

# Get labels and clean them
q36d_data_long$value_desc <- as_factor(q36d_data_long$value)
q36d_data_long$variable <- q36d_data_long$variable %>%
  str_replace_all("\\xa0", " ") %>% # remove hidden non-breaking spaces
  str_replace_all("Texture/Mouthfeel", "") %>%
  str_replace_all("\\[|\\]", "") %>%
  trimws() %>%
  str_wrap(width = 60)

# Aggregate and calculate percentages
q36d_data_agg <- q36d_data_long %>%
  group_by(variable, value, value_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(percentage = count / sum(count) * 100,
         text_label = ifelse(value %in% c('1', '2') & percentage > 5, paste0(round(percentage, 1), "%"), "")) %>%
  arrange(percentage) %>%
  filter(value == 1)

# Plot
q36d_plot <- plot_ly(q36d_data_agg, x = ~variable, y = ~percentage,
                     type = 'bar', 
                     orientation = 'v',
                     text = ~paste0(round(percentage, 1), "%"),
                     hoverinfo = 'text',
                     hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>%{meta}<extra></extra>",
                     meta = ~value_desc,
                     textposition = 'none',
                     insidetextanchor = 'middle',
                     insidetextfont = list(color = 'white'),
                     marker = list(color = colors_8)) %>%
  layout(margin = list(pad=4), barmode = 'stack', xaxis = list(title = "", categoryorder = "trace"), yaxis = list(title = ""), margin = list(pad=4)) %>%
  config(displayModeBar = FALSE, displaylogo = FALSE)
q36d_plot_print <- q36d_plot %>%
  style(textposition = 'inside')
save_image(q36d_plot_print, "images/sensory/q36d_plot.png", scale = 8)
