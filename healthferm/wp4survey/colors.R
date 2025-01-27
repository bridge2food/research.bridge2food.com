library(grDevices)

hf_colors <- c("#006e5f", "#f58b0c", "#5e0700", "#b64b2e")

# Function to generate a palette of n colors
generate_palette <- function(colors, n) {
  rev(colorRampPalette(colors)(n))
}

# Generate palettes
colors_1  <- generate_palette(hf_colors, 1)
# colors_2  <- generate_palette(hf_colors, 2)
colors_2 <- c("#006e5f", "#f58b0c")
# colors_3  <- generate_palette(hf_colors, 3)
colors_3 <- c("#006e5f", "#f58b0c", "#b64b2e")
colors_4  <- generate_palette(hf_colors, 4)
colors_5  <- generate_palette(hf_colors, 5)
colors_6  <- generate_palette(hf_colors, 6)
colors_7  <- generate_palette(hf_colors, 7)
colors_8  <- generate_palette(hf_colors, 8)
colors_8_alt <- c(colors_7, "#d66a4b")
colors_9  <- generate_palette(hf_colors, 9)
colors_9_alt <- c("#a0a0a0", colors_8)  
colors_10 <- generate_palette(hf_colors, 10)
colors_10_alt <- c(colors_9, "#ffe119")  
colors_11 <- generate_palette(hf_colors, 11)
colors_14 <- generate_palette(hf_colors, 14)
colors_15 <- generate_palette(hf_colors, 15)
