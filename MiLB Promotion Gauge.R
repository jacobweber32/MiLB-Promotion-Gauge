# Required libraries
library(tidyverse)
library(lubridate)
library(plotly)
library(zoo)
library(baseballr)

# Data Collection
Carson_McCusker_data <- fg_milb_batter_game_logs(playerid = "sa3022304", year = 2024)
# Export Game Log CSV if wanted
# write.csv(Carson_McCusker_data, "C:/Users/weber/OneDrive/Desktop/Baseball/Carson.McCusker.csv", row.names = FALSE)

# Define position adjustment factors
position_factors <- list(
  "C" = 0.85,    # Catchers get a 15% reduction in offensive requirements
  "SS" = 0.90,   # Premium defensive positions get slight reduction
  "2B" = 0.90,
  "CF" = 0.90,
  "3B" = 0.95,
  "RF" = 1.05,   # Corner outfield positions need more offense
  "LF" = 1.05,
  "1B" = 1.10,   # First base has highest offensive requirements
  "DH" = 1.15    # DH needs to really hit to promote
)

# Define average ages for each level
level_avg_ages <- c(
  "DSL" = 17.70151306,
  "Com" = 19.37089201,
  "A" = 21.02167630,
  "A+" = 21.81830743,
  "AA" = 23.89126559,
  "AAA" = 26.49371069
)

# Define level-specific thresholds
level_thresholds <- list(
  "DSL" = list(
    OPS = list(values = c(0.650, 0.700, 0.750, 0.800), scores = c(10, 15, 20, 25)),
    wRC = list(values = c(0.6, 0.8, 1.0, 1.2), scores = c(10, 15, 20, 25)),
    BB_pct = list(values = c(6, 8, 10, 12), scores = c(5, 10, 15, 20)),
    K_pct = list(values = c(30, 27, 24, 21), scores = c(5, 10, 15, 20)),
    ISO = list(values = c(0.100, 0.125, 0.150, 0.175), scores = c(5, 10, 15, 20))
  ),
  "Com" = list(
    OPS = list(values = c(0.675, 0.725, 0.775, 0.825), scores = c(10, 15, 20, 25)),
    wRC = list(values = c(0.7, 0.9, 1.1, 1.3), scores = c(10, 15, 20, 25)),
    BB_pct = list(values = c(7, 9, 11, 13), scores = c(5, 10, 15, 20)),
    K_pct = list(values = c(28, 25, 22, 19), scores = c(5, 10, 15, 20)),
    ISO = list(values = c(0.120, 0.145, 0.170, 0.195), scores = c(5, 10, 15, 20))
  ),
  "A" = list(
    OPS = list(values = c(0.700, 0.750, 0.800, 0.850), scores = c(10, 15, 20, 25)),
    wRC = list(values = c(0.8, 1.0, 1.2, 1.4), scores = c(10, 15, 20, 25)),
    BB_pct = list(values = c(7, 9, 11, 13), scores = c(5, 10, 15, 20)),
    K_pct = list(values = c(27, 24, 21, 18), scores = c(5, 10, 15, 20)),
    ISO = list(values = c(0.130, 0.155, 0.180, 0.205), scores = c(5, 10, 15, 20))
  ),
  "A+" = list(
    OPS = list(values = c(0.725, 0.775, 0.825, 0.875), scores = c(10, 15, 20, 25)),
    wRC = list(values = c(0.9, 1.1, 1.3, 1.5), scores = c(10, 15, 20, 25)),
    BB_pct = list(values = c(8, 10, 12, 14), scores = c(5, 10, 15, 20)),
    K_pct = list(values = c(26, 23, 20, 17), scores = c(5, 10, 15, 20)),
    ISO = list(values = c(0.140, 0.165, 0.190, 0.215), scores = c(5, 10, 15, 20))
  ),
  "AA" = list(
    OPS = list(values = c(0.750, 0.800, 0.850, 0.900), scores = c(10, 15, 20, 25)),
    wRC = list(values = c(1.0, 1.2, 1.4, 1.6), scores = c(10, 15, 20, 25)),
    BB_pct = list(values = c(8, 10, 12, 14), scores = c(5, 10, 15, 20)),
    K_pct = list(values = c(25, 22, 19, 16), scores = c(5, 10, 15, 20)),
    ISO = list(values = c(0.150, 0.175, 0.200, 0.225), scores = c(5, 10, 15, 20))
  ),
  "AAA" = list(
    OPS = list(values = c(0.800, 0.850, 0.900, 0.950), scores = c(10, 15, 20, 25)),
    wRC = list(values = c(1.1, 1.3, 1.5, 1.7), scores = c(10, 15, 20, 25)),
    BB_pct = list(values = c(9, 11, 13, 15), scores = c(5, 10, 15, 20)),
    K_pct = list(values = c(23, 20, 17, 14), scores = c(5, 10, 15, 20)),
    ISO = list(values = c(0.175, 0.200, 0.225, 0.250), scores = c(5, 10, 15, 20))
  )
)

# Process batter data function
process_batter_data <- function(data, level_avg_ages) {
  data %>%
    mutate(
      Date = as.Date(Date),
      Level = str_extract(Level, "\\(.*?\\)"),
      Level = str_replace_all(Level, "[()]", ""),
      # Calculate rolling stats with partial windows at end of season
      rolling_OPS = rollapply(OPS, width = 14, FUN = mean, align = "right", fill = "extend", partial = TRUE),
      rolling_wRC = rollapply(wRC, width = 14, FUN = mean, align = "right", fill = "extend", partial = TRUE),
      rolling_BB_pct = rollapply(`BB%`, width = 14, FUN = mean, align = "right", fill = "extend", partial = TRUE),
      rolling_K_pct = rollapply(`K%`, width = 14, FUN = mean, align = "right", fill = "extend", partial = TRUE),
      rolling_ISO = rollapply(ISO, width = 14, FUN = mean, align = "right", fill = "extend", partial = TRUE)
    ) %>%
    group_by(Level) %>%
    mutate(
      # Improved streak detection using rolling z-scores
      rolling_OPS_zscore = (rolling_OPS - mean(rolling_OPS, na.rm = TRUE)) / sd(rolling_OPS, na.rm = TRUE),
      hot_streak = rolling_OPS_zscore > 1,  # More than 1 SD above mean
      slump = rolling_OPS_zscore < -1,      # More than 1 SD below mean
      age_vs_level_avg = as.numeric(AgeYears) - level_avg_ages[Level]
    ) %>%
    ungroup() %>%
    select(
      Date, Level, Position, G, PA, AVG, OBP, SLG, OPS, wRC, `BB%`, `K%`, ISO,
      rolling_OPS, rolling_wRC, rolling_BB_pct, rolling_K_pct, rolling_ISO,
      hot_streak, slump, age_vs_level_avg, AgeYears
    )
}

# Check threshold function
check_threshold <- function(stat, threshold, lower_is_better = FALSE) {
  if (is.null(stat) || is.na(stat)) return(0)
  
  score <- 0
  for (i in seq_along(threshold$values)) {
    if (lower_is_better) {
      if (stat <= threshold$values[i]) score <- threshold$scores[i]
    } else {
      if (stat >= threshold$values[i]) score <- threshold$scores[i]
    }
  }
  return(score)
}

# Calculate promotion score function
calculate_promotion_score <- function(player_data, level_thresholds, position_factors) {
  score <- 0
  current_level <- player_data$Level
  thresholds <- level_thresholds[[current_level]]
  pos_factor <- position_factors[[player_data$Position]]
  
  if (is.null(pos_factor)) pos_factor <- 1  # Default factor if position not found
  
  if (!is.null(thresholds)) {
    score <- score + check_threshold(player_data$rolling_OPS, thresholds$OPS)
    score <- score + check_threshold(player_data$rolling_wRC, thresholds$wRC)
    score <- score + check_threshold(player_data$rolling_BB_pct, thresholds$BB_pct)
    score <- score + check_threshold(player_data$rolling_K_pct, thresholds$K_pct, lower_is_better = TRUE)
    score <- score + check_threshold(player_data$rolling_ISO, thresholds$ISO)
    
    # Apply age and position adjustments
    age_factor <- 1 + (player_data$age_vs_level_avg * -0.05)
    age_factor <- max(min(age_factor, 1.2), 0.8)
    score <- score * age_factor * pos_factor
    
    # Apply streak adjustments
    if (isTRUE(player_data$hot_streak)) score <- score * 1.1
    if (isTRUE(player_data$slump)) score <- score * 0.9
  }
  
  return(min(score, 100))
}

# Modified create_visualization function with more prominent threshold line
create_visualization <- function(promotion_scores) {
  # Create empty plotly object
  p <- plot_ly()
  
  # Split data into level segments
  level_segments <- split(promotion_scores, cumsum(c(1, diff(match(promotion_scores$Level, unique(promotion_scores$Level))) != 0)))
  
  # Distinct colors for levels
  level_colors <- c(
    "AA" = "#2E86C1",  # Strong blue
    "AAA" = "#28B463"  # Strong green
  )
  
  # First, add the promotion threshold with increased prominence
  p <- p %>% add_segments(
    x = min(promotion_scores$Date),
    xend = max(promotion_scores$Date),
    y = 65,
    yend = 65,
    line = list(
      color = '#E74C3C',  # Solid red instead of rgba
      dash = 'dash',
      width = 2  # Increased from 1
    ),
    name = "Promotion Threshold",
    showlegend = TRUE
  )
  
  # Add promotion score lines with improved visibility
  for(i in seq_along(level_segments)) {
    segment <- level_segments[[i]]
    level <- unique(segment$Level)
    
    p <- p %>% add_lines(
      data = segment,
      x = ~Date,
      y = ~promotion_score,
      name = paste("Promotion Score -", level),
      line = list(
        color = level_colors[level],
        width = 3  # Thicker lines
      ),
      showlegend = TRUE
    )
  }
  
  # Add separate subplot for performance metrics
  p <- p %>% layout(
    yaxis2 = list(
      side = "right",
      overlaying = "y",
      title = "OPS/wRC",
      showgrid = FALSE,
      range = c(0, max(c(promotion_scores$rolling_OPS, promotion_scores$rolling_wRC), na.rm = TRUE) * 1.1)
    )
  )
  
  # Add performance metrics with reduced opacity
  p <- p %>%
    add_lines(
      data = promotion_scores,
      x = ~Date,
      y = ~rolling_OPS,
      name = "Rolling OPS",
      line = list(
        color = 'rgba(142, 68, 173, 0.6)',  # Purple
        dash = 'dot',
        width = 2
      ),
      yaxis = "y2"
    ) %>%
    add_lines(
      data = promotion_scores,
      x = ~Date,
      y = ~rolling_wRC,
      name = "Rolling wRC",
      line = list(
        color = 'rgba(243, 156, 18, 0.6)',  # Orange
        dash = 'dot',
        width = 2
      ),
      yaxis = "y2"
    )
  
  # Add level change indicators
  level_changes <- promotion_scores %>%
    mutate(level_change = Level != lag(Level)) %>%
    filter(level_change == TRUE)
  
  for(i in 1:nrow(level_changes)) {
    p <- p %>% add_segments(
      x = level_changes$Date[i],
      xend = level_changes$Date[i],
      y = 0,
      yend = 100,
      line = list(
        color = 'rgba(0, 0, 0, 0.2)',  # Very light black
        dash = 'dash',
        width = 1
      ),
      showlegend = FALSE,
      hoverinfo = "text",
      text = paste("Level Change:", level_changes$Level[i])
    )
  }
  
  # Update layout with improved readability
  p <- p %>% layout(
    title = list(
      text = "Promotion Readiness by Level",
      font = list(size = 24)
    ),
    xaxis = list(
      title = "Date",
      tickfont = list(size = 12),
      gridcolor = 'rgba(0,0,0,0.1)',
      showgrid = TRUE
    ),
    yaxis = list(
      title = "Promotion Score",
      range = c(0, 100),
      side = "left",
      tickfont = list(size = 12),
      gridcolor = 'rgba(0,0,0,0.1)',
      showgrid = TRUE
    ),
    showlegend = TRUE,
    legend = list(
      x = 1.05,
      y = 1,
      font = list(size = 12),
      bgcolor = 'rgba(255,255,255,0.9)',
      bordercolor = 'rgba(0,0,0,0.2)',
      borderwidth = 1
    ),
    margin = list(r = 150, t = 100),  # Increased margins
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    width = 1000,  # Explicitly set width
    height = 600   # Explicitly set height
  )
  
  return(p)
}

# Create level-specific plots function
create_level_specific_plots <- function(promotion_scores) {
  # Split data by level
  level_data <- split(promotion_scores, promotion_scores$Level)
  
  # Create a plot for each level
  level_plots <- lapply(names(level_data), function(level) {
    level_df <- level_data[[level]]
    
    plot_ly() %>%
      add_lines(data = level_df, x = ~Date, y = ~promotion_score, 
                name = "Promotion Score", line = list(color = 'blue')) %>%
      add_lines(data = level_df, x = ~Date, y = ~rolling_OPS, 
                name = "Rolling OPS", line = list(color = 'red'), yaxis = "y2") %>%
      add_lines(data = level_df, x = ~Date, y = ~rolling_wRC, 
                name = "Rolling wRC", line = list(color = 'green'), yaxis = "y3") %>%
      layout(
        title = paste(level, "Level Performance"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Promotion Score", range = c(0, 100), side = "left"),
        yaxis2 = list(title = "OPS", overlaying = "y", side = "right"),
        yaxis3 = list(title = "wRC", overlaying = "y", side = "right", position = 0.95),
        showlegend = TRUE
      )
  })
  
  names(level_plots) <- names(level_data)
  return(level_plots)
}

# Main analysis function
analyze_player <- function(data) {
  # Process data
  processed_data <- process_batter_data(data, level_avg_ages)
  
  # Calculate promotion scores
  promotion_scores <- processed_data %>%
    rowwise() %>%
    mutate(promotion_score = calculate_promotion_score(cur_data(), level_thresholds, position_factors)) %>%
    ungroup()
  
  # Create overall visualization
  overall_plot <- create_visualization(promotion_scores)
  
  # Create level-specific plots
  level_plots <- create_level_specific_plots(promotion_scores)
  
  # Return results
  list(
    processed_data = processed_data,
    promotion_scores = promotion_scores,
    overall_plot = overall_plot,
    level_plots = level_plots
  )
}

# Run the analysis
results <- analyze_player(Carson_McCusker_data)

# View plots
results$overall_plot

# View level-specific plots
for (level in names(results$level_plots)) {
  print(results$level_plots[[level]])
}

# View promotion scores data frame
View(results$promotion_scores)
