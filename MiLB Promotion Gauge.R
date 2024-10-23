# Required libraries
library(tidyverse)
library(lubridate)
library(plotly)
library(zoo)
library(baseballr)

# Data Collection
Carson_McCusker_data <- fg_milb_batter_game_logs(playerid = "sa3022304", year = 2024)
# Export Game Log CSV if wanted, example code:
# write.csv(Carson_McCusker_data, "C:/Users/weber/OneDrive/Desktop/Baseball/Carson.McCusker.csv", row.names = FALSE)

# Define scouting grade structure
scouting_grades <- list(
  "20" = 0,    # Well Below Average
  "30" = 10,   # Below Average
  "35" = 15,   # Below Average/Fringe
  "40" = 20,   # Below Average/Fringe
  "45" = 25,   # Fringe Average
  "50" = 30,   # Average
  "55" = 35,   # Above Average
  "60" = 40,   # Plus
  "65" = 42.5, # Plus/Plus Plus
  "70" = 45,   # Plus Plus
  "80" = 50    # Elite
)

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

# Add position assignment function
assign_position <- function(data, current_position, future_positions = NULL) {
  # Assign current position
  data <- data %>%
    mutate(
      Position = current_position,
      Original_Position = Position  # Keep track of original position designation
    )
  
  return(list(
    current_data = data,
    future_positions = future_positions
  ))
}


# Function to convert 20-80 grade to score
convert_grade_to_score <- function(grade) {
  scouting_grades[[as.character(grade)]]
}

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

# Calculate promotion score function with scouting grades
calculate_promotion_score <- function(player_data, level_thresholds, position_factors, fielding_grade, baserunning_grade) {
  score <- 0
  current_level <- player_data$Level
  thresholds <- level_thresholds[[current_level]]
  pos_factor <- position_factors[[player_data$Position]]
  
  if (is.null(pos_factor)) pos_factor <- 1
  
  if (!is.null(thresholds)) {
    # Offensive metrics (60% of total score)
    offense_score <- (
      check_threshold(player_data$rolling_OPS, thresholds$OPS) +
        check_threshold(player_data$rolling_wRC, thresholds$wRC) +
        check_threshold(player_data$rolling_BB_pct, thresholds$BB_pct) +
        check_threshold(player_data$rolling_K_pct, thresholds$K_pct, lower_is_better = TRUE) +
        check_threshold(player_data$rolling_ISO, thresholds$ISO)
    ) * 0.6
    
    # Convert scouting grades to scores
    fielding_score <- convert_grade_to_score(fielding_grade) * 0.25
    baserunning_score <- convert_grade_to_score(baserunning_grade) * 0.15
    
    # Combine scores
    score <- offense_score + fielding_score + baserunning_score
    
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

# Modified analyze_player function to handle multiple position scenarios
analyze_player <- function(data, 
                           current_position,
                           future_positions = NULL,
                           fielding_grade = 50, 
                           baserunning_grade = 50) {
  
  # Assign position(s)
  position_data <- assign_position(data, current_position, future_positions)
  
  # Process data
  processed_data <- process_batter_data(position_data$current_data, level_avg_ages)
  
  # Calculate promotion scores for current position
  promotion_scores <- processed_data %>%
    rowwise() %>%
    mutate(
      promotion_score = calculate_promotion_score(
        cur_data(), 
        level_thresholds, 
        position_factors, 
        fielding_grade, 
        baserunning_grade
      )
    ) %>%
    ungroup()
  
  # Calculate alternative position scenarios if specified
  future_scenarios <- NULL
  if (!is.null(future_positions)) {
    future_scenarios <- lapply(future_positions, function(pos) {
      modified_data <- processed_data %>%
        mutate(Position = pos)
      
      scores <- modified_data %>%
        rowwise() %>%
        mutate(
          promotion_score = calculate_promotion_score(
            cur_data(), 
            level_thresholds, 
            position_factors, 
            fielding_grade, 
            baserunning_grade
          )
        ) %>%
        ungroup()
      
      list(
        position = pos,
        scores = scores,
        plot = create_visualization(scores)
      )
    })
    names(future_scenarios) <- future_positions
  }
  
  # Create visualizations
  overall_plot <- create_visualization(promotion_scores)
  level_plots <- create_level_specific_plots(promotion_scores)
  
  return(list(
    processed_data = processed_data,
    promotion_scores = promotion_scores,
    overall_plot = overall_plot,
    level_plots = level_plots,
    future_scenarios = future_scenarios,
    scouting_grades = list(
      fielding = fielding_grade,
      baserunning = baserunning_grade
    )
  ))
}

# Usage example:
results <- analyze_player(
  Carson_McCusker_data,
  current_position = "RF",  # Assign current position as RF
  future_positions = c("LF", "DH"),  # Evaluate future scenarios
  fielding_grade = 40,
  baserunning_grade = 40
)

# View current position results
results$overall_plot

# View future position scenarios
if (!is.null(results$future_scenarios)) {
  for(pos in names(results$future_scenarios)) {
    print(paste("Scenario:", pos))
    print(results$future_scenarios[[pos]]$plot)
  }
}

# Create position comparison summary
if (!is.null(results$future_scenarios)) {
  position_comparison <- bind_rows(
    mutate(results$promotion_scores, Position_Scenario = "Current (RF)"),
    bind_rows(lapply(names(results$future_scenarios), function(pos) {
      mutate(results$future_scenarios[[pos]]$scores, Position_Scenario = pos)
    }))
  ) %>%
    group_by(Position_Scenario, Level) %>%
    summarise(
      Avg_Promotion_Score = round(mean(promotion_score, na.rm = TRUE), 2),
      Max_Promotion_Score = round(max(promotion_score, na.rm = TRUE), 2),
      Days_Above_Threshold = sum(promotion_score >= 65, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Print the detailed comparison table
  print(position_comparison)
  
  # Inside your if statement, update the position plot creation:
  position_plot <- plot_ly() %>%
    # AA Average Scores
    add_trace(
      data = position_comparison %>% filter(Level == "AA"),
      x = ~Position_Scenario,
      y = ~Avg_Promotion_Score,
      type = "bar",
      name = "AA Average Score",
      marker = list(color = '#2E86C1')  # Blue for AA
    ) %>%
    # AAA Average Scores
    add_trace(
      data = position_comparison %>% filter(Level == "AAA"),
      x = ~Position_Scenario,
      y = ~Avg_Promotion_Score,
      type = "bar",
      name = "AAA Average Score",
      marker = list(color = '#28B463')  # Green for AAA
    ) %>%
    # AA Max Scores
    add_trace(
      data = position_comparison %>% filter(Level == "AA"),
      x = ~Position_Scenario,
      y = ~Max_Promotion_Score,
      type = "scatter",
      mode = "markers",
      name = "AA Max Score",
      marker = list(
        color = '#1B4F72',  # Darker blue for AA
        size = 10,
        symbol = "circle"
      )
    ) %>%
    # AAA Max Scores
    add_trace(
      data = position_comparison %>% filter(Level == "AAA"),
      x = ~Position_Scenario,
      y = ~Max_Promotion_Score,
      type = "scatter",
      mode = "markers",
      name = "AAA Max Score",
      marker = list(
        color = '#186A3B',  # Darker green for AAA
        size = 10,
        symbol = "diamond"
      )
    ) %>%
    layout(
      title = "Promotion Scores by Position",
      xaxis = list(
        title = "Position",
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = "Score",
        range = c(0, 100),
        tickfont = list(size = 12),
        gridcolor = 'rgba(0,0,0,0.1)'
      ),
      barmode = 'group',
      showlegend = TRUE,
      legend = list(
        x = 1.05,
        y = 1,
        font = list(size = 12)
      ),
      margin = list(r = 150),  # Add right margin for legend
      plot_bgcolor = 'white',
      paper_bgcolor = 'white'
    )
  
  # Print the position comparison plot
  print(position_plot)
  
  # Print the position comparison plot
  print(position_plot)
