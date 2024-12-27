#' Simulate Schelling's Segregation Model
#'
#' This function simulates Schelling's segregation model using an Agent-Based Modeling (ABM) approach.
#' Agents are relocated based on their neighborhood satisfaction, determined by the proportion of agents
#' from the same group within their vicinity.
#'
#' @param grid_size Numeric. The size of the square grid (number of rows and columns).
#' Must be a positive integer. Default is 25.
#' @param population_density Numeric. The proportion of grid cells occupied by agents (0 < population_density <= 1).
#' Default is 0.8.
#' @param group_prop Numeric vector. Proportions of different groups in the population. Must be positive and sum to 1.
#' Default is \code{c(0.5, 0.5)}. For more than two groups, use \code{c(0.5, 0.25, 0.25)}.
#' @param minimum_same_prop Numeric. The minimum proportion of agents of the same group in a neighborhood
#' required for an agent to be satisfied (0 <= minimum_same_prop <= 1).
#' Default is 0.3.
#' @param max_time Numeric. Maximum number of time steps for the simulation. Must be a positive integer.
#' Default is 100.
#' @param tol Numeric. Tolerance for the change in the proportion of satisfied agents to determine convergence.
#' Must be non-negative. Default is 0.01. If users just want to run the simulation until `max.time`, set this as \code{NA}.
#' @param save_log Logical. If \code{TRUE}, saves the state of the simulation at each time step. Default is \code{FALSE}.
#' @param do_plot Logical. If \code{TRUE}, generates a plot of the simulation. Defaults to \code{FALSE}.
#' @param plot_sec Numeric. Time in seconds for each frame in the animation when \code{play_movie = TRUE}.
#' Defaults to 1.
#' @param save_GIF Logical. If \code{TRUE}, generates an GIF animation of the simulation. Defaults to \code{FALSE}.
#' Note that \code{save_log} must be \code{TRUE} for \code{save_GIF} to work.
#' @param return_G Logical. If \code{TRUE}, returns the final ABM object. Defaults to \code{TRUE}.
#' @param movie.name Character. The name of the GIF file to save the animation. Defaults to "schelling_simulation.gif".
#'
#' @return If \code{return_G = TRUE}, returns an ABM object representing the final state of the simulation.
#' The object contains the following:
#' \itemize{
#'   \item \code{agent_attr}: A data frame of agent attributes including group and location.
#'   \item \code{setting}: A list of simulation settings and parameters.
#'   \item \code{log}: A log of simulation states (if \code{save_log = TRUE}).
#' }
#'
#' @examples
#' # Basic simulation (not run due to potential long execution time)
#' \dontrun{
#' result <- game_schelling_segregation(grid_size = 25, max_time = 10)
#' }
#'
#' # Simulation with logging and plots
#' \dontrun{
#' result <- game_schelling_segregation(
#'   grid_size = 25,
#'   max_time = 10,
#'   save_log = TRUE,
#'   do_plot = TRUE
#' )
#' }
#'
#' # Generate an animation (requires HTML output)
#' \dontrun{
#' game_schelling_segregation(
#'   grid_size = 25,
#'   max_time = 10,
#'   save_log = TRUE,
#'   play_movie = TRUE,
#'   html_file = "example_simulation.html"
#' )
#' }
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom animation saveHTML
#' @importFrom R6 R6Class
#' @export
#' @references
#' Schelling, Thomas C. (1971). "Dynamic models of segregation".
#' The Journal of Mathematical Sociology, 1 (2): 143–186.

game_schelling_segregation <- function(
    grid_size = 25,
    population_density = 0.8,
    group_prop = c(0.5, 0.5),
    minimum_same_prop = 0.3,
    max_time = 100,
    tol = 0.01,
    save_log = FALSE,
    do_plot = FALSE,
    plot_sec = 1,
    save_GIF = FALSE,
    return_G = TRUE,
    movie.name = "schelling_simulation.gif") {
  # 初期化
  G <- NULL

  # Input validation
  if (!is.numeric(grid_size) || grid_size <= 0 || length(grid_size) != 1) {
    stop("grid_size must be a positive integer.")
  }
  if (!is.numeric(population_density) || population_density <= 0 || population_density > 1) {
    stop("population_density must be a numeric value between 0 and 1.")
  }
  if (any(group_prop <= 0)) {
    stop("group_prop must be a numeric vector of positive values.")
  }
  if (!is.numeric(minimum_same_prop) || minimum_same_prop < 0 || minimum_same_prop > 1) {
    stop("minimum_same_prop must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(max_time) || max_time <= 0 || length(max_time) != 1) {
    stop("max_time must be a positive integer.")
  }
  if (!is.na(tol) && tol < 0) {
    stop("tol must be a non-negative numeric value.")
  }
  if (play_movie == TRUE && do_plot == FALSE) {
    do_plot <- TRUE
  }
  if (play_movie == TRUE && save_log == FALSE) {
    warning("'play_movie' is active only when 'save_log = TRUE'. Set 'save_log = TRUE' to enable 'play_movie'.")
  }

  # Ensure the sum of group_prop is 1
  group_prop <- group_prop / sum(group_prop)

  # Agent setup
  agent_n <- round(grid_size^2 * population_density)
  agent_ID <- 1:agent_n
  group <- generate_group_labels(agent_n, prop = group_prop)

  # City setup
  city_address <- 1:(grid_size^2)

  # Settings
  setting <- list(
    city = matrix(0, nrow = grid_size, ncol = grid_size),
    minimum_same_prop = minimum_same_prop,
    max_time = max_time,
    tol = tol,
    agent_n = agent_n,
    city_address = city_address,
    prev_prop_happy = 0
  )

  # Agent attributes
  agent_attr <- data.frame(
    ID = agent_ID,
    group = group,
    address = sample(city_address, size = agent_n)
  )

  # Group map
  group_map <- function() {
    city <- self$setting$city
    city[self$agent_attr$address] <- self$agent_attr$group
    city
  }

  # Neighborhood list
  neib_list <- function() {
    group_map_value <- self$group_map
    neib <- neib8(mat = group_map_value, posit = self$agent_attr$address)
    rownames(neib) <- self$agent_attr$ID
    neib
  }

  # Unhappy agents
  unhappy_agents <- function() {
    neib_list <- self$neib_list
    same_gr_n <- sapply(1:nrow(neib_list), function(i) {
      sum(neib_list[i, ] == self$agent_attr$group[i], na.rm = TRUE)
    })
    len <- rowSums(!is.na(neib_list))
    same_gr_prop <- same_gr_n / len
    same_gr_prop[is.nan(same_gr_prop)] <- 1
    self$agent_attr$ID[same_gr_prop < self$setting$minimum_same_prop]
  }

  # Proportion happy
  prop_happy <- function() {
    unhappy_agents <- self$unhappy_agents
    len <- length(unhappy_agents)
    if (len == 1 && is.na(unhappy_agents)) {
      return(1)
    } else {
      return(1 - len / self$setting$agent_n)
    }
  }

  # Relocate agents
  relocate_agents <- function() {
    unhappies <- self$unhappy_agents
    vacant_places <- setdiff(G$setting$city_address, G$agent_attr$address)
    if (length(unhappies) <= length(vacant_places)) {
      G$agent_attr$address[unhappies] <- sample(vacant_places, size = length(unhappies))
    } else {
      G$agent_attr$address[sample(unhappies, size = length(vacant_places))] <- vacant_places
    }
  }

  # Stop condition
  stop_condition <- function() {
    change_prop_happy <- abs(self$prop_happy - self$setting$prev_prop_happy)
    if (any(c(self$time >= self$setting$max_time, change_prop_happy < self$setting$tol), na.rm = TRUE)) {
      return(TRUE)
    } else {
      self$setting$prev_prop_happy <- self$prop_happy
      return(FALSE)
    }
  }

  # Set ABM
  G <- setABM(
    stage = list(agent_attr = agent_attr, setting = setting),
    global_FUN = relocate_agents,
    stop_FUN = stop_condition,
    active_binding_field = list(
      group_map = group_map,
      neib_list = neib_list,
      unhappy_agents = unhappy_agents,
      prop_happy = prop_happy
    )
  )

  # Run ABM
  G <- runABM(
    G,
    schedule = "relocate_agents",
    stop_FUN_name = "stop_condition",
    save_log = save_log
  )

  # Plotting utility
  plot_frame <- function(map, time, happy_percentage, legend_labels, colors) {
    layout(matrix(c(1, 2), ncol = 2), widths = c(3, 1))
    par(mar = c(3, 1, 2, 0), xpd = TRUE)
    image(map, col = colors, axes = FALSE, asp = 1, main = paste("time =", time))
    text(0.7, -0.05, cex = 0.9, labels = paste("Happy agents (%):", round(happy_percentage * 100, 1)))
    par(mar = c(3, 0, 3, 0))
    plot.new()
    legend("topright", legend = legend_labels, fill = colors, bty = "n")
  }

  # Plot
  if (do_plot) {
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))

    n_category <- length(group_prop) + 1
    category_label <- c("0 (vacant)", 1:(n_category - 1))
    color_palette <- if (n_category <= 3) {
      c("white", "red", "blue")
    } else {
      c("white", RColorBrewer::brewer.pal(n_category - 1, "Set3"))
    }

    if (save_log) {
      group_map_list <- get_log(G$group_map, G = G)
      prop_happy_list <- get_log(G$prop_happy, G = G)

      if (save_GIF) {
        animation::saveGIF({
          for (i in seq_along(group_map_list)) {
            cat(sprintf("Processing GIF frame %d of %d.\n", i, length(group_map_list)))
            plot_frame(
              map = group_map_list[[i]],
              time = i,
              happy_percentage = prop_happy_list[[i]],
              legend_labels = category_label,
              colors = color_palette
            )
          }
        }, interval = plot_sec, movie.name = movie.name)
      } else {
        for (i in seq_along(group_map_list)) {
          plot_frame(
            map = group_map_list[[i]],
            time = i,
            happy_percentage = prop_happy_list[[i]],
            legend_labels = category_label,
            colors = color_palette
          )
        }
      }
    } else {
      plot_frame(
        map = G$group_map,
        time = G$time,
        happy_percentage = G$prop_happy,
        legend_labels = category_label,
        colors = color_palette
      )
    }
  }

  # Return
  if (return_G) {
    return(G)
  }
}
