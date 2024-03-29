#' @title Visualize the Result of the netABM Simulation
#' @description
#' This function plots the result of simulation of netABM.
#'
#' @param D A \code{netABM} object.
#' @param log Logical, indicating whether to plot the figures of each time slice. If \code{FALSE}, the only the current network will be plotted. Default is \code{FALSE}.
#' @param duplicated_edges Character string indicating how duplicated edges should be handled.
#'   Options are \code{"sum"}, code{"max"}, \code{"min"}, or \code{"mean"}. Default is \code{"sum"}.
#' @param which_net Integer or character string specifying which network to plot if there are multiple networks in the \code{netABM} object. Default is \code{1}.
#' @param show_d3movie Logical, indicating whether to display the movie of the change of the network.
#'   Default is \code{TRUE}. If set as \code{FALSE}, the normal plotting will be executed.
#' @param movie_output_mode Character string specifying the output mode for the movie.
#'   Options are \code{"HTML"}, \code{"JSON"}, \code{"inline"}, or \code{"htmlWidget"}. Default is \code{"HTML"}.
#' @param displaylabels Logical, indicating whether to display labels for nodes. Default is \code{TRUE}.
#' @param label.cex Numeric value specifying the size of node labels. Default is \code{0.6}.
#' @param label.col Character string specifying the color of node labels. Default is \code{"blue"}.
#' @param vertex.cex Numeric value or function specifying the size of nodes. Default is \code{1}.
#' @param vertex.col Character string or function specifying the color of nodes. Default is \code{"purple"}.
#' @param edge.lwd Numeric value or function specifying the width of edges. Default is \code{"weight"},
#' so that edge weight of the network will be reflected as the width.
#' @param edge.col Character string or function specifying the color of edges. Default is \code{"darkgray"}.
#' @param usearrows Logical, indicating whether to use arrows for directed edges. Default is \code{TRUE}.
#' @param filename Character string specifying the filename for the output HTML file if \code{show_d3movie} is \code{TRUE}. Default is "D_net".
#' @param animation.mode Character string specifying the animation mode for dynamic networks. Default is \code{"kamadakawai"}.
#' Other options are: \code{"MDSJ"}, \code{"useAttriute"}, and \code{"Graphviz"}.
#' @param d3.options List of options for D3 rendering. The default duration of each time slice is 1600 milliseconds.
#' see \code{d3.options} in [ndtv::render.d3movie()]
#' @param render.par List of parameters for rendering the movie. see \code{render.par} in [ndtv::render.d3movie()]
#'
#' @details
#' \code{plotABM_network} is the wrapper function of the \code{ndtv} package so that
#' users can directly supply \code{netABM} object and plot or show the movie of it.
#' This function internally calls the \code{D_to_statnet} function and pass the returned object to
#' \code{ndtv}'s functions with some format setting. Accordingly, if the user want to
#' make more fine-grained movie by themselves, just use \code{D_to_statnet} function and
#' write further scripts.
#'
#' Basically, four different options are available for \code{plotABM_network} depending on
#' how the user sets \code{log} and \code{show_d3movie}:
#' - \code{log = F} and \code{show_d3movie = F}: Showing the current plot as a static figure.
#' This combination uses the \code{network} package's \code{plot} function.
#' - \code{log = T} and \code{show_d3movie = F}: Showing each time-step plot as separate figures
#' using \code{network} pacakge
#' - code{log = F} and \code{show_d3movie = T}: Showing the current plot using the
#' \code{ndtv}'s \code{render.d3movie = T} function.
#' - code{log = T} and \code{render.d3movie = T}: Showing the movie of whole steps using
#' \code{ndtv}'s \code{render.d3movie = T} function.
#'
#' \code{movie_output_mode} determines which file of output movie should be the returned.
#' The default \code{"HTML"} exports an HTML file with embedded javascript player app including the JSON data structure,
#' with which the user's default web-browser will be opend and the movie will be shown.
#' \code{"JSON"} exports JSON data structure for loading into user's existing page.
#' \code{"inline"} renders the HTML inside an iframe tag and suppresses all
#' other output in an attempt to make it embedable in rmarkdown documents.
#' \code{"htmlWidget"} is suitable for displaying in an RStudio plot window or Shiny app.
#' See the description of \code{output.mode} of [ndtv::render.d3movie()].
#'
#' \code{"animation.mode"} determines the animation layout. While \code{"kamadakawai"} is the
#' is the default value, this may be actually not the best option and other option may produce
#' a better visualization. For using the latter options, user's firstly need to call it from their computer,
#' and do some steps before using these options.
#' For the detail, see the page of [ndtv::network.layout.animate.Graphviz()].
#'
#' If the user sets the nodal or edge properties to the visualization options that changes over time
#'  (e.g. setting changing "age" to the size of the nodes), which is called
#'  "TEA" (temporary ), it should be specified
#'  how to retrieve attributes in each time slice.
#'
#' @import network
#' @import networkDynamic
#' @import ndtv
#' @export
#' @examples
#' # preparing the dataset
#' node_attr <- data.frame(
#'   sex = factor(c(rep("m", 5), rep("f", 5))))
#' D <- setABM_network(n = 10,
#'                     node_attr = node_attr,
#'                     .act = actAgent_addEdges_random)
#' D <- runABM_network(D, .stopCondition = stopABM_times(simTimes = 5),
#'                     save_log = TRUE)
#'
#' # Example 1: Plot the current network with the default settings
#' plotABM_network(D, show_d3movie = FALSE)
#'
#' # Example 2: Plot the figures of each time slice
#' plotABM_network(D, log = TRUE, show_d3movie = FALSE)
#'
#' # Example 3: Show the movie of the change of the network and set different colors to nodes
#' plotABM_network(D, log = TRUE, show_d3movie = TRUE, label.col = "sex")

plotABM_network <- function(D, log = F, duplicated_edges = "sum", which_net = 1,
                            show_d3movie = TRUE,
                            movie_output_mode = "HTML",
                            displaylabels = TRUE,
                            label.cex = 0.6,
                            label.col = "blue",
                            vertex.cex = 1,
                            vertex.col = "purple",
                            edge.lwd = "weight",
                            edge.col = "darkgray",
                            usearrows = TRUE,
                            filename = "D_net",
                            animation.mode = "kamadakawai",
                            d3.options = list(animationDuration = 1600),
                            render.par = list(tween.frames = 10,
                                              show.time = T,
                                              show.stats = NULL,
                                              extraPlotCmds = NULL,
                                              initial.coords = 0)){
  # statnetデータにconvertする
  D_net <- D_to_statnet(D = D, log = log,
                        duplicated_edges = duplicated_edges,
                        which_net = which_net)
  # 条件を整理する
  plot_cond <- if(log == F & show_d3movie == F){"single-plot"
  }else if(log == T & show_d3movie == F){"multi-plot"
  }else if(log == F & show_d3movie == T){"single-movie"
  }else{"multi-movie"}


  # 条件ごとにプロット処理
  switch(plot_cond,
         "single-plot" = {plot(D_net,
                               displaylabels = displaylabels,
                               label.col = label.col,
                               vertex.cex = vertex.cex,
                               vertex.col = vertex.col,
                               edge.lwd = edge.lwd,
                               edge.col = edge.col,
                               usearrows = usearrows,
                               mode = animation.mode)
         },
         "multi-plot" = {for(m in 1:length(D_net)){
           plot(D_net[[m]],
                displaylabels = displaylabels,
                label.col = label.col,
                vertex.cex = vertex.cex,
                vertex.col = vertex.col,
                edge.lwd = edge.lwd,
                edge.col = edge.col,
                usearrows = usearrows,
                main = paste0("t = ", get.network.attribute(D_net[[m]], "time")),
                mode = animation.mode)}
         },
         "single-movie" = {
           D_net <- networkDynamic(network.list = list(D_net), create.TEAs = T)
           # render.parの設定
           render.par <- list(durationControl = F,
                              show.time = F)
           # Movie
           render.d3movie(D_net,
                          output.mode = movie_output_mode, filename = filename,
                          displaylabels = displaylabels,
                          label.col = label.col,
                          vertex.cex = vertex.cex,
                          vertex.col = vertex.col,
                          edge.lwd = edge.lwd,
                          edge.col = edge.col,
                          usearrows = usearrows,
                          render.par = list(show.time = F),
                          d3.options = list(playControls = F,
                                            durationControl = FALSE)
           )
         },
         "multi-movie" = {
           D_net <- networkDynamic(
             network.list = D_net,
             create.TEAs = TRUE,
             start = 1, end = (length(D$log)+1))

           # ndtvに使えるようにanimationをかける
           slice.par <- list(start = 1, end = length(D$log), interval = 1,
                             aggregate.dur = 1, rule = "latest")
           ndtv::compute.animation(D_net, slice.par = slice.par,
                                   animation.mode = animation.mode)
           # render.parの設定
           render.par <- list(tween.frames = 5, durationControl = TRUE,
                              show.time = F)
           render.d3movie(D_net,
                          output.mode = movie_output_mode, filename = filename,
                          d3.options = d3.options,
                          render.par = render.par,
                          displaylabels = displaylabels,
                          label.col = label.col,
                          vertex.cex = vertex.cex,
                          vertex.col = vertex.col,
                          edge.lwd = edge.lwd,
                          edge.col = edge.col,
                          usearrows = usearrows,
                          main = function(slice){paste("t =", get.network.attribute(slice, "time"))}
           )
         }
  )
}
