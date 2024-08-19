#' @title Get Neighboring Cells in a Cellular Automaton
#' @description This function retrieves the ID of the neighboring cells in a
#' cellular automaton (CA) grid, or either just retrieve or calculate their attributes.
#' It can handle both Moore and Neumann neighborhood types and
#' can also handle toroidal grids.
#'
#' @param which_ca The index or name of the CA grid to use within the \code{D} object.
#' Defaults is \code{NULL}, resulting in selecting the first "ca" object found by
#' \code{D$.field_type()} method.
#' @param neib_type The type of neighborhood to use,
#' either \code{"Moore"} or \code{"Neumann"}. Defaults to \code{"Moore"}.
#' @param which_attr A character vector specifying the attributes of the
#' neighboring agents to retrieve. If \code{NULL}, only the IDs of neighboring
#' cells are returned.
#' @param neib_size A numeric vector specifying the distance of neighbors considered
#' from the agent. If a single vector is provided, it will copied to each dimension.
#' Default is \code{1}.
#' @param torus Logical, indicating whether the grid should be treated as a torus.
#' Defaults to \code{FALSE}.
#' @param D A \code{ABM_D} class object. The default \code{NULL} results in retrieving
#' D from the parent.frame, which will be usual usage.
#' @param ID The ID of the target agent. The default \code{NULL} results in retrieving
#' the ID from the parent.frame, which will be usual usage.
#' @param loc The location of the target cell as a 1*CA's dimension matrix format
#'  (e.g., matrix(c(row_ID, column_ID)), 1, 2))
#' If not provided, it will be retrieved from the \code{ID}'s location.
#' @param FUN An optional function to apply to the attributes of neighboring
#' agents to calculate the specified score.
#' If \code{NULL}, the attributes are returned as-is.
#' @param ... Additional arguments passed to \code{FUN}.
#' @return Depending on the input parameters:
#' - If \code{which_attr} is \code{NULL}, a vector of neighboring cell IDs is returned.
#' - If \code{which_attr} is provided, a data frame of the specified
#' attributes of neighboring agents is returned.
#' - If both \code{which_attr} and \code{FUN} are provided,
#' the result of applying \code{FUN} to the attributes is returned.
#' @importFrom rlang is_empty
#' @examples
#' # creating a CA
#' CA <- matrix(1:16, 4, 4)
#' attr <- 1:16
#' # Get attributes of neighboring agents
#' ca_neib(ID = 1, CA = CA, attr = attr)
#' # Apply a function to the attributes of neighboring agents
#' ca_neib(ID = 1, CA = CA, attr = attr, FUN = mean)
#' @export
#' @examples
#' D <- setABM(agent_n = 5, agent_attr = data.frame(age = 1:5), ca = 1)
#' ca_neib(D = D, ID = 1)
#' ca_neib(D = D, ID = 1, which_attr = "age")
#' ca_neib(D = D, ID = 1, which_attr = "age", FUN = mean)

ca_neib <- function(
    which_ca = NULL,
    neib_type = "Moore",
    torus = FALSE,
    which_attr = NULL,
    neib_size = 1,
    D = NULL,
    ID = NULL,
    loc = NULL,
    FUN = NULL,
    ...){
  ## DとIDが入っていない場合にはparent.frameから取得する
  if(is.null(D)){D <- parent.frame()$D}
  if(is.null(ID)){ID <- parent.frame()$self$ID}
  ## which_caが直接指定されている場合にはそちらを取る。
  ## そうでない場合にはDの中の1つめのcaを取得
  if(is.null(which_ca)){
    field_type <- D$.field_type()
    which_ca <- names(field_type)[field_type=="ca"][1]
  }
  ## locが指定されていない場合には、自動取得
  if(is.null(loc)){
    loc <- ca_loc(which_ca = which_ca, ID = ID, D = D, arr.ind = TRUE)
  }
  ## CAを取得
  CA <- D[[which_ca]]
  ## CAのdimを取得
  CA_dim <- dim(CA)
  ## neib_sizeを確認(異なる場合にはneib_sizeの1番目の値をCA_dimの各次元に拡大)
  if(length(neib_size) != length(CA_dim)){
    neib_size <- rep(neib_size[1], length(CA_dim))
  }
  stopifnot("neib_size must be equal or greater than 0" = all(neib_size >= 0))

  ## torusに対応させてCAを拡大する
  if(torus){
    if(length(CA_dim)==2){
      row_left <- ifelse(neib_size[1]==0, 0, seq(CA_dim[1], CA_dim[1] - neib_size[1] + 1, by = -1))
      row_right <- ifelse(neib_size[1]==0, 0, seq(1, neib_size[1], by = 1))
      col_upper <- ifelse(neib_size[2]==0, 0, seq(CA_dim[2], CA_dim[2] - neib_size[2] + 1, by = -1))
      col_bottom <- ifelse(neib_size[2]==0, 0, seq(1, neib_size[2], by = 1))
      new_row <- c(row_left, 1:CA_dim[1], row_right)
      new_col <- c(col_upper, 1:CA_dim[2], col_bottom)
      CA <- CA[new_row, new_col]
      loc <- matrix(c(loc[1]+neib_size[1], loc[2]+neib_size[2]), 1, 2)
      CA_dim <- dim(CA)
    }else if(length(CA_dim)==3){
      row_left <- ifelse(neib_size[1]==0, 0, seq(CA_dim[1], CA_dim[1] - neib_size[1] + 1, by = -1))
      row_right <- ifelse(neib_size[1]==0, 0, seq(1, neib_size[1], by = 1))
      col_upper <- ifelse(neib_size[2]==0, 0, seq(CA_dim[2], CA_dim[2] - neib_size[2] + 1, by = -1))
      col_bottom <- ifelse(neib_size[2]==0, 0, seq(1, neib_size[2], by = 1))
      depth_upper <- ifelse(neib_size[3]==0, 0, seq(CA_dim[3], CA_dim[3] - neib_size[3] + 1, by = -1))
      depth_bottom <- ifelse(neib_size[3]==0, 0, seq(1, neib_size[3], by = 1))
      new_row <- c(row_left, 1:CA_dim[1], row_right)
      new_col <- c(col_upper, 1:CA_dim[2], col_bottom)
      new_depth <- c(depth_upper, 1:CA_dim[3], depth_bottom)
      CA <- CA[new_row, new_col, new_depth]
      loc <- matrix(c(loc[1]+neib_size[1], loc[2]+neib_size[2], loc[3]+neib_size[3]), 1, 3)
      CA_dim <- dim(CA)
    }else{
      warning("The CA with this size of dimension is currently not supported by this function.
              NULL is returned.")
      return(NULL)
    }
  }

  ## 回りのIDを取得する
    if(length(CA_dim)==2){
      neib_row <- ifelse(loc[1]-neib_size[1] < 0, 1, loc[1]-neib_size[1]):
        ifelse(loc[1]+neib_size[1] > CA_dim[1], CA_dim[1], loc[1]+neib_size[1])
      neib_col <- ifelse(loc[2]-neib_size[2] < 0, 1, loc[2]-neib_size[2]):
        ifelse(loc[2]+neib_size[2] > CA_dim[2], CA_dim[2], loc[2]+neib_size[2])
      neib_ID <- switch(neib_type,
                        "Moore" = as.vector(CA[neib_row, neib_col]),
                        "Neumann" = {
                          neib_ID_row <- as.vector(CA[neib_row, loc[2], loc[3]])
                          neib_ID_colc <- as.vector(CA[loc[1], neib_col, loc[3]])},
                        stop("No neib_type option found.")
      )
    }else if(length(CA_dim)==3){
      neib_row <- ifelse(loc[1]-neib_size[1] < 0, 1, loc[1]-neib_size[1]):
        ifelse(loc[1]+neib_size[1] > CA_dim[1], CA_dim[1], loc[1]+neib_size[1])
      neib_col <- ifelse(loc[2]-neib_size[2] < 0, 1, loc[2]-neib_size[2]):
        ifelse(loc[2]+neib_size[2] > CA_dim[2], CA_dim[2], loc[2]+neib_size[2])
      neib_depth <- ifelse(loc[3]-neib_size[3] < 0, 1, loc[3]-neib_size[3]):
        ifelse(loc[3]+neib_size[3] > CA_dim[3], CA_dim[3], loc[3]+neib_size[3])
      neib_ID <- switch(neib_type,
                        "Moore" = as.vector(CA[neib_row, neib_col, neib_depth]),
                        "Neumann" = {
                          neib_ID_row <- as.vector(CA[neib_row, loc[2], loc[3]])
                          neib_ID_colc <- as.vector(CA[loc[1], neib_col, loc[3]])
                          neib_ID_depth <- as.vector(CA[loc[1], loc[2], neib_depth])},
                        stop("No neib_type option found.")
      )
    }else{
      warning("The CA with this size of dimension is currently not supported by this function.
              NULL is returned.")
      return(NULL)
    }
    # neib_IDから0とself_IDをぬく
    neib_ID <- setdiff(neib_ID, c(0, ID))

  # 何も周りにいない場合にはNULLを返す
    if(rlang::is_empty(neib_ID)){
      return(NULL)
    }

  # attrがない場合にはneib_IDを返す
  if(is.null(which_attr)){
    return(neib_ID)
  }

  ## which_attrが投入されている場合
  neib_attr <- do.call(data.frame,
                       lapply(which_attr, function(X){
                         unlist(lapply(neib_ID, function(i){D$agent[[i]][[X]]}))
                       }))
  colnames(neib_attr) <- which_attr

  ## FUNがない場合にはneib_attrを返す
  if(is.null(FUN)){
      return(neib_attr)
  }

  ## FUNが投入されている場合にはさらに計算を行う
  calc_out <- apply(neib_attr, MARGIN = 2, FUN = FUN, ...)
  ## calc_outを戻す
  calc_out
}




