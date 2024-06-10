#' @title Calculate the Index of dissimilarity (D-index) of CA
#' @description This function calculates the D-index,
#' which is a measure of segregation given cross-tabulated data for multiple zones and groups.
#'
#' @param CA A matrix/array or a list of them representing the cellular automaton wherein each entry is coded with the group attribute.
#' @param zone A matrix/array or a list of them representing the zones.
#' @param CA_exclude_value Values in CA to be excluded from the calculation (default is \code{0}).
#' @param with_zone_level Logical, if \code{TRUE}, the function returns D index for each zone in addition to the global D-index (default is \code{FALSE}).
#'
#' @return If \code{with_zone_level} is \code{TRUE}, a list with two elements:
#'   \describe{
#'     \item{d}{The D index.}
#'     \item{d_by_zone}{The D index by zone.}
#'   }
#'   Otherwise, returns the D index.
#'
#' @examples
#' \dontrun{
#' d <- ca_d_index(CA = agrD$room_by_ethnicity, zone = agrD$zone, with_zone_level = TRUE)
#' d$d_by_zone
#' }
#'
#' @export
ca_d_index <- function(
    CA, zone, CA_exclude_value = 0, with_zone_level = FALSE){
  # CAのインプットを整える
  ### リストに揃える
  if(is.list(CA)==FALSE){
    CA <- list(CA)
  }
  ### すべてarrayかを確認
  stopifnot("CA must be matrix/array or list of them." = all(unlist(lapply(CA, is.array))))

  # zoneのインプットを整える
  ### リストに揃える
  if(is.list(zone)==FALSE){
    zone <- list(zone)
  }
  ### すべてarrayかを確認
  stopifnot("zone must be matrix/array or list of them." = all(unlist(lapply(zone, is.array))))

  # 設定の値をNAにする
  for(t in 1:length(CA)){
    CA[[t]][CA[[t]] %in% CA_exclude_value] <- NA
  }

  # Dインデックスを計算する
  pop_zone <- mapply(function(X, Y){table(X, Y)}, X = zone, Y = CA, SIMPLIFY = FALSE)
  pop_whole <- lapply(pop_zone, colSums)
  # 2グループ以上ある場合には警告を出す
  if(ncol(pop_zone[[1]])>2){
    message(
      "More than 2 groups are supplied. Only the following groups are used for the calculation: ",
      paste(colnames(pop_zone[[1]]), collapse = " "))
  }

  # Dインデックスを計算
  d_by_zone <- mapply(function(X, Y){
    apply(X, 1, function(each_zone){
      abs(each_zone[1]/Y[1] - each_zone[2]/Y[2])
    })*1/2
  }, X = pop_zone, Y = pop_whole, SIMPLIFY = FALSE)
  d <- unlist(lapply(d_by_zone, sum))

  # もしも1時点しか入っていない場合には、そのまま返す
  if(length(d_by_zone)==1){
    d_by_zone <- d_by_zone[[1]]
  }
  if(length(d)==1){
    d <- d[1]
    names(d) <- "d"
  }
  # アウトプット
  if(with_zone_level){
    out <- list(d = d,
                d_by_zone = d_by_zone)
  }else{
    out <- d
  }
  out
}


