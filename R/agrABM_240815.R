#' @title Aggregate and Transform the ABM_D Object for Analysis
#' @description The \code{agrABM} function processes a given \code{ABM_D} object
#'  to extract and organize its agent attributes and other fields.
#' It provides different methods of extracting these components based on the specified parameters.
#'
#' @param D A \code{ABM_D} class object to be processed.
#' @param from_log A logical value indicating whether to extract data
#' from the log \code{TRUE} or directly from the current state \code{FALSE}.
#' Default is \code{FALSE}.
#' @param attr_long A logical value. When \code{TRUE}, agent attributes in each time point is converted into a long format. Default is \code{FALSE}.
#' @param incl_attr_ca A logical value. When \code{TRUE}, agent's position in each \code{ca} type field is included in the agent attribute data. Default is \code{FALSE}.
#' @param incl_attr_euc A logical value. When \code{TRUE}, agent's position in each \code{euc} type field is included in the agent attribute data. Default is \code{FALSE}.
#' @param incl_attr_notes A logical value. When \code{TRUE}, the description in \code{notes}  is included in the agent attribute data. Default is \code{FALSE}.
#' @param ca_arr.ind A logical value. When \code{TRUE}, agent's position in each \code{ca} type field is included in a x-y coordinates format. Default is \code{FALSE}.
#'
#' @details The function processes the data object \code{D} and
#' extracts its attributes and other fields.
#' The behavior of the function changes based on the values of
#' \code{from_log} and \code{by_field} parameters:
#' \itemize{
#'   \item When \code{from_log} is \code{FALSE}, the function extracts the current agent attributes and the fields directly from \code{D}.
#'   \item When \code{from_log} is \code{TRUE}:
#'   \itemize{
#'     \item If \code{by_field} is \code{FALSE}, the function extracts data for each time point in the log, organizing the output by time.
#'     \item If \code{by_field} is \code{TRUE}, the function organizes the data by fields, optionally using a long format for attributes if \code{attr_long} is \code{TRUE}.
#'   }
#' }
#'
#' There are other functions that extract and organize the \code{D} data for some specific purpose (see: "see also" section).
#' @return A list containing the extracted agent attributes and fields.
#' @seealso [get_attr_from_ca()]
#' @importFrom dplyr bind_cols
#' @export
#' @examples
#' \dontrun{
#' # Example usage of agrABM function
#' result <- agrABM(D = some_D_object)
#' }

agrABM <- function(
    D, from_log = FALSE, by_field = FALSE, attr_long = FALSE,
    incl_attr_ca = FALSE, incl_attr_euc = FALSE, incl_attr_notes = FALSE,
    ca_arr.ind = FALSE){
  # field名を取得する
  field_name <- names(D)[!names(D) %in% c(".__enclos_env__",".f","clone",".save","time",
                            "log","agent","notes")]
  # attributeを処理する
    ## attribute
    agent_attr <- get_agent_attr(D = D, from_log = from_log, attr_long = attr_long)
    ## attr_ca
    if(incl_attr_ca==TRUE){
      agent_attr_ca <- .get_ca_for_attr(D = D, from_log = from_log, ca_arr.ind = ca_arr.ind)
    }else{
      agent_attr_ca <- NULL
    }
    ## attr_euc
    if(incl_attr_euc==TRUE){
      agent_attr_euc <- .get_euc_for_attr(D = D, from_log = from_log, attr_long = attr_long)
    }else{
      agent_attr_euc <- NULL
    }
    ## attr_notes
    if(incl_attr_notes==TRUE){
      agent_attr_notes <- .get_notes_for_attr(D = D, from_log = from_log, attr_long = attr_long)
    }else{
      agent_attr_notes <- NULL
    }

  # 現在のDから取得する場合
  if(from_log==FALSE){
    ## attributeをまとめる
    agent_attr <- do.call(dplyr::bind_cols, list(agent_attr, agent_attr_ca, agent_attr_euc, agent_attr_notes))
    ## すべてのfieldをまとめる
    if(is.null(field_name)){
      agrD <- list()
    }else{
      agrD <- lapply(1:length(field_name), function(p){
        D[[field_name[p]]]
      })
      names(agrD) <- field_name
    }
    agrD$agent <- agent_attr
    agrD$time <- D$time
    agrD$notes <- D$notes
  }else{
  # logから取得する場合
    ## attr_long = FALSEの場合
    if(attr_long==FALSE){
      ### by_field = FALSEの場合
      ### すべての要素をtimeごとに並べる
      if(by_field == FALSE){
        #### attributeを処理する
        agent <- lapply(1:length(agent_attr), function(t){
          do.call(dplyr::bind_cols, list(agent_attr[[t]], agent_attr_ca[[t]], agent_attr_euc[[t]], agent_attr_notes[[t]]))
        })
        #### すべての要素をまとめる
        agrD <- lapply(1:length(D$log), function(t){
          if(is.null(field_name)){
            agrD_t <- list()
          }else{
            agrD_t <- lapply(1:length(field_name), function(p){
              D$log[[t]][[field_name[p]]]
            })
            names(agrD_t) <- field_name
          }
          agrD_t$agent <- agent_attr[[t]]
          agrD_t$time <- D$log[[t]]$time
          agrD_t$notes <- D$log[[t]]$notes
          agrD_t
        })
        names(agrD) <- names(D$log)
      }else{
      ### by_field = TRUEの場合
      ### 各要素をtimeごとに並べる
        #### attributeを処理する
        agent <- lapply(1:length(agent_attr), function(t){
          do.call(dplyr::bind_cols, list(agent_attr[[t]], agent_attr_ca[[t]], agent_attr_euc[[t]], agent_attr_notes[[t]]))
        })
        #### 各要素を時間ごとに並べる
        agrD <- vector("list", length(field_name) + 3)
        agrD <- lapply(1:length(agrD), function(j){
          agrD[[j]] <- rep(NA, length(D$log))
          names(agrD[[j]]) <- names(D$log)
          agrD[[j]]
        })
        names(agrD) <- c(field_name, "agent", "time", "notes")
        #### agentとnotesはlistに置き換える
        agrD$agent <- vector("list", length(agrD$agent))
        names(agrD$agent) <- names(D$log)
        agrD$notes <- vector("list", length(agrD$notes))
        names(agrD$notes) <- names(D$log)
        #### 各要素に値を付与する
        for(j in field_name){
          temp <- lapply(1:length(D$log), function(t){
            D$log[[t]][[j]]
          })
          names(temp) <- names(D$log)
          agrD[[j]] <- temp
        }
        for(t in 1:length(D$log)){
          agrD$agent[[t]] <- agent_attr[[t]]
          agrD$time[[t]] <- D$log[[t]]$time
          agrD$notes[[t]] <- D$log[[t]]$notes
        }
      }
    }else{
    ## attr_long = TRUEの場合
      ### by_field = FALSEの場合
      ### attrはlongで、その他の要素はtimeごとに並べる
      if(by_field==FALSE){
        #### attributeを処理する
        agent <- dplyr::bind_cols(agent_attr, agent_attr_ca, agent_attr_euc, agent_attr_notes)
        #### すべての要素をまとめる
        agrD <- lapply(1:length(D$log), function(t){
          if(is.null(field_name)){
            agrD_t <- list()
          }else{
            agrD_t <- lapply(1:length(field_name), function(p){
              D$log[[t]][[field_name[p]]]
            })
            names(agrD_t) <- field_name
          }
          agrD_t$time <- D$log[[t]]$time
          agrD_t$notes <- D$log[[t]]$notes
          agrD_t
        })
        names(agrD) <- names(D$log)
        # agentを付加する
        agrD$agent <- agent_attr
      }else{
      ### by_field = TRUEの場合
      ### attrはlong、その他の各要素はtimeごとに並べる
        #### attributeを処理する
        agent <- dplyr::bind_cols(agent_attr, agent_attr_ca, agent_attr_euc, agent_attr_notes)
        #### 各要素を時間ごとに並べる
        agrD <- vector("list", length(field_name) + 2)
        agrD <- lapply(1:length(agrD), function(j){
          agrD[[j]] <- rep(NA, length(D$log))
          names(agrD[[j]]) <- names(D$log)
          agrD[[j]]
        })
        names(agrD) <- c(field_name, "time", "notes")
        #### notesはlistに置き換える
        agrD$notes <- vector("list", length(agrD$notes))
        names(agrD$notes) <- names(D$log)
        #### 各要素に値を付与する
        for(j in field_name){
          temp <- lapply(1:length(D$log), function(t){
            D$log[[t]][[j]]
          })
          names(temp) <- names(D$log)
          agrD[[j]] <- temp
        }
        for(t in 1:length(D$log)){
          agrD$time[[t]] <- D$log[[t]]$time
          agrD$notes[[t]] <- D$log[[t]]$notes
        }
        # attributeを付加する
        agrD$agent <- agent_attr
      }
    }
  }#---logから取得ここまで

  # リターン
    agrD
}
