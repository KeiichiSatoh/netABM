
#-------------------------------------------------------------------------------
# .get_ca_for_attr
#-------------------------------------------------------------------------------

.get_ca_for_attr <- function(D, from_log = FALSE, ca_arr.ind = FALSE){
  field_name <- names(D)[!names(D) %in% c(".__enclos_env__",".f","notes","log","time","agent","clone",".save")]
  field_type <- unlist((lapply(field_name, function(X){attr(D[[X]], "field_type")})))

  # もしも一つもcaがなければ、NULLを返す
  if(all(field_type != "ca")){
    return(NULL)
  }

  # caのfield_nameを特定
  field_name <- field_name[field_type=="ca"]

  # agentの数を特定
  agent_n <- length(D$agent)

  # 現在のDから取得する場合
  if(from_log == FALSE){
    # 空のpositを作成
    attr_ca <- matrix(0, agent_n, 1)
    attr_ca <- data.frame(attr_ca[,-1])
    # caを取得する
    retrieved_ca <- lapply(field_name, function(X){D[[X]]})
    names(retrieved_ca) <- field_name
    for(m in 1:length(field_name)){
      temp <- lapply(1:agent_n, function(i){which(D[[field_name[m]]]==i, arr.ind = ca_arr.ind)})
      posit <- do.call(rbind, temp)
      if(is.null(colnames(posit))){
        colnames(posit) <- field_name[m]
      }else{
        colnames(posit) <- paste0(field_name[m],"_",colnames(posit))
      }
      attr_ca <- cbind(attr_ca, posit)
    }
    out <- attr_ca
  }else{
  # logから取得する場合
    # 時間を計算
    times <- unlist(lapply(D$log, function(X){X$time}))
    log_attr_ca <- vector("list", length(times))
    names(log_attr_ca) <- names(times)
    for(t in 1:time){
      # 空のpositを作成
      attr_ca <- matrix(0, agent_n, 1)
      attr_ca <- data.frame(attr_ca[,-1])
      # caを取得する
      retrieved_ca <- lapply(field_name, function(X){D$log[[t]][[X]]})
      names(retrieved_ca) <- field_name
      for(m in 1:length(field_name)){
        temp <- lapply(1:agent_n, function(i){which(D$log[[t]][[field_name[m]]]==i, arr.ind = ca_arr.ind)})
        posit <- do.call(rbind, temp)
        if(is.null(colnames(posit))){
          colnames(posit) <- field_name[m]
        }else{
          colnames(posit) <- paste0(field_name[m],"_",colnames(posit))
        }
        attr_ca <- cbind(attr_ca, posit)
      }
      log_attr_ca[[t]] <- attr_ca
    }
    out <- log_attr_ca
  }
  out
}


#-------------------------------------------------------------------------------
# .get_euc_for_attr
#-------------------------------------------------------------------------------


.get_euc_for_attr <- function(D, from_log = FALSE, attr_long = FALSE){
  field_name <- names(D)[!names(D) %in% c(".__enclos_env__",".f","notes","log","time","agent","clone",".save")]
  field_type <- unlist((lapply(field_name, function(X){attr(D[[X]], "field_type")})))

  # もしも一つもeucがなければ、NULLを返す
  if(all(field_type != "euc")){
    return(NULL)
  }

  # eucのfield_nameを特定
  field_name <- field_name[field_type=="euc"]

  # agentの数を特定
  agent_n <- length(D$agent)

  # 現在のDから取得する場合
  if(from_log == FALSE){
    fieldwise_euc <- lapply(1:length(field_name), function(p){
      temp <- D[[field_name[p]]]
      colnames(temp) <- paste0(field_name[p], ".", colnames(temp))
      temp
    })
    out <- do.call(data.frame, fieldwise_euc)
  }else{
  # logから取得する場合
    out <- lapply(1:length(D$log), function(t){
      D_t <- D$log[[t]]
      D_t_p <- lapply(1:length(field_name), function(p){
        temp <- D_t[[field_name[p]]]
        colnames(temp) <- paste0(field_name[[p]], ".", colnames(temp))
      })
      do.call(data.frame, D_t_p)
    })
    ## attr_long = FALSEの場合
    if(attr_long == FALSE){
      names(out) <- names(D$log)
    }else{
    ## attr_long = TRUEの場合
      times <- unlist(lapply(D$log, function(X){X$time}))
      out <- lapply(1:length(out), function(t){
        data.frame(out[[t]], time = t)
      })
      out <- do.call(rbind, out)
    }
  }
  # リターン
  out
}

#===============================================================================
# attr_notes
#===============================================================================

.get_notes_for_attr <- function(D, from_log = FALSE, attr_long = FALSE){
  # agent_n
  agent_n <- length(D$agent)
  # 現在の時間から
  if(from_log==FALSE){
    # names
    note_names <- names(D$notes)
    # notesを取得する
    D_notes <- (lapply(1:length(D$notes), function(p){
      ## D$notesの要素の長さは1
      if(length(D$notes[[p]])==1){
        rep(D$notes[[p]], agent_n)
      }else if(length(D$notes[[p]])==agent_n){
        D$notes[[p]]
      }else{
        warning(
          paste0("The length of ",note_names[[p]],  " does not match the number of agents. The first element is recycled.")
        )
        rep(D$notes[[p]][1], agent_n)
      }
    }))
    out <- do.call(data.frame, D_notes)
    colnames(out) <- note_names
  }else{
  # logから取得する場合
    times <- unlist(lapply(D$log, function(X){X$time}))
    out <- lapply(1:length(times), function(t){
      notes_t <- D$log[[t]]$notes
      notes_t_temp <- (lapply(1:length(notes_t), function(p){
        ## D$notesの要素の長さは1
        if(length(notes_t[[p]])==1){
          rep(notes_t[[p]], agent_n)
        }else if(length(notes_t[[p]])==agent_n){
          notes_t[[p]]
        }else{
          warning(
            paste0("The length of ",note_names[[p]],  " does not match the number of agents. The first element is recycled.")
          )
          rep(notes_t[[p]][1], agent_n)
        }
      }))
      notes_t_retrieved <- do.call(data.frame, notes_t_temp)
      colnames(notes_t_retrieved) <- names(D$log[[t]]$notes)
      notes_t_retrieved
    })
    ## attr_long == FALSEの場合
    if(attr_long==FALSE){
      names(out) <- names(D$log)
    }else{
    ## attr_long == TRUEの場合
      out <- lapply(1:length(out), function(t){
        data.frame(out[[t]], time = times[[t]])
      })
      out <- do.call(dplyr::bind_rows, out)
    }
  }
    # リターン
    out
  }

