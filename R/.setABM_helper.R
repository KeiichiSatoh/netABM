
#---------------------------------------------------------
# .shape_agent_attr
#---------------------------------------------------------
.shape_agent_attr <- function(agent_attr_subst = NULL){
  # substituteされているオブジェクト名
  obs_name <- as.character(agent_attr_subst)
  # substituteを評価して中身を取り出す
  agent_attr <- eval(agent_attr_subst)
  # NULLの場合にはNULLを返す
  if(is.null(agent_attr)){
    return(NULL)}
  # なんらか投入されている場合data.frameの形に揃える
  ### リストの場合
  if(is.list(agent_attr)){
    if(is.null(names(agent_attr))){
      names(agent_attr) <- paste0("X", 1:length(agent_attr))
    }
    agent_attr <- as.data.frame(agent_attr, col.names = names(agent_attr))
  }else if(is.vector(agent_attr)){
    ### ベクトルの場合
    agent_attr <- data.frame(agent_attr)
    if(length(obs_name)>1){
      obs_name <- obs_name[length(obs_name)]
    }
    colnames(agent_attr) <- obs_name
  }else if(is.matrix(agent_attr)){
    agent_attr <- as.data.frame(agent_attr)
  }
  # アウトプット
  agent_attr
}


