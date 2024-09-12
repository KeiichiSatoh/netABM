# .get_attr_from_caのログなし版
.get_attr_from_ca <- function(D, ID_ca, attr_ca = NULL, with_agent_attr = FALSE,
                              incl_posit = FALSE, which_attr = NULL){
  # データを取得する
  ID_ca_retrieved <- D[[ID_ca]]
  attr_ca_retrieved <- lapply(attr_ca, function(p){D[[p]]})
  names(attr_ca_retrieved) <- attr_ca

  # ID_caから得られるagent ID
  agent_ID <- sort(unique(as.vector(ID_ca_retrieved, mode = "numeric")))
  agent_ID <- agent_ID[agent_ID != 0]

  # positを取得
  posit <- unlist(lapply(agent_ID, function(i){which(i==ID_ca_retrieved)}))
  # attr_caから各agentのattrを取得
  out <- data.frame(lapply(attr_ca_retrieved, function(X){X[posit]}))
  # positを含めるか
  if(incl_posit==TRUE){
    out <- data.frame(out, posit = posit)
  }
  # agent_attrを含めるか
  if(with_agent_attr==TRUE){
    agent_attr <- get_agent_attr(D, which_attr = which_attr)
    agent_attr <- agent_attr[agent_attr$ID %in% agent_ID, ] # ID_caに入っているIDのものだけとる
    out <- data.frame(out, agent_attr)
    out <- out[,-which(colnames(out)=="ID")]
  }
  # リターン
  out <- data.frame(ID = agent_ID, out)
  out
}
