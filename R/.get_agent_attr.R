# get_agent_attrのベアーボーン関数

.get_agent_attr <- function(D, which_attr = which_attr){
  # attrをリストで取得する
  agent_attr_list <- lapply(1:length(D$agent), function(i){
    attr_i <- data.frame(lapply(which_attr, function(p){
      D$agent[[i]][[p]]
    }))
    names(attr_i) <- which_attr
    attr_i
  })
  # まとめる
  do.call(rbind, agent_attr_list)
}
