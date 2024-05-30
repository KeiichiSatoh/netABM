
## Library
#' library(reshape)
#' library(ggplot2)
#' library(RColorBrewer)
#'
#' ## Example
#' attr <- data.frame(X1 = as.character(c(1,1,1,2,2)))
#' D <- setABM_ca(agent_n = 5, agent_attr = attr)
#' plot_ca(D, attr = "X1")


# 関数
plot_ca <- function(D = NULL, which_ca = 1, CA = NULL, attr = NULL, show.ID = TRUE){
  # CAが直接投入されている場合にはそちらを利用
  # そうでない場合にはDとwhich_caから取得
  if(is.null(CA)){
    # CAを特定する
    if(is.numeric(which_ca)){
      ca_namelist <- names(D$ca)[!names(D$ca) %in% c(".__enclos_env__","clone","print")]
      which_ca <- ca_namelist[which_ca]
    }
    CA <- D$ca[[which_ca]]
  }

  # CAの次元を確認
  CA_dim <- dim(CA)

  # 2次元の場合
  if(length(CA_dim)==2){
    # ggplotで正しく表示されるようマトリクスを90右に回転させる
    CA_rotate <- t(apply(CA, 2, rev))
    # マトリクスをデータフレーム形式に
    CA_melt <- melt(CA_rotate)
  }else{
    # 3次元の場合
    CA_rotate <- array(0, dim = CA_dim)
    CA_melt <- vector("list",CA_dim[3])
    for(i in 1:CA_dim[3]){
      CA_rotate[,,i] <- t(apply(CA[,,i],2,rev))
      CA_melt[[i]] <- melt(CA_rotate[,,i])
      # 2階以上は1階部分に足していく
      CA_melt[[i]][,2] <- CA_melt[[i]][,2]+(i-1)*CA_dim[2]
    }
    # CA_meltを盾につなげる
    CA_melt_rbind <- CA_melt[[1]]
    for(i in 2:CA_dim[3]){
      CA_melt_rbind <- rbind(CA_melt_rbind, CA_melt[[i]])
    }
    CA_melt <- CA_melt_rbind
  }

  # CA_meltの表示名を調整
  colnames(CA_melt) <- c("row","col","ID")
  CA_melt$exist <- CA_melt$ID > 0
  CA_melt$ID[CA_melt$ID == 0] <- ""

  # Attrが指定されている場合には取得し、CA_meltにマージする
  if(!is.null(attr)){
    attr_vect <- unlist(lapply(1:length(D$agent), function(i){
      D$agent[[i]]$a[[attr]]
    }))
    attr_vect <- data.frame(ID = 1:length(attr_vect),
                            attr = attr_vect)
    CA_melt <- merge(CA_melt, attr_vect, by = "ID", all.x = TRUE)
    CA_melt[is.na(CA_melt)] <- 0
  }

  # IDをそのまま表示
  ## エージェントの位置のみの場合
  if(is.null(attr)){
    p <- ggplot(CA_melt, aes(x = row, y = col, fill = exist)) +
      geom_tile(color = "white", lwd = 0.5, linetype = 1) +
      scale_fill_brewer(palette = 1) +
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
      theme(legend.position = "none") +
      ggtitle(which_ca)
    if(show.ID){
      p <- p + geom_text(aes(label = ID), color = "black")
    }
  }else{
    if(is.numeric(CA_melt$attr)){
      ## 数値の場合
      p <- ggplot(CA_melt, aes(x = row, y = col, fill = scale(attr))) +
        geom_tile(color = "white", lwd = 0.5, linetype = 1) +
        scale_fill_gradient2(name = attr, midpoint = 0, low = "blue", mid = "white", high = "red") +
        theme(
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank()) +
        ggtitle(which_ca)
      if(show.ID){
        p <- p + geom_text(aes(label = ID), color = "black")
      }
    }else{
      ## attributeがカテゴリーの場合
      p <- ggplot(CA_melt, aes(x = row, y = col, fill = factor(attr))) +
        geom_tile(color = "white", lwd = 0.5, linetype = 1) +
        scale_fill_brewer(name = attr, palette = "Set2") +
        theme(
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank()) +
        ggtitle(which_ca)
      if(show.ID){
        p <- p + geom_text(aes(label = ID), color = "black")
      }
    }
  }
  #アウトプット
  p
}



