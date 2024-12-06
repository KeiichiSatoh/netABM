stage_ca_neib <- function(stage = NULL,
                          stage_name = NULL,
                          D = NULL,
                          neib_type = c("moore", "newmann", "horizontal", "vertial"),
                          torus = FALSE,
                          include_ego = FALSE,
                          radius = c(1,1),
                          FUN = NULL, ...){

  # stageが直接投入されていない場合には名前からstageを取得
  if(is.null(stage)){
    # Dが投入されている場合にはそれをselfに
    if(!is.null(D)){
      self <- D
    }
    stage <- self[[stage_name]]
  }

  # neib_type
  neib_type <- match.arg(neib_type)

  # dimension
  stage_dim <- dim(stage)

  # switch
  switch(neib_type,
         "neumann" = {
           # 空のarray
           stage_array <- array(0, dim = c(stage_dim, 4))
           # 近傍の値を取得する
           if(torus){
             stage_array[,,1] <- cbind(stage[ ,-1], stage[ ,1]) # 右側
             stage_array[,,2] <- cbind(stage[ ,stage_dim[2]], stage[ ,-stage_dim[2]]) # 左側
             stage_array[,,3] <- rbind(stage[stage_dim[1], ], stage[-stage_dim[1], ]) # 上側
             stage_array[,,4] <- rbind(stage[-1, ], stage[stage_dim[1], ]) # 下側
           }else{
             stage_array[,,1] <- cbind(stage[ ,-1], NA)
             stage_array[,,2] <- cbind(NA, stage[ ,-stage_dim[2]])
             stage_array[,,3] <- rbind(NA, stage[-stage_dim[1], ])
             stage_array[,,4] <- rbind(stage[-1, ], NA)
           }
           # ベクトル化
           neib_vec <- matrix(as.vector(stage_array), nrow = prod(stage_dim))
           dimnames(neib_vec) <- list(1:prod(stage_dim), c("R","L","U","D"))
         },
         "moore" = {
           # 空のarray
           stage_array <- array(0, dim = c(stage_dim, 8))
           # 近傍の値を取得する
           if(torus){
             R <- stage_array[,,1] <- cbind(stage[ ,-1], stage[ ,1]) # 右側
             L <- stage_array[,,2] <- cbind(stage[ ,stage_dim[2]], stage[ ,-stage_dim[2]]) # 左側
             stage_array[,,3] <- rbind(stage[stage_dim[1], ], stage[-stage_dim[1], ]) # 上側
             stage_array[,,4] <- rbind(stage[-1, ], stage[stage_dim[1], ]) # 下側
             stage_array[,,5] <- rbind(R[stage_dim[1], ], R[-stage_dim[1], ]) # 右上側
             stage_array[,,6] <- rbind(R[-1, ], R[stage_dim[1], ]) # 右下側
             stage_array[,,7] <- rbind(L[stage_dim[1], ], L[-stage_dim[1], ]) # 左上側
             stage_array[,,8] <- rbind(L[-1, ], L[stage_dim[1], ]) # 左下側
           }else{
             R <- stage_array[,,1] <- cbind(stage[ ,-1], NA)            # 右
             L <- stage_array[,,2] <- cbind(NA, stage[ ,-stage_dim[2]]) # 左
             stage_array[,,3] <- rbind(NA, stage[-stage_dim[1], ])　　　# 上
             stage_array[,,4] <- rbind(stage[-1, ], NA)　　　　　　　　 # 下
             stage_array[,,5] <- rbind(NA, R[-stage_dim[1], ])　　　　　# 右上
             stage_array[,,6] <- rbind(R[-1, ], NA)　    　　　　　　　 # 右下
             stage_array[,,7] <- rbind(NA, L[-stage_dim[1], ])　　　# 左上
             stage_array[,,8] <- rbind(NA, L[-1, ])　　　# 左下
           }
           # ベクトル化
           neib_vec <- matrix(as.vector(stage_array), nrow = prod(stage_dim))
           dimnames(neib_vec) <- list(1:prod(stage_dim), c("R","L","U","D","UR","DR","UL","DL"))
         },
         "horizontal" = {
           # 空のarray
           stage_array <- array(0, dim = c(stage_dim, 2))
           # 近傍の値を取得する
           if(torus){
             stage_array[,,1] <- cbind(stage[ ,-1], stage[ ,1]) # 右側
             stage_array[,,2] <- cbind(stage[ ,stage_dim[2]], stage[ ,-stage_dim[2]]) # 左側
           }else{
             stage_array[,,1] <- cbind(stage[ ,-1], NA)
             stage_array[,,2] <- cbind(NA, stage[ ,-stage_dim[2]])
           }
           # ベクトル化
           neib_vec <- matrix(as.vector(stage_array), nrow = prod(stage_dim))
           dimnames(neib_vec) <- list(1:prod(stage_dim), c("R","L"))
         },
         "vertical" = {
           # 空のarray
           stage_array <- array(0, dim = c(stage_dim, 2))
           # 近傍の値を取得する
           if(torus){
             stage_array[,,1] <- rbind(stage[stage_dim[1], ], stage[-stage_dim[1], ]) # 上側
             stage_array[,,2] <- rbind(stage[-1, ], stage[stage_dim[1], ]) # 下側
           }else{
             stage_array[,,1] <- rbind(NA, stage[-stage_dim[1], ])
             stage_array[,,2] <- rbind(stage[-1, ], NA)
           }
           # ベクトル化
           neib_vec <- matrix(as.vector(stage_array), nrow = prod(stage_dim))
           dimnames(neib_vec) <- list(1:prod(stage_dim), c("U","D"))
         }
  )

  # ego.include
  if(include_ego){
    neib_vec <- cbind(neib_vec, ego = as.vector(stage))
  }

  # FUN
  if(!is.null(FUN)){
    out <- apply(neib_vec, 1, FUN = FUN, ...)
    return(out)
  }else{
    return(neib_vec)
  }
}

