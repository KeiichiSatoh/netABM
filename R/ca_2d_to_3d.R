ca_2d_to_3d <- function(CA, height){
  # 次元を取得
  CA_dim <- dim(CA)
  # 2次元でないときには停止
  stopifnot("The number of dimension of CA must be 2." = length(CA_dim)==2)
  # 3D化したもの
  CA_3d <- array(rep(as.vector(CA), height), dim = c(CA_dim, height))
  # アウトプット
  CA_3d
}

