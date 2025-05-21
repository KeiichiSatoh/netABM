#' @title Modifying a set of ABM_Agent class objects
#' @description This function modifies ABM_Agent class objects
#' @details
#' "rename"や"remove"を行う場合には、注意が必要。もしもこれらのフィールドを、
#' act_FUNやactive_bindingが参照している場合にはオブジェクトが適切に動かなくなる。
#' このため、特にこれらの作業を行う場合には、デフォルトのdeep_cloneをTRUEにし、
#' 変更後のオブジェクトがもとのDオブジェクトとは異なる名前をもつ別のオブジェクト
#' を用意したうえで、動作を確認することが勧められる。
#'
#' 動作が確認できれば、あとはmodify_D関数を用いて、もとのDオブジェクトにコピーし直してもよい。
#'
#' @import R6
#' @export
#' @examples
#' # Create a simple D object with a set of agents
#' person <- init_agent(n = 3)
#' D <- setABM(agents = person)
#'
#' # Add an 'age' attribute to each agent
#' D2 <- modify_agents(D = D, agents_name = "person",
#'                    what = "add_attr", name = "age", attr = c(1,2,3))
#'
#' # Add a function 'print_age' as an action function (act_FUN) for each agent
#' print_age <- function(beta = 1){beta*self$age}
#'
#' D2 <- modify_agents(D = D2, agents_name = "person", what = "add_act_FUN",
#'                    name = "print_age", act_FUN = print_age)
#'
#' # Add another 'print_age' function with a default beta value of 2
#' D2 <- modify_agents(D = D2, agents_name = "person", what = "add_act_FUN",
#'                    name = "print_age2", act_FUN = "print_age(beta = 2)")
#'
#' # Add an active binding 'sqrt_age' to calculate the square root of 'age'
#' sqrt_age <- function(){sqrt(self$age)}
#' D2 <- modify_agents(D = D2, agents_name = "person", what = "add_active_binding",
#'                    name = "sqrt_age", active_binding = sqrt_age)
#'
#' # Copy 'age' to create a new attribute 'age2'
#' D2 <- modify_agents(D = D2, agents_name = "person", what = "copy",
#'                    copy_from = "age", name = "age2")
#'
#' # Rename 'age2' to 'age3'
#' D2 <- modify_agents(D = D2, agents_name = "person", what = "rename",
#'                    rename_from = "age2", name = "age3")
#'
#' # Remove the 'age3' attribute
#' D2 <- modify_agents(D = D2, agents_name = "person", what = "remove",
#'                     name = "age3")
#'
#' # Setting deep_clone to FALSE modifies the original input object 'D' directly
#' modify_agents(D = D, agents_name = "person",
#'               what = "add_attr", name = "age3", attr = c(4,5,6),
#'               deep_clone = FALSE)


modify_agents <- function(
    D, agents_name, name,
    attr = NULL, act_FUN = NULL, active_binding = NULL,
    rename_from = NULL, copy_from = NULL,
    what = c("add_attr", "add_act_FUN", "add_active_binding",
             "rename", "copy", "remove"),
    deep_clone = TRUE){

  # やることを特定
  if(all(what == c("add_attr", "add_act_FUN", "add_active_binding",
               "rename", "copy", "remove"))){
    cat("Note: 'what' is set as the default for 'add_attr'.", "\n")
  }
  what <- match.arg(what)

  # nameが長さ１のcharacterか
  stopifnot("'name' must be a character vector of length 1." = is.character(name) && length(name)==1)

  # deep_clone
  if(deep_clone){
    D <- D$clone(deep = TRUE)
  }else{
    D <- D$clone(deep = FALSE)
  }

  # やることに応じて動作
  switch(what,
         "add_attr" = {
           ## attrの存在を確認
           stopifnot("Please provide a value for 'attr'" = !is.null(attr))
           stopifnot("The 'attr' argument must be a vector, not a data.frame." = NCOL(attr)==1)
           stopifnot("The 'attr' argument must be a vector, not a list." = !is.list(attr))

           ## agentの数
           n <- length(D[[agents_name]])
           # agentの数とattrの長さがあっているか
           stopifnot("The length of 'attr' must match the number of agents." = length(attr)==n)

           ## add
           for(i in 1:n){
             D[[agents_name]][[i]]$.add_field(name = name, value = attr[i])
           }

           ## message
           cat(paste0("'", name , "'", " has been added to each field of '", agents_name, "'."))

         # "add_attr"--------#
         },
         "add_act_FUN" = {
           ## act_FUNの存在を確認
           stopifnot("Please provide a function for 'act_FUN'" = !is.null(act_FUN))

           ## agentの数
           n <- length(D[[agents_name]])

           ## act_FUNが長さが1の場合には、それを人数分コピー
           if(length(act_FUN)==1){
             if(is.list(act_FUN)){act_FUN <- act_FUN[[1]]}
             act_FUN_list <- lapply(1:n, function(x){act_FUN})
           }else{
             act_FUN_list <- act_FUN
           }

           ## 人数があっているかを確認
           stopifnot("The length of act_FUN must match the number of agents." = length(act_FUN_list)==n)

           ## すべての形をfunction型に揃える
           for(i in 1:n){
             if(is.character(act_FUN_list[[i]])){
               parsed_FUN <- parse(text = act_FUN_list[[i]])[[1]]
               if(is.name(parsed_FUN)){
                 retrieved_FUN <- get(parsed_FUN)
                 stopifnot("The 'act_FUN' retrieved from the specified object must be a function." = is.function(retrieved_FUN))
                 act_FUN_list[[i]] <- retrieved_FUN
               }else if(is.call(parsed_FUN)){
                 retrieved_FUN <- get(call_name(parsed_FUN))
                 stopifnot("The 'act_FUN' retrieved from the specified object must be a function." = is.function(retrieved_FUN))
                 formals(retrieved_FUN) <- call_args(parsed_FUN)
                 act_FUN_list[[i]] <- retrieved_FUN
               }
             }
           }

           ## すべてfunction型になっているか確認
           stopifnot("act_FUN must be a function." = all(unlist(lapply(act_FUN_list, is.function))))

           ## D = D, E = Eをformalsに加える
           for(i in 1:n){
             FUN <- act_FUN_list[[i]]
             retrieved_formals <- formals(FUN)
             retrieved_formals[names(retrieved_formals)=="D"|names(retrieved_formals)=="E"] <- NULL
             formals(FUN) <- c(retrieved_formals, alist(D = D, E = E))
             body(FUN) <- as.call(append(as.list(body(FUN)), expression(self <- self), after=1))
             act_FUN_list[[i]] <- FUN
           }

           ## すべてのact_FUNを付与する
           for(i in 1:n){
             D[[agents_name]][[i]]$.add_method(name, act_FUN_list[[i]])
           }

           ## message
           cat(paste0("act_FUN '", name , "'", " has been added to each field of '", agents_name, "'."))

         # add_act_FUN----------------#
         },
         "add_active_binding" = {
           ## active_bindingの存在を確認
           stopifnot("Please provide a function for 'active_binding'" = !is.null(active_binding))

           ## agentの数
           n <- length(D[[agents_name]])

           ## act_FUNが長さが1の場合には、それを人数分コピー
           if(length(active_binding)==1){
             if(is.list(active_binding)){active_binding <- active_binding[[1]]}
             active_binding_list <- lapply(1:n, function(x){active_binding})
           }else{
             active_binding_list <- active_binding
           }

           ## 人数があっているかを確認
           stopifnot("The length of active_binding must match the number of agents." = length(active_binding_list)==n)

           ## すべての形をfunction型に揃える
           for(i in 1:n){
             if(is.character(active_binding_list[[i]])){
               parsed_FUN <- parse(text = active_binding_list[[i]])[[1]]
               if(is.name(parsed_FUN)){
                 retrieved_FUN <- get(parsed_FUN)
                 stopifnot("The 'active_binding' retrieved from the specified object must be a function." = is.function(retrieved_FUN))
                 active_binding_list[[i]] <- retrieved_FUN
               }else if(is.call(parsed_FUN)){
                 retrieved_FUN <- get(call_name(parsed_FUN))
                 stopifnot("The 'active_binding' retrieved from the specified object must be a function." = is.function(retrieved_FUN))
                 formals(retrieved_FUN) <- call_args(parsed_FUN)
                 active_binding_list[[i]] <- retrieved_FUN
               }
             }
           }

           ## すべて関数型になっているか確認
           stopifnot("act_FUN must be a function." = all(unlist(lapply(act_FUN_list, is.function))))

           ## すべてのact_FUNを付与する
           for(i in 1:n){
             D[[agents_name]][[i]]$.add_active_binding(name = name,
                                                       FUN = active_binding_list[[i]])
           }

           ## message
           cat(paste0("active_binding '", name , "'", " has been added to each field of '", agents_name, "'."))

         # active_binding-------------------------#
         },
         "rename" = {
           ## rename_fromの存在を確認
           stopifnot("Please provide a value for 'rename_from'" = !is.null(rename_from))
           ## character_vectorか
           stopifnot("'rename_from' must be a character of length 1." = length(rename_from)==1 & is.character(rename_from))

           ## agentの数
           n <- length(D[[agents_name]])

           # copy & delete
           for(i in 1:n){
             value <- D[[agents_name]][[i]][[rename_from]]
             D[[agents_name]][[i]]$.add_field(name = name, value = value)
             D[[agents_name]][[i]]$.remove_field(field_name = rename_from)
           }

           ## message
           cat(paste0("The '", rename_from, "' field of '", agents_name, "' has been renamed to '", name, "'."))

         # rename--------------------#
         },
         "copy" = {
           ## copy_fromの存在を確認
           stopifnot("Please provide a value for 'copy_from'" = !is.null(copy_from))
           ## character_vectorか
           stopifnot("'copy_from' must be a character of length 1." = length(copy_from)==1 & is.character(copy_from))

           ## agentの数
           n <- length(D[[agents_name]])

           # copy
           for(i in 1:n){
             value <- D[[agents_name]][[i]][[copy_from]]
             D[[agents_name]][[i]]$.add_field(name = name, value = value)
           }

           ## message
           cat(paste0("The '", copy_from, "' field of '", agents_name, "' has been copied to '", name, "'."))

          # copy---------------------#
         },
         "remove" = {
           ## agentの数
           n <- length(D[[agents_name]])

           # copy & delete
           for(i in 1:n){
             D[[agents_name]][[i]]$.remove_field(field_name = name)
           }

           ## message
           cat(paste0("The '", name, "' field in '", agents_name, "' has been removed.","\n"))

          # delete--------------------#
         }
         )
  # return
  if(deep_clone){
    return(D)
  }else{
    cat("\n", "\n", "Note: Since 'deep_clone' is set to FALSE, modifications have been made directly to the input object 'D'.")
    invisible(NULL)
  }
}

