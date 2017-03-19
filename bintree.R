newbintree <- function(){
  tree <- new.env(parent=globalenv()) 
  tree$vals <- matrix(cbind(NA,NA,NA),nrow=1,ncol=3, dimnames = list(c(NULL),
                                                      c("Value","Left", "Right")) ) 
  colnames(tree, do.NULL = FALSE)
  class(tree) <- append(class(tree),"bintree")
  return(tree)
}

push <- function(obj, val) UseMethod("push")
pop <- function(obj, val) UseMethod("pop")

# val <- graph[root,1]
# left <- graph[root,2]
# right <- graph[root,3]
# root <- the row of the node
helper <- function(root, inVal ,graph){
  if(inVal < graph[root,1] & is.na(graph[root,2])){ #BASE CASE
    graph <- rbind(graph,c(inVal,NA,NA))
    graph[root,2] <- nrow(graph)
    return(graph)
  }
  if(inVal > graph[root,1] & is.na(graph[root,3])){ #BASE CASE
    graph <- rbind(graph,c(inVal,NA,NA))
    graph[root,3] <- nrow(graph)
    return(graph)
  }
  if(inVal < graph[root,1]) helper(graph[root,2],inVal,graph)

  else helper(graph[root,3],inVal,graph)
}

push.bintree <- function(obj,val) {
     if(nrow(obj$vals) == 1){ # Empty Tree
        obj$vals <- rbind(obj$vals,c(val,NA,NA))
     }
     else obj$vals <- helper(2,val,obj$vals)
     return(obj$vals)
}

pop_helper <- function(root,graph,path=c()){
  if(is.na(graph[root,2])){ #Base Case
    return(path)
  }
  path <- c(path,graph[root,2])
  pop_helper(graph[root,2],graph,path)
}

pop.bintree <- function(tree){
  path <- pop_helper(2,tree$vals)
  node <- tail(path,1)
  parent <- tail(path,2)[1]
  top <- tree$vals[node,1]
  # Update the structure 
  tree$vals[parent,2] <- NA
  tree$vals <- tree$vals[-node,]
  # Update the left and right columns
  m <-  matrix(rbind(0,1,1),ncol=3)
  m <- matrix(rep(m,each=nrow(tree$vals)),nrow=nrow(tree$vals)) 
  tree$vals <- tree$vals - m
  return(top)
}


print.bintree <- function(tree) {
  obj <- tree$vals
  printhelper <- function(obj, row) {
    if(!is.na(obj[as.numeric(row),2])) {
      printhelper(obj,obj[as.numeric(row),2])
    }
    print(obj[as.numeric(row),1])
    if(!is.na(obj[as.numeric(row),3])) {
      printhelper(obj,obj[as.numeric(row),3])
    }
  }
  if(nrow(obj) > 1) printhelper(obj,2)
}

######## TEST CASE #################
tree <- newbintree()
push(tree,7)
push(tree,5)
push(tree,6)
push(tree,9)
push(tree,4)
push(tree,1)
push(tree,30)
push(tree,8)
push(tree,10)



