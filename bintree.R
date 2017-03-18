newbintree <- function(){
  vals <- matrix(cbind(NA,NA,NA),nrow=1,ncol=3, dimnames = list(c(NULL),
                                                      c("Value","Left", "Right")) ) 
  colnames(tree, do.NULL = FALSE)
  class(vals) <- append(class(vals),"bintree")
  return(vals)
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
     if(nrow(obj) == 1){ # Empty Tree
        obj <- rbind(obj,c(val,NA,NA))
     }
     else obj <- helper(2,val,obj)
     class(obj) <- append(class(obj),"bintree")
     return(obj)
}


print.bintree <- function(obj) {

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

tree <- newbintree()
tree <- push(tree,7)
tree <- push(tree,5)
tree <- push(tree,6)
tree <- push(tree,9)
tree <- push(tree,4)
tree <- push(tree,1)
tree <- push(tree,30)
tree <- push(tree,8)
tree <- push(tree,10)


