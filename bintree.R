newbintree <- function(){
  tree <- new.env(parent=globalenv()) 
  tree$vals <- matrix(cbind(NA,NA,NA,NA),nrow=1,ncol=4, dimnames = list(c(NULL),
                                                      c("Value","Left", "Right", "Count")) ) 
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
push_helper <- function(root, inVal ,graph){
  root <- as.numeric(root)
  if(inVal < graph[root,1] & is.na(graph[root,2])){ #BASE CASE
    rowToInsert <- push_firstNArow(graph)
    if(rowToInsert == -1) {
      graph <- rbind(graph,c(inVal,NA,NA,1))
      graph[root,2] <- nrow(graph)
    } else {
      graph[rowToInsert,] <- c(inVal,NA,NA,1)
      graph[root,2] <- rowToInsert
    }
    return(graph)
  }
  if(inVal > graph[root,1] & is.na(graph[root,3])){ #BASE CASE
    rowToInsert <- push_firstNArow(graph)
    if(rowToInsert == -1) {
      graph <- rbind(graph,c(inVal,NA,NA,1))
      graph[root,3] <- nrow(graph)      
    } else {
      graph[rowToInsert,] <- c(inVal,NA,NA,1)
      graph[root,3] <- rowToInsert
    }
    return(graph)
  }
  if(inVal == graph[root,1]) { # base case, duplicate item
    graph[root,4] <- as.numeric(graph[root,4])+1
    return(graph)
  }
  if(inVal < graph[root,1]) push_helper(graph[root,2],inVal,graph)

  else push_helper(graph[root,3],inVal,graph)
}

push_firstNArow <- function(graph) {
  for(i in 2:nrow(graph)) {
    if(all(is.na(graph[i,]))) return(i)
  }
  return(-1)
}

push.bintree <- function(obj,val) {
     if(nrow(obj$vals) == 1){ # Empty Tree
        obj$vals <- rbind(obj$vals,c(val,NA,NA,1))
        return(obj$vals)
     }
     if(nrow(obj$vals) > 1) { #possibly empty
        if(is.na(obj$vals[2,1])){
          obj$vals[2,] <- c(val,NA,NA,1)
          return(obj$vals)
        }
      }
     obj$vals <- push_helper(2,val,obj$vals)
     return(obj$vals)
}

pop.bintree <- function(tree) {
  if(nrow(tree$vals) < 2 || is.na(tree$vals[2,1])) stop("Tree is empty")
  if(is.na(tree$vals[2,2])) { # checks if root is smallest item
    if(is.na(tree$vals[2,3])) { # checks if there is only one item in tree
      top <- tree$vals[2,1]
      if(tree$vals[2,4] == 1) tree$vals[2,] <- NA
      else tree$vals[2,4] <- as.numeric(tree$vals[2,4]) - 1
      return(top)
    }
    if(tree$vals[2,4] ==1) {
      newRoot <- as.numeric(tree$vals[2,3]) # right subtree exists, assign new root to row 2
      top <- tree$vals[2,1]
      tree$vals[2,] <- tree$vals[newRoot,]
      tree$vals[newRoot,] <- NA
      return(top)
    } else {
      top <- tree$vals[2,1]
      tree$vals[2,4] <- as.numeric(tree$vals[2,4]) - 1
      return(top)
    }

  }
  pop_helper(tree, tree$vals[2,2], 2)

}

pop_helper <- function(tree, row, parent) {
  row <- as.numeric(row)
  parent <- as.numeric(parent)
  if(is.na(tree$vals[row,2])) { # base case
    top <- tree$vals[row,1] # value to return
    if(tree$vals[row, 4] == 1) { # no duplicates remaining
      rTree <- tree$vals[row,3]
      if(!is.na(rTree)) {
        tree$vals[parent, 2] <- rTree
        tree$vals[row,] <- NA
      } else {
        tree$vals[parent,2] <- NA
        tree$vals[row,] <- NA
      }
      return(top)
    } else { # dupliate remain, decrement count
      tree$vals[row, 4] <- as.numeric(tree$vals[row, 4]) - 1
    }
    return(top)
  }
  pop_helper(tree, tree$vals[as.numeric(row),2], row)
}

print.bintree <- function(tree) {
  obj <- tree$vals
  printhelper <- function(obj, row) {
    if(!is.na(obj[as.numeric(row),2])) {  # print left tree
      printhelper(obj,obj[as.numeric(row),2])
    }
    for(i in 1:obj[as.numeric(row),4]) print(obj[as.numeric(row),1]) # print root
    if(!is.na(obj[as.numeric(row),3])) {  # print right subtree
      printhelper(obj,obj[as.numeric(row),3])
    }
  }
  if(nrow(obj) > 1) {
    if(!is.na(obj[2,1])) printhelper(obj,2)
    else print("Tree is empty.")
  } else {
    print("Tree is empty.")
  }
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



