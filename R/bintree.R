# bintree.r

newbintree <- function(){
  tree <- new.env(parent=globalenv()) 
  tree$vals <- matrix(cbind(NA,NA,NA,NA),nrow=1,ncol=4, dimnames = list(c(NULL),
                                                      c("Value","Left", "Right", "Count")) ) 
  colnames(tree, do.NULL = FALSE)
  class(tree) <- append(class(tree),"bintree")
  return(tree)
}

push <- function(tree, val) UseMethod("push")
pop <- function(tree) UseMethod("pop")

#################### push ####################
push.bintree <- function(tree,val) {
     if(nrow(tree$vals) == 1){ # Empty Tree
        tree$vals <- rbind(tree$vals,c(val,NA,NA,1))
        return(tree$vals)
     }
     if(nrow(tree$vals) > 1) { # Check for null root
        if(is.na(tree$vals[2,1])){
          tree$vals[2,] <- c(val,NA,NA,1)
          return(tree$vals)
        }
      }
     tree$vals <- push_helper(2,val,tree$vals)
     return(tree$vals)
}
push_helper <- function(root, inVal, graph){
  root <- as.numeric(root)
  # base case, insert as left child
  if(inVal < graph[root,1] & is.na(graph[root,2])){
    rowToInsert <- push_firstNArow(graph) # find avail row
    if(rowToInsert == -1) { # create new row
      graph <- rbind(graph,c(inVal,NA,NA,1))
      graph[root,2] <- nrow(graph) # update parent node's left child value
    } else {
      graph[rowToInsert,] <- c(inVal,NA,NA,1) # put val in found NA row
      graph[root,2] <- rowToInsert
    }
    return(graph)
  }
  # base case, insert as right child
  if(inVal > graph[root,1] & is.na(graph[root,3])){ # base case, insert to right
    rowToInsert <- push_firstNArow(graph) # find avail row
    if(rowToInsert == -1) { # create new row
      graph <- rbind(graph,c(inVal,NA,NA,1))
      graph[root,3] <- nrow(graph) # update parent node's right child value
    } else {
      graph[rowToInsert,] <- c(inVal,NA,NA,1) # put val in found NA row
      graph[root,3] <- rowToInsert
    }
    return(graph)
  }
  # base case, duplicate item
  if(inVal == graph[root,1]) {
    graph[root,4] <- as.numeric(graph[root,4]) + 1 # increment count
    return(graph)
  }
  # search left tree
  if(inVal < graph[root,1]) push_helper(graph[root,2],inVal,graph)
  # search right tree
  else push_helper(graph[root,3],inVal,graph)
}

# helper method that finds the first row of NA's
# in graph so to insert new value appropriately
push_firstNArow <- function(graph) {
  for(i in 2:nrow(graph)) {
    if(all(is.na(graph[i,]))) return(i)
  }
  return(-1) # all rows taken, return -1
}

#################### pop ####################
pop.bintree <- function(tree) {
  if(nrow(tree$vals) < 2 || is.na(tree$vals[2,1])) stop("Tree is empty")
  if(is.na(tree$vals[2,2])) { # checks if root is smallest item (no left subtree)
    if(is.na(tree$vals[2,3])) { # checks if there is only one item in tree (no right subtree)
      top <- tree$vals[2,1]
      if(tree$vals[2,4] == 1) tree$vals[2,] <- NA # check if duplicates remain
      else tree$vals[2,4] <- as.numeric(tree$vals[2,4]) - 1
      return(top)
    }
    if(tree$vals[2,4] == 1) { # move right subtree to root (row 2 in matrix)
      newRoot <- as.numeric(tree$vals[2,3])
      top <- tree$vals[2,1]
      tree$vals[2,] <- tree$vals[newRoot,]
      tree$vals[newRoot,] <- NA
      return(top)
    } else { # duplicates remain, decrement count
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
  # base case, found smallest element
  if(is.na(tree$vals[row,2])) { 
    top <- tree$vals[row,1] # value to return
    # next we check for duplicates and whether to move right subtree up
    if(tree$vals[row, 4] == 1) { # no duplicates remaining
      rTree <- tree$vals[row,3]
      if(!is.na(rTree)) { # right subtree exists
        tree$vals[parent, 2] <- rTree # update parent to new subtree
        tree$vals[row,] <- NA
      } else {
        tree$vals[parent,2] <- NA
        tree$vals[row,] <- NA
      }
      return(top)
    } else { # duplicate remain, decrement count
      tree$vals[row, 4] <- as.numeric(tree$vals[row, 4]) - 1
    }
    return(top)
  }
  pop_helper(tree, tree$vals[as.numeric(row),2], row)
}

#################### print ####################
print.bintree <- function(tree) {
  obj <- tree$vals
  colnames(obj) <- NULL
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


