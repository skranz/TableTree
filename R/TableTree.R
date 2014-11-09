examples.standardize.stages.yaml = function() {
  setwd("D:/libraries/XEconDB/examples")
  file = "ug_stages.yaml"
  li = yaml.load_file(file)
  tt = table.tree(li)
  library(dplyr)
  n = filter(tt, name=="payoff", level==3)$num.rows[1]-1
  
  tt.find.subtree(tt,name=="stages")
  
  tt.find(tt, name=="payoff", level=3)
}

list.tree.length = function(li) {
  if (is.list(li)) {
    lens = sapply(li,list.tree.length)
    return(1+sum(lens))
  }
  return(1)
}

list.tree.to.object.list = function(li) {
 # restore.point("list.tree.to.object.list")
  if (is.list(li)) {
    sub.li = lapply(li, list.tree.to.object.list)
    sub.li = do.call(c, sub.li)
    #restore.point("list.tree.to.object.list2")
    names(sub.li)=NULL
    return(c(list(li),sub.li))
  }
  return(list(li))
}


add.list.tree.to.table = function(li, tab, row=1L, name="", level=0,parent=0) {
  row = as.integer(row)
  num.rows = 1L
  if (is.list(li)) {
    n = length(li)
    names = names(li)
    rows = as.integer((row+1):(row+n))
    srow=row+1L
    for (i in 1:n) {
      sub.num.rows = add.list.tree.to.table(li[[i]],tab,srow,names[i],level=level+1, parent=row)
      srow=srow+sub.num.rows
      num.rows = num.rows+sub.num.rows
    }
  }
  set(tab,row,2L,name)
  set(tab,row,3L,parent)
  set(tab,row,4L,num.rows)
  set(tab,row,5L,level)
  return(num.rows)
}

#' Make a table tree from a list tree
table.tree = function(li, add.obj.li = TRUE) {
  nrows = list.tree.length(li)
  library(data.table)
  tab = data.table(row.ind=1:nrows,name=rep("",nrows),parent=0,num.rows=0, level=0)
  add.list.tree.to.table(li,tab,name="game")
  if (add.obj.li) {
    obj.li = list.tree.to.object.list(li)
    attr(tab,"obj.li") = obj.li
  }
  tab
}

#' Extract the object in the specified row of a table tree
tt.object = function(tt,row) {
  attr(tt,"obj.li")[[row]]
}

#' Extract list of objects in the specified rows of a table tree
tt.objects = function(tt,rows) {
  attr(tt,"obj.li")[rows]
}

#' Extract object list of a table tree
tt.obj.li = function(tt) {
  attr(tt,"obj.li")
}

#' Extract subtree (as a table tree) starting at the specified row of a table tree
tt.subtree = function(tt,row) {
  rows = ind:(ind+tt$num.rows[row]-1)
  new.tt = tt[rows,]
  attr(new.tt,"obj.li") <- tt.obj.li(tt)[rows]
  
  new.tt$row.ind = new.tt$row.ind-row+1
  new.tt$parent = new.tt$parent-row+1
  new.tt$parent[1] = 0
  new.tt$level = new.tt$level-new.tt$level[1] 
  new.tt
}

#' Find rows of a table tree using filter on tt
tt.find.rows = function(tt,...) {
  filter(tt,...)$row.ind
}

#' Find objects of a table tree using filter on tt
tt.find.objects = function(tt,...) {
  rows = tt.find.rows(tt,...)
  tt.objects[[rows]]
}

#' Find first object of a table tree using filter on tt
tt.find.object = function(tt,...) {
  rows = tt.find.rows(tt,...)
  if (length(rows)==0)
    return(NULL)
  tt.object(tt,rows[1])
}

#' Find first subtree of a table tree using filter on tt
tt.find.subtree = function(tt,...) {
  rows = tt.find.rows(tt,...)
  if (length(rows)==0)
    return(NULL)
  tt.subtree(tt,rows[1])
}