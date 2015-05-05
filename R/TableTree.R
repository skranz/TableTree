examples.standardize.stages.yaml = function() {
  setwd("D:/libraries/XEconDB/examples")
  file = "ug_stages.yaml"
  library(yaml)
  li = yaml.load_file(file)
  tt = table.tree(li)
  library(dplyr)
  n = filter(tt, name=="payoff", level==3)$num.rows[1]-1
  
  tt.find.subtree(tt,name=="stages")
  
  tt = table.tree(li)  
  st = tt.find.subtree(tt, name=="stages", level==1, .assert.num=1)

  
  tt.find(tt, name=="payoff", level=3)
}

list.tree.length = function(li) {
  #restore.point("list.tree.length")
  if (is.list(li)) {
    if (length(li)==0)
      return(1)
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


add.list.tree.to.table = function(li, tab, row=1L, name="", level=0,parent=0,pos.as.child=1,num.children=0) {
  restore.point("add.list.tree.to.table")
  row = as.integer(row)
  num.rows = 1L
  num.children=0L
  if (is.list(li) & length(li)>0) {
    n = length(li)
    names = names(li)
    if (is.null(names))
      names = paste0("_UNNAMED_",1:n)
    
    rows = as.integer((row+1):(row+n))
    srow=row+1L
    for (i in 1:n) {
      #restore.point("before")
      sub.num.rows = add.list.tree.to.table(li[[i]],tab,srow,names[i],level=level+1, parent=row, pos.as.child=i)
      #restore.point("after")
      
      srow=srow+sub.num.rows
      num.rows = num.rows+sub.num.rows
    }
    num.children = length(li)
  }
  restore.point("add.list.tree.to.table_1")

  set(tab,row,2L,name)
  set(tab,row,3L,parent)
  set(tab,row,4L,level)
  set(tab,row,5L,num.rows)
  set(tab,row,6L,num.children)
  set(tab,row,7L,pos.as.child)

  
  return(num.rows)
}

#' Make a table tree from a list tree
table.tree = function(li,name="", add.obj.li = TRUE) {
  nrows = list.tree.length(li)
  library(data.table)
  tab = data.table(row.ind=1:nrows,name=rep("",nrows),parent=0,level=0,num.rows=0, num.children=0, pos.as.child=0)
  add.list.tree.to.table(li,tab,name=name)
  if (add.obj.li) {
    obj.li = list.tree.to.object.list(li)
    names(obj.li) = tab$name
    attr(tab,"obj.li") = obj.li
  }
  tab
}

#' Extract the object in the specified row of a table tree
tt.object = function(tt,row=1) {
  attr(tt,"obj.li")[[row]]
}

#' Extract list of objects in the specified rows of a table tree
tt.objects = function(tt,rows=NULL) {
  if (is.null(rows))
    return(attr(tt,"obj.li"))  
  attr(tt,"obj.li")[rows]
}

#' Extract object list of a table tree
tt.obj.li = function(tt) {
  attr(tt,"obj.li")
}

#' Extract subtree (as a table tree) starting at the specified row of a table tree
tt.subtree = function(tt,row=1) {
  rows = row:(row+tt$num.rows[row]-1)
  new.tt = tt[rows,]
  attr(new.tt,"obj.li") <- tt.obj.li(tt)[rows]
  
  new.tt$row.ind = new.tt$row.ind-row+1
  new.tt$parent = new.tt$parent-row+1
  new.tt$parent[1] = 0
  new.tt$level = new.tt$level-new.tt$level[1] 
  new.tt
}

#' Find rows of a table tree using filter on tt
tt.find.rows = function(tt,...,.assert.num=NULL) {
  rows = filter(tt,...)$row.ind
  if (!is.null(.assert.num))
    tt.assert(tt,rows,.assert.num=.assert.num)
  rows
}

tt.assert = function(tt, rows,...,.assert.num=1) {
  if (length(rows)!=.assert.num) 
    stop("Wrong number of rows found!")
  
}

#' Find objects of a table tree using filter on tt
tt.find.objects = function(tt,...) {
  rows = filter(tt,...)$row.ind

  #rows = tt.find.rows(tt,...)
  tt.objects[[rows]]
}

#' Find first object of a table tree using filter on tt
tt.find.object = function(tt,...) {
  #browser()
  rows = filter(tt,...)$row.ind
  #rows = tt.find.rows(tt,...)
  if (length(rows)==0)
    return(NULL)
  tt.object(tt,rows[1])
}

#' Find first subtree of a table tree using filter on tt
tt.find.subtree = function(tt,...) {
  rows = filter(tt,...)$row.ind
  #rows = tt.find.rows(tt,...)
  if (length(rows)==0)
    return(NULL)
  tt.subtree(tt,rows[1])
}



#' Add additional columns to a table.tree by applying fun on each object in obj.li
tt.add.extra.cols = function(tt, fun, obj.li = tt.obj.li(tt)) {
  li = lapply(obj.li, fun)
  edt = rbindlist(li)
  colnames = c("row.ind", "name", colnames(edt), colnames(tt)[-(1:2)])

  tt = cbind(tt,edt)
  setcolorder(tt, colnames)
  
  attr(tt,"obj.li") <- obj.li
  tt
}

