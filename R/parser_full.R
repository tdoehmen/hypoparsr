makeuuid = function() {
	baseuuid <- paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
	paste(
	    substr(baseuuid,1,8),
	    "-",
	    substr(baseuuid,9,12),
	    "-",
	    "4",
	    substr(baseuuid,13,15),
	    "-",
	    sample(c("8","9","a","b"),1),
	    substr(baseuuid,16,18),
	    "-",
	    substr(baseuuid,19,30),
	    sep="",
	    collapse=""
	)
}

generate_node_name = function(level,hypothesis,desc_length=100){
  level_name = parsing_hierarchy[[level]]$level
  confidence = paste("confidence:",round(hypothesis$confidence,2))
  desciption = gsub("\\\"","DOUBLEQUOTE",parsing_hierarchy[[level]]$desc(hypothesis))
  uuid = makeuuid()
  #intermediate_dim = paste(dim(intermediate),collapse="x")
  return(paste(level_name,confidence,substr(desciption, 1, desc_length),uuid,sep="\n"))
}

print_error = function(node,e){
  print(e)
  print(parsing_hierarchy[[node$parsing_level]]$level)
  print(parsing_hierarchy[[node$parsing_level]]$desc(node$hypothesis))
  print(class(node$intermediate))
  print(dim(node$intermediate))
}

add_child_hypothesis = function(node,parsing_level,hypothesis,confidence){
  node$AddChild(name=generate_node_name(parsing_level,hypothesis),
                parsing_level=parsing_level,
                hypothesis=hypothesis,
                confidence=confidence,
                evaluated=F)
}

add_parsing_step_results = function(node,intermediate_results){
  nodes = list(node)
  
  node_name = node$name
  if(length(intermediate_results)>1){
    node$splitted=T
    node$split_index=1
    node$name=paste0(node_name,"-",1)
  }
  #node$name = substr(node$name,1,min(1000,nchar(node$name)))
  
  for(i in seq_along(intermediate_results)){
    if(i>1){
      node = node$AddSibling(name=paste0(node_name,"-",i),parsing_level=node$parsing_level,hypothesis=node$hypothesis,confidence=node$confidence,splitted=T,split_index=i)
      nodes[[length(nodes)+1]] = node
    }
    
    node$evaluated=T
    node$intermediate=intermediate_results[[i]]$intermediate
    node$warning=intermediate_results[[i]]$warning
    node$metadata=intermediate_results[[i]]$metadata
    node$edits=intermediate_results[[i]]$edits
    node$moves=intermediate_results[[i]]$moves
    node$cells=intermediate_results[[i]]$cells
  }
  
  nodes
}


generate_parsing_tree = function(file, configuration=list(traversal_order = "pre-order", prunning_level=0.1, remove_aggregates=F, remove_named_empty_cols=F, interpolate_spanning_column_header_cells=T, interpolate_spanning_column_data_cells=F, conservative_type_casting=T, separate_multiple_units=T, only_one_table=T)){
  tree = data.tree::Node$new(name=paste("file",file),parsing_level=0,hypothesis=list(),confidence=1,evaluated=T,intermediate=file,splitted=F)
  
  level=1
  
  hypotheses = parsing_hierarchy[[level]]$detector(file,configuration)
  
  for(hypothesis in hypotheses){
    add_child_hypothesis(tree,level,hypothesis,hypothesis$confidence)
  }
  
  while(!all(tree$Get('evaluated', traversal = configuration$traversal_order))){
    data.tree::Do(data.tree::Traverse(tree, traversal = configuration$traversal_order), function(node) {
      if(node$evaluated){return()}
      
      node$evaluated = T      
      if(node$confidence < configuration$prunning_level){return()}
      
      intermediate_results = tryCatch({
          parsing_hierarchy[[node$parsing_level]]$parser(node$parent$intermediate, node$hypothesis, configuration=configuration)
      },error=function(e){
          print(e)
          return(NULL)
      })
     
      if(is.null(intermediate_results)){return()}
       
      updated_nodes = add_parsing_step_results(node,intermediate_results)
      
      for(node in updated_nodes){
        if(node$parsing_level==length(parsing_hierarchy)){next}
        if(is.null(node$intermediate)){next}
        
        level = node$parsing_level+1
        
        intermediate = node$intermediate
        hypotheses = tryCatch({ 
          parsing_hierarchy[[level]]$detector(intermediate,configuration)
        },error=function(e){
          print(e)
          return(NULL)
        })
        
        for(hypothesis in hypotheses){
          if(is.null(hypothesis)){next}
          add_child_hypothesis(node,level,hypothesis,hypothesis$confidence)
        }
      }

      return()
    })
  }
  
  #only collects non-splitted tables #TODO: also evaluate splitted tables
  results = tree$Get(function(node){
      confidence = unlist(node$Get('confidence', traversal = "ancestor"))
      metadata = unlist(node$Get('metadata', traversal = "ancestor"))
      metadata = metadata[!is.na(metadata)]
      warnings = unlist(node$Get('warning', traversal = "ancestor"))
      warnings = warnings[!is.na(warnings)]
      edits = sum(node$Get('edits', traversal = "ancestor"),na.rm=T)
      moves = sum(node$Get('moves', traversal = "ancestor"),na.rm=T)
      cells = unlist(node$Get('cells', traversal = "ancestor"))
      cells = unname(cells[!is.na(cells)])
      list(table=node$intermediate, metadata=metadata, warnings=warnings, edits=edits, moves=moves, cells=cells, confidence=confidence)
  },filterFun = function(node){ 
      node$parsing_level == length(parsing_hierarchy) & !any(node$Get('splitted', traversal = "ancestor"),na.rm=T) & !is.null(node$intermediate)
  },simplify=F)

  if(length(results)==0)  stop("Parsing failed.")
  
  return(list(results=results,tree=tree))
}

# TODO: allow different parser configurations (full etc? here as a param)
parse_file = function(file,pruning_level=0.1,quality_weights=c(warnings=-1,edits=-1,moves=-1,confidence=1,total_cells=1,typed_cells=1,empty_header=-1,empty_cells=-1,non_latin_chars=-1,row_col_ratio=1)) {

  res = tryCatch({ 
    if (length(file) != 1 || !file.exists(file) || file.size(file) > 400000) {
      stop("File '",file,"'' either does not exist or is larger than 400KB.")
    }

    results_and_tree = generate_parsing_tree(file,configuration=list(traversal_order = "pre-order", prunning_level=pruning_level, remove_aggregates=F, remove_named_empty_cols=F, interpolate_spanning_column_header_cells=T, interpolate_spanning_column_data_cells=F, conservative_type_casting=T, separate_multiple_units=T, only_one_table=T))
  
    quality_ranking = rank_quality(results_and_tree$results, weights=-quality_weights)

    list(file=file, results=results_and_tree$results, ranking=quality_ranking)

  },error=function(e){
    list(file=file, error=e)
  })

  class(res) = "hypoparser_result"
  return(res)
}


print.hypoparser_result = function(x, ...) {
  if (is.null(x$error)) {
	 print(paste0("Result of running hypoparsr on file '", x$file, "', generated ", length(x$results)," hypotheses, use as.data.frame() to get best parsing result."), ...)
  } else {
    print(paste0("Failed to run hypoparsr on file '", x$file, "', generated no hypotheses."), ...)
  }
	invisible(x)
}

as.data.frame.hypoparser_result = function(x, row.names, optional, rank=1, ...) {
  if (!is.null(x$error) || length(rank) != 1 || rank < 1 || rank > length(x$ranking)) {
    warning("Trying to get data.frame from failed parsing run or invalid rank.")
    return(data.frame())
  }
	df=x$result[x$ranking][[rank]]$table
	tibble::repair_names(df)
}

