#potential varienties of line ending, quotes, seps and escaping methods (default is on first position)
eols=c("\r\n","(?<!\r)\n","\r(?!\n)")
delims=c(",", ";", "\t", "\\|", "\u060c", "\u3001", "\034", "\035", "\036", "\037")
quotes=c("", "\"", "'", "`", "\u00b4", "\u2018", "\u2019", "\u201c", "\u201d", "\u2032", "\u2033", "\u00b4", "\u02bc", "\u02c8", "\u02bb")
escape_char = "\\\\"

detect = function(text,configuration){
  if(class(text)[1]!="character") return(stop(paste("Can not handle data type:",class(text)[1])))
  
  hypotheses = create_hypothesis_list("dialect")
  
  for(eol in eols){
    lines = unlist(strsplit(text, eol, perl = TRUE))
    if(length(lines)>1){
      for(delim in delims){
        matching_delimiter = grepl(delim,lines)
        if(sum(matching_delimiter)>0){
          for(quote in quotes){
            matching_quote = grepl(quote,text)
            matching_quote_and_delim_1 = grepl(paste0(quote,"\\s*",delim),text)
            matching_quote_and_delim_2 = grepl(paste0(delim,"\\s*",quote),text)
            if(matching_quote && (matching_quote_and_delim_1 || matching_quote_and_delim_2)){
              if(grepl("\\\\",delim)){ delim = gsub("\\\\","","\\|") }
              if(any(grepl(paste0(escape_char,quote),text))){
                hypotheses = add_hypothesis(hypotheses,confidence=1,eol=eol,delim=delim,quote=quote,q_method="escape")
              }else{
                hypotheses = add_hypothesis(hypotheses,confidence=1,eol=eol,delim=delim,quote=quote,q_method="double")
              }
            }
          }
        }
      }
    }
  }
  
  if(length(hypotheses)>9){
    for(i in 1:9){
      hypotheses[[i]]$confidence = 0.1
    }
    for(i in 10:length(hypotheses)){
      hypotheses[[i]]$confidence = 0.1/(length(hypotheses)-9)
    }
  }
  
  hypotheses = normalize_confidence(hypotheses)
  
  return(hypotheses)
}

get_desc = function(hypothesis){
  return(paste("delimiter:",hypothesis$delim,"quote:",hypothesis$quote,"quote method:",hypothesis$q_method))
}

replace_with_empty_strings = function(table){
  colnames(table) = character(ncol(table))
  table[is.na(table)] = ""
  for(i in seq_along(table)){
    table[,i] = enc2native(unlist(table[,i]))
  }
  return(table)
}

parse = function(text, hypothesis, errorHandler, configuration){
  if(hypothesis$q_method=="double"){
    escape_double = T
    escape_backslash = F
  }else if(hypothesis$q_method=="escape"){
    escape_double = F
    escape_backslash = T
  }
  
  #make sure readr reads text as text and not as file path
  text = paste0(text,"\n")
  
  result = tryCatch({
      result = list()
      #hack to circumvent readr auto datatype detection
      dim_check = suppressWarnings(readr::read_delim(text,delim=hypothesis$delim,quote=hypothesis$quote,col_names=F,escape_double=escape_double,escape_backslash=escape_backslash,n_max=10))
      result$intermediate = readr::read_delim(text,delim=hypothesis$delim,quote=hypothesis$quote,col_names=F,escape_double=escape_double,escape_backslash=escape_backslash,col_types=paste(rep("c",ncol(dim_check)),collapse=""))
      result$intermediate = replace_with_empty_strings(result$intermediate)
      result$intermediate = data.frame(result$intermediate)
      result$cells = nrow(result$intermediate) * ncol(result$intermediate)
      result
    },warning=function(w){
      result = list()
      result$intermediate = suppressWarnings(readr::read_delim(text,delim=hypothesis$delim,quote=hypothesis$quote,col_names=F,escape_double=escape_double,escape_backslash=escape_backslash,col_types=paste(rep("c",ncol(dim_check)),collapse="")))
      result$intermediate = replace_with_empty_strings(result$intermediate)
      result$intermediate = data.frame(result$intermediate)
      result$cells = nrow(result$intermediate) * ncol(result$intermediate)
      result$warning = w$message
      result
    },
    error=function(e){
      result = list()
      result$error = e$message
      result
    }
  )

  return(list(result))
}

register_parsing_step(level="dialect",detect,parse,get_desc)
