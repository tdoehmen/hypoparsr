detect = function(file,configuration){
  hypotheses = create_hypothesis_list("encoding")
  
  guess = data.frame(readr::guess_encoding(file))

  for(i in seq_along(guess$encoding)){
    hypotheses = add_hypothesis(hypotheses,confidence=guess$confidence[i],encoding=as.character(guess$encoding[i]))
  }

  #by default, add UTF8 encoding
  if(!any(sapply(guess$encoding,function(x){x=="UTF-8"}))){
    hypotheses = add_hypothesis(hypotheses,confidence=0.2,encoding="UTF-8")
  }
  
  hypotheses = normalize_confidence(hypotheses)

  return(hypotheses)
}

get_desc = function(hypothesis){
  return(hypothesis$encoding)
}

parse = function(file, hypothesis, errorHandler, configuration){
  result = tryCatch({
      result = list()
      result$intermediate = readr::read_file(file, locale=readr::locale(encoding = hypothesis$encoding))
      result$intermediate = iconv(result$intermediate)
      result
    },
    warning=function(w){
      result = list()
      result$intermediate = suppressWarnings(readr::read_file(file, locale=readr::locale(encoding = hypothesis$encoding)))
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

register_parsing_step(level="encoding",detect,parse,get_desc)
