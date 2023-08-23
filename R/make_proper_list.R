#' A function to Make Proper Lists
#' @param x the vector of items
#' @param conjunction the conjunction to use with default \code{"and"}
#' @examples
#' make_proper_list("Apples")
#' make_proper_list(c("Apples", "Bananas"))
#' make_proper_list(c("Apples", "Bananas", "Pears"))
#' @export

make_proper_list <- function(x, conjunction = "and"){
  if(!is.vector(x)){
    stop("Please supply a vector")
  }

  if(is.numeric(x)){
    warning("Coercing to string")
    x <- suppressWarnings(as.character(x))
  }

  count_items <- length(x)

  if(count_items==1){
    return(x)
  } else if(count_items == 2){
    use_conjunction <- paste0(" ", conjunction," ")
    out <- paste(x[1:2], collapse = use_conjunction)
    return(out)
  } else{
    use_conjunction <- paste0(", ", conjunction," ")
    out <- paste(paste(x[1:(length(x)-1)], collapse = ", "),use_conjunction,
                 x[length(x)], sep = "")
    return(out)
  }

}

