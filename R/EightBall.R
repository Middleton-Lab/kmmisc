#' Ask the magic eight ball a question, and it will reply.
#' 
#' @title  Ask the Magic Eight Ball
#' 
#' @param question A string that you would like the eight ball to
#' evaluate.
#' 
#' @author Kevin Middleton (\email{middletonk@@missouri.edu})
#'
#' @export
#'
#' @examples
#' EightBall("Should I learn S4 methods?")
#' 
EightBall <- function(question){
  if (grepl("\\?", question) != TRUE)
    stop("You must ask the magic eight ball a question.")
  answers <- c("As I see it, yes",
               "It is certain",
               "It is decidedly so",
               "Most likely",
               "Outlook good",
               "Signs point to yes",
               "Without a doubt",
               "Yes",
               "Yes - definitely",
               "You may rely on it",
               "Reply hazy, try again",
               "Ask again later",
               "Better not tell you now",
               "Cannot predict now",
               "Concentrate and ask again",
               "Don't count on it",
               "My reply is no",
               "My sources say no",
               "Outlook not so good",
               "Very doubtful")
  return(cat("\n\n", sample(answers, 1), "\n\n"))
}
