
getRecommendation <- function () {
  
  movieQuestion <- function (movie) {
    result <- 0
    switch(readkey(paste("\nHave you ever seen '", movie, "'?\nType 'y' - yes\nType 'n' - no\n\n> ", collapse="")),
      y={
        result <- 1
      },
      n={},
      {
        cat("\nOption not identified, please try again.")
        movieQuestion(movie)
      }
    )
    result
  }
  
  jaccardSimilarity <- function (user1, user2) {
    sums <- user1 + user2
    similarity <- length(sums[sums==2])
    total <- length(sums[sums==1]) + similarity
    result <- similarity/total
    if (is.nan(result)) {
      result <- 0
    }
    result
  }
  
  totalQuestions <- 4
  indices <- sample(1:length(movies), totalQuestions)
  me <- rep(0, totalQuestions)
  cat("Please answer these questions to calculate your movie recommendation list...\n")
  for (i in seq(1, totalQuestions)) {
    me[i] <- movieQuestion(movies[indices[i]])
  }

  similarities <- c(numeric(0))
  for (user_id in seq(1, length(df))) {
    user_ratings <- df[user_id][indices, 1]
    similarities <- c(similarities, jaccardSimilarity(me, user_ratings))
  }
  # result <- data.frame(similarities)
  suggestions <- df[, which(similarities == max(similarities))]
  if (is.data.frame(suggestions)) {
    # more than one user with same max similarity, we get the first one
    suggestions <- as.array(suggestions[, 1])
  }
  suggestions <- as.logical(suggestions)
  # remove suggestions of movies you already watched it
  for (i in seq(1, totalQuestions)) {
    ind <- indices[i]
    suggestions[ind] <- suggestions[ind] && !as.logical(me[i])
  }
  
  cat("\n********************************************************\n")
  cat("System strongly recommend to you the following title(s):\n")
  cat(paste(movies[suggestions], collapse=", "))
  cat("\n\nWant to try again?\n")
  printMenu()
}

readkey <- function (prompt_str) {
  if (interactive()) {
    option <- invisible(readline(prompt = prompt_str))
  }
  else {
    cat(prompt_str)
    option <- invisible(readLines(file("stdin"), 1))
  }
  option
}

printMenu <- function () {
  switch(readkey("Type:\n'q' - to exit\n'r' - to get your recommendation list based on few questions\n\n> "),
    q={
      cat("Thank you for using (R)etFlix and see you soon.\n")
    },
    r={
      getRecommendation()
    },
    {
      cat("Option not identified, please try again.\n")
      printMenu()
    }
  )
}

# Create a fake dataset with movies
movies <- c("Saving Private Ryan", "Stranger Things", "Jurassic World",  "Platoon",  "Pulp Fiction", "Jumanji", "Toy Story", "Star Wars")
David <- c(1, 0, 0, 1, 1, 0, 0, 0)
Zach <- c(0, 1, 1, 0, 0, 0, 0, 1)
Jose <- c(0, 0, 0, 0, 1, 1, 0, 0)
df <- data.frame(David, Zach, Jose)
row.names(df) <- movies

cat("\n\nWelcome to (R)etFlix, a movie & series recommendation system developed in R lang\n")
cat("********************************************************************************\n")
printMenu()
