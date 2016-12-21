# The program recommends to you a movie list based on the responses about movies that you have already seen.
# How it works? It calculates the similarity between your responses and other users in the system, and recommends to you 
# movies from the user with higher similarity (jaccard index similarity: https://en.wikipedia.org/wiki/Jaccard_index).

## How to run it? > Rscript retflix.R
## Use --test command parameter to use for the user data frame a small sample with 8 movies and 3 users.

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

  cat("Please answer these questions to calculate your movie recommendation list...\n")
  totalQuestions <- 4
  indices <- sample(1:length(movies), totalQuestions) # pick 4 random movies by its indexes
  me <- rep(0, totalQuestions)
  for (i in seq(1, totalQuestions)) {
    me[i] <- movieQuestion(movies[indices[i]])
  }

  similarities <- c(numeric(0))
  # for every user, calculate its jaccard similarity based on the 4 random movies
  for (user_id in row.names(df)) {
    user_ratings <- df[user_id, indices]
    similarities <- c(similarities, jaccardSimilarity(me, user_ratings))
  }
  # system chooses as suggestions, the movies watched from the user with more similarity
  suggestions <- df[which(similarities == max(similarities)), ]
  if (is.data.frame(suggestions)) {
    # more than one user with same max similarity, we get the first one
    suggestions <- suggestions[1, ]
  }
  suggestions <- as.logical(suggestions)

  # remove suggestions of movies you already watched it (from previous questions)
  for (i in seq(1, totalQuestions)) {
    ind <- indices[i]
    suggestions[ind] <- suggestions[ind] && !as.logical(me[i])
  }

  cat("\n********************************************************\n")
  cat("System strongly recommend to you the following title(s):")
  cat("\n********************************************************\n")
  cat(paste(movies[suggestions], collapse="\n"))
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

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1 & identical(args[1], "--test")) {
  # Create a fake dataset with some data
  movies <- c("Saving Private Ryan", "Underworld", "Jurassic World",  "Platoon",  "Pulp Fiction",
              "Jumanji", "Toy Story", "Star Wars")
  df <- data.frame(matrix(NA, nrow=3, ncol=length(movies)))
  colnames(df) <- movies
  rownames(df) <- c("David", "Zach", "Jose")
  df["David", ] <- c(1, 0, 0, 1, 1, 0, 0, 0)
  df["Zach", ] <- c(0, 1, 1, 0, 0, 0, 0, 1)
  df["Jose", ] <- c(0, 0, 0, 0, 1, 1, 0, 0)
} else {
  # By default, read dataset from file
  df <- read.csv("movie_ratings.csv", header=T, row.names=1, check.names=F)
  movies <- as.array(colnames(df))
}

cat("\n\nWelcome to (R)etFlix, a movie recommendation system developed in R lang\n")
cat("***************************************************************************\n")
printMenu()
