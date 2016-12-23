# The program recommends to you a movie list based on the responses about movies that you have already seen.
# How it works? It calculates the similarity between your responses and other users in the system, and recommends to you 
# movies from the user with higher similarity (jaccard index similarity: https://en.wikipedia.org/wiki/Jaccard_index).

## How to run it? > Rscript retflix.R
## Use --test command parameter to use for the user data frame a small sample with 8 movies and 3 users.

getRecommendation <- function () {

  # returns vector with the responses in order about the movies selected by the ids from indices
  movieSurvey <- function (indices, movies) {

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

    cat("Please answer these questions to calculate your movie recommendation list...\n")
    survey <- rep(0, length(indices))
    for (i in seq(1, length(indices))) {
      survey[i] <- movieQuestion(movies[indices[i]])
    }
    survey
  }

  # returns the vector with the similarity value between every single user and the survey responses
  getSimilarities <- function (survey, users) {

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

    similarities <- c(numeric(0))
    # for every user, calculate its similarity based on the selected random movies
    for (user_id in row.names(users)) {
      user_ratings <- users[user_id, ]
      similarities <- c(similarities, jaccardSimilarity(survey, user_ratings))
    }
    similarities
  }

  # returns the suggestion list: movies watched from the user with more similarity
  getSuggestions <- function (df, similarities, survey, indices) {
    suggestions <- df[which(similarities == max(similarities)), ]
    if (is.data.frame(suggestions)) {
      # more than one user with same max similarity, we get the first one
      suggestions <- suggestions[1, ]
    }
    suggestions <- as.logical(suggestions)

    # remove suggestions of movies you already watched it (from previous questions)
    for (i in seq(1, length(indices))) {
      ind <- indices[i]
      suggestions[ind] <- suggestions[ind] && !as.logical(survey[i])
    }
    suggestions
  }

  totalQuestions <- 4
  indices <- sample(1:length(movies), totalQuestions) # pick 4 random movies by its indexes
  survey <- movieSurvey(indices, movies)
  similarities <- getSimilarities(survey, df[, indices])
  suggestions <- getSuggestions(df, similarities, survey, indices)

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
  movies <- c("Saving Private Ryan", "Underworld", "Jurassic World",  "Platoon",  "Pulp Fiction", "Jumanji", "Toy Story", "Star Wars")
  df <- data.frame(matrix(NA, nrow=3, ncol=length(movies)))
  colnames(df) <- movies
  rownames(df) <- c("David", "Zach", "Jose")
  df["David", ] <- c(1, 0, 0, 1, 1, 0, 0, 0)
  df["Zach", ] <- c(0, 1, 1, 0, 0, 0, 0, 1)
  df["Jose", ] <- c(0, 0, 0, 0, 1, 1, 0, 0)
}

if (length(args) == 0) {
  # By default, read dataset from file
  df <- read.csv("movie_ratings.csv", header=T, row.names=1, check.names=F)
  movies <- as.array(colnames(df))
}

cat("\n\nWelcome to (R)etFlix, a movie recommendation system developed in R lang\n")
cat("***************************************************************************\n")
printMenu()
