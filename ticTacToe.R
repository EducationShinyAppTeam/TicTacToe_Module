#' tttUI
#'
#' The UI component for the Tic-Tac-Toe shiny module
#'
#' @param namespaceID Required--the unique namespace for each instance
#' @return UI components for an invisible retriggering of MathJax
#' @examples
#' tttMathUI(namespaceID = "gamePage")
#'
#' @export
tttUI <- function(namespaceID) {
  tagList(
    uiOutput(
      outputId = NS(namespace = namespaceID, id = "player"),
      container = tags$h3
    ),
    fluidRow(
      div(
        class = "col-sm-12 col-md-4",
        h3("Game Board"),
        br(),
        uiOutput(
          outputId = NS(namespace = namespaceID, id = "gameBoard"),
          class = "game-board"
        )
      ),
      div(
        class = "col-sm-12 col-md-8",
        h3("Question"),
        withMathJax(
          uiOutput(outputId = NS(namespace = namespaceID, id = "question")),
          uiOutput(outputId = NS(namespace = namespaceID, id = "extraOutput")),
          h3("Answer"),
          uiOutput(outputId = NS(namespace = namespaceID, id = "answer")),
          uiOutput(outputId = NS(namespace = namespaceID, id = "mark")),
          uiOutput(outputId = NS(namespace = namespaceID, id = "feedback"))
        ),
        bsButton(
          inputId = NS(namespace = namespaceID, id = "submit"),
          label = "Submit",
          size = "large",
          style = "default",
          disabled = FALSE
        ),
        bsButton(
          inputId = NS(namespace = namespaceID, id = "hint"),
          label = "Hint",
          icon = icon("question"),
          size = "large",
          disabled = FALSE
        ),
        bsButton(
          inputId = NS(namespace = namespaceID, id = "reset"),
          label = "Reset Game",
          color = "primary",
          size = "large",
          style = "default"
        ),
        br(),
        br(),
        uiOutput(outputId = NS(namespace = namespaceID, id = "hintDisplay")),
        #These two triggers help with MathJax re-rendering
        uiOutput(outputId = NS(namespace = namespaceID, id = "trigger1")),
        uiOutput(outputId = NS(namespace = namespaceID, id = "trigger2"))
      )
    )
  )
}


#' tttServer
#'
#' The Server component the Tic-Tac-Toe shiny module
#'
#' @param namespaceID Required--the unique namespace for each instance
#' @param gridSize Required--the number of rows (and columns) for the grid; 3, 4, 5
#' @param questionBank Required--the data frame containing questions; see rules
#' @param parent Required--link to the app's parent session environment
#' @return UI and server components for playing Tic-Tac-Toe
#' @examples
#' tttServer(
#' namespaceID = "retrigger1",
#' gridSize = 3,
#' questionBank = questionBank,
#' parent = session
#' )
#'
#' @export
tttServer <- function(namespaceID, gridSize, questionBank, parent) {
  moduleServer(id = namespaceID, function(input, output, session) {
    ## Error check
    if (gridSize > 5 || gridSize < 3) {
      stop("Current allowed sizes are 3, 4, or 5.")
    } else if (trunc(sqrt(nrow(questionBank))) >= gridSize) {
      GRID_SIZE <- gridSize
    } else if (trunc(sqrt(nrow(questionBank))) >= 3) {
      print("Not enough questions requested game; reducing to largest possible.")
      GRID_SIZE <- trunc(sqrt(nrow(questionBank)))
    } else {
      stop("Question bank does not have enough questions for a 3 x 3 game. Fix.")
    }

    ## Constants ----
    TILE_COUNT <- GRID_SIZE ^ 2

    ## Game Variables and Reactive Values ----
    activeBtn <- reactiveVal(NULL)
    activeQuestion <- reactiveVal(NULL)
    player <- reactiveVal(NULL)
    opponent <- reactiveVal(NULL)
    scoreMatrix <- reactiveVal(
      matrix(
        data = rep.int(0, times = TILE_COUNT),
        nrow = GRID_SIZE,
        ncol = GRID_SIZE
      )
    )
    gameProgress <- reactiveVal(FALSE)

    qSelected <- sample(
      x = seq_len(nrow(questionBank)),
      size = TILE_COUNT,
      replace = FALSE
    )
    gameSet <- questionBank[qSelected,]

    ## Tic-Tac-Toe Helper Functions ----
    ### Get the coordinates of the button in the game board
    tileCoordinates <- function(tile = NULL, index = NULL) {
      row <- -1
      col <- -1
      # if: button tile is given, derive from id
      # else: derive from index
      if (!is.null(tile)) {
        # grid-[row]-[col]
        tile <- strsplit(tile, "-")[[1]]
        tile <- tile[-1] # remove oxo
        row <- strtoi(tile[1])
        col <- strtoi(tile[2])
      }
      else {
        row <- (index - 1) %/% GRID_SIZE + 1
        col <- index - (GRID_SIZE * (row - 1))
      }
      coordinates <- list("row" = row, "col" = col)
      return(coordinates)
    }

    ### Convert game button coordinates into an index for questions
    tileIndex <- function(tile) {
      coords <- tileCoordinates(tile)
      index = GRID_SIZE * (coords$row - 1) + coords$col
      return(index)
    }

    ### Score Keeping
    score <- function(score, tile, value) {
      i <- tileCoordinates(tile)
      score[i$row, i$col] <- value
      return(score)
    }

    ### Check the game's status
    gameCheck <- function(mat) {
      rows <- rowSums(mat)
      cols <- colSums(mat)
      if (GRID_SIZE > 1) {
        mainD <- sum(diag(mat))
        rotated <- apply(t(mat), 2, rev)
        offD <- sum(diag(rotated))

        if (GRID_SIZE %in% rows || GRID_SIZE %in% cols ||
            mainD == GRID_SIZE || offD == GRID_SIZE) {
          return("win")
        }
        else if (-GRID_SIZE %in% rows ||
                 -GRID_SIZE %in% cols == 1 ||
                 mainD == -GRID_SIZE || offD == -GRID_SIZE) {
          return("lose")
        }
        else if (any(mat == 0)) {
          return("continue")
        }
        else {
          return("draw")
        }
      } else {
        ifelse(rows == 1 && rows != 0,
               return("win"),
               return("lose"))
      }
    }

    ## Shiny Required Helper Functions ----
    ### Reset button
    btnReset <- function(index) {
      coords <- tileCoordinates(index = index)
      id <- paste0("grid-", coords$row, "-", coords$col)
      updateButton(
        session = session,
        inputId = id,
        label = "?",
        disabled = FALSE
      )
    }

    ### Set up the game board buttons
    boardBtn <- function(tile) {
      index <- tileIndex(tile)
      activeQuestion(gameSet[index, "id"])
      output$question <- renderUI({
        withMathJax()
        return(gameSet[index, "question"])
      })
      output$answer <- ansFunc(index, gameSet)
      if (gameSet[index, "extraOutput"] != "") {
        output$extraOutput <- renderText({
          gameSet[index, "extraOutput"]
        })
      }
      else {
        output$extraOutput <- NULL
      }
      #Retrigger MathJax processing
      output$trigger1 <- renderUI({withMathJax()})
      output$trigger2 <- renderUI({withMathJax()})
      #Enable Submit Button
      updateButton(
        session = session,
        inputId = NS(namespace = namespaceID, id = "submit"),
        disabled = FALSE
      )
      #Enable hint button
      updateButton(
        session = session,
        inputId = NS(namespace = namespaceID, id = "hint"),
        disabled = FALSE
      )
      output$hintDisplay <- NULL
    }

    ### Setup the different answer types
    ansFunc <- function(index, df) {
      if (df[index, "format"] == "numeric") {
        renderUI({
          numericInput(
            inputId = "ans",
            label = df[index, "label"],
            value = 0)
        })
      }
      else if (df[index, "format"] == "two") {
        renderUI({
          radioGroupButtons(
            inputId = "ans",
            choices = list(
              df[index, "A"],
              df[index, "B"]
            ),
            selected = character(0),
            checkIcon = list(
              yes = icon("check-square"),
              no = icon("square-o")
            ),
            status = "textGame",
            direction = "horizontal",
            individual = TRUE
          )
        })
      }
      else if (df[index, "format"] == "three") {
        renderUI({
          radioGroupButtons(
            inputId = "ans",
            choices = list(
              df[index, "A"],
              df[index, "B"],
              df[index, "C"]
            ),
            selected = character(0),
            checkIcon = list(
              yes = icon("check-square"),
              no = icon("square-o")
            ),
            status = "textGame",
            direction = "vertical"
          )
        })
      }
      else {
        renderUI({
          radioGroupButtons(
            inputId = "ans",
            choices = list(
              df[index, "A"],
              df[index, "B"],
              df[index, "C"],
              df[index, "D"]
            ),
            selected = character(0),
            checkIcon = list(
              yes = icon("check-square"),
              no = icon("square-o")
            ),
            status = "textGame",
            direction = "vertical"
          )
        })
      }
    }

    ### Reset the game
    gameReset <- function() {
      lapply(1:TILE_COUNT, btnReset)
      qSelected <<- sample(
        x = seq_len(nrow(questionBank)),
        size = TILE_COUNT,
        replace = FALSE)
      gameSet <<- questionBank[qSelected,]
      output$question <- renderUI({
        return("Click a button on the game board to get started on your
                   new game.")
      })
      output$answer <- renderUI({""})
      output$extraOutput <- renderUI({""})
      scoreMatrix(matrix(
        data = rep.int(0, times = TILE_COUNT),
        nrow = GRID_SIZE,
        ncol = GRID_SIZE
      ))

      gameProgress(FALSE)
      activeBtn(NULL)
      updateButton(
        session = session,
        inputId = NS(namespace = namespaceID, id = "submit"),
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = NS(namespace = namespaceID, id = "hint"),
        disabled = TRUE
      )
    }

    ## Reset Button ----
    observeEvent(
      eventExpr = input$reset,
      handlerExpr = {
        gameReset()
        output$mark <- renderIcon()
        output$feedback <- renderUI(NULL)
        output$hintDisplay <- renderUI(NULL)
      })

    ## Render Game Board / Attach Observers ----
    output$gameBoard <- renderUI({
      board <- list()
      index <- 1
      sapply(1:GRID_SIZE, function(row) {
        sapply(1:GRID_SIZE, function(column) {
          id <- paste0("grid-", row, "-", column)
          board[[index]] <<- tags$li(
            actionButton(
              inputId = paste0("grid-", row, "-", column),
              label = "?",
              color = "primary",
              style = "bordered",
              class = "grid-fill"
            ),
            class = "grid-tile"
          )
          observeEvent(
            eventExpr = parent$input[[id]],
            handlerExpr = {
              activeBtn(id)
              boardBtn(id)
              output$mark <- renderUI(NULL)
              output$feedback <- renderUI(NULL)
              output$testing <- renderUI({paste("Button Trigger:", id)})
            })
          index <<- index + 1
        })
      })

      tags$ol(board, class = paste(
        "grid-board",
        "grid-fill",
        paste0("grid-", GRID_SIZE, "x", GRID_SIZE)
      ))
    })

    ## Submit Button ----
    observeEvent(
      eventExpr = input$submit,
      handlerExpr = {
        index <- tileIndex(activeBtn())
        answer <- ""
        if (gameSet[index, "format"] == "numeric") {
          answer <- gameSet[index, "answer"]
        }
        else {
          answer <- gameSet[index, gameSet[index, "answer"]]
        }
        success <- parent$input$ans == answer
        if (is.null(success) || length(success) == 0) {
          sendSweetAlert(
            session = session,
            title = "Error",
            text = "Please select an answer before pressing Submit.",
            type = "error"
          )
        }
        else if (success) {
          updateButton(
            session = session,
            inputId = activeBtn(),
            label = player(),
            disabled = TRUE
          )
          output$mark <- renderIcon(
            icon = "correct",
            width = 50
          )
          output$feedback <- renderUI(
            paste("Your answer is correct!")
          )
          scoreMatrix(score(scoreMatrix(), activeBtn(), 1))
        }
        else {
          updateButton(
            session = session,
            inputId = activeBtn(),
            label = opponent(),
            disabled = TRUE
          )
          scoreMatrix(score(scoreMatrix(), activeBtn(), -1))
          output$mark <- renderIcon(
            icon = "incorrect",
            width = 50
          )
          output$feedback <- renderUI(
            paste("Your answer is incorrect. The correct answer is:",
                  answer,
                  ".")
          )
          output$hintDisplay <- NULL
        }
        ### Check for game over states ----
        gameState <- gameCheck(scoreMatrix())
        completion <- ifelse(gameState == "continue", FALSE, TRUE)
        interactionType <- ifelse(gameSet[index,]$format == "numeric", "numeric", "choice")

        if (gameState == "win") {
          confirmSweetAlert(
            session = session,
            inputId = "endGame",
            title = "You Win!",
            text = "You've filled either a row, a column, or a main diagonal.
                         Start over and play a new game.",
            btn_labels = "Start Over",
            type = "success"
          )
          output$mark <- renderUI(NULL)
          output$feedback <- renderUI(NULL)
          output$hintDisplay <- renderUI(NULL)
        }
        else if (gameState == "lose") {
          confirmSweetAlert(
            session = session,
            inputId = "endGame",
            title = "You lose :(",
            text = "Take a moment to review the concepts and then try again.",
            btn_labels = "Start Over",
            type = "error"
          )
          output$mark <- renderIcon()
          output$feedback <- renderUI(NULL)
          output$hintDisplay <- renderUI(NULL)
        }
        else if (gameState == "draw") {
          confirmSweetAlert(
            session = session,
            inputId = "endGame",
            title = "Draw!",
            text = "Take a moment to review the concepts and then try again.",
            btn_labels = "Start Over"
          )
          output$mark <- renderUI(NULL)
          output$feedback <- renderUI(NULL)
          output$hintDisplay <- renderUI(NULL)
        }
        if (is.null(success) || length(success) == 0) {
          updateButton(
            session = session,
            inputId = NS(namespace = namespaceID, id = "submit"),
            disabled = FALSE
          )
          updateButton(
            session = session,
            inputId = NS(namespace = namespaceID, id = "hint"),
            disabled = FALSE
          )
        }
        else{
          updateButton(
            session = session,
            inputId = NS(namespace = namespaceID, id = "submit"),
            disabled = TRUE
          )
          updateButton(
            session = session,
            inputId = NS(namespace = namespaceID, id = "hint"),
            disabled = TRUE)
        }
      })

    ## Game Start Check ----
    observeEvent(
      eventExpr = parent$input$pages,
      handlerExpr = {
        if (parent$input$pages == "game") {
          if (!gameProgress()) {
            shinyalert(
              title = "Player Select",
              text = "Select whether you want to play as O or X.",
              showConfirmButton = TRUE,
              confirmButtonText = "Play as X",
              showCancelButton = TRUE,
              cancelButtonText = "Play as O"
            )
            gameProgress(TRUE)
          }
        }
      },
      ignoreInit = TRUE
    )

    ## End Game ----
    observeEvent(
      eventExpr = input$endGame,
      handlerExpr = {
        gameReset()
      })

    ## Select Player Symbol ----
    observeEvent(
      eventExpr = input$shinyalert,
      handlerExpr = {
        if (input$shinyalert == TRUE) {
          player("X")
          opponent("O")
        }
        if (input$shinyalert == FALSE) {
          player("O")
          opponent("X")
        }
        output$player <- renderUI({
          return(paste0("You are playing as ", player(), "."))
        })
      })

    ## Display hint ----
    observeEvent(
      eventExpr = input$hint,
      handlerExpr = {
        index <- tileIndex(activeBtn())
        hint <- ""
        if (gameSet[index, "format"] == "numeric") {
          hint <- gameSet[index, "hint"]
        }
        else {
          hint <- gameSet[index, gameSet[index, "hint"]]
        }

        if (gameSet[index, "hint"] != "") {
          output$hintDisplay <- renderUI({
            ns <- session$ns
            paste("Hint:", hint)
          })
        } else {
          output$hintDisplay <- NULL
        }
      }
    )
  }
  )
}