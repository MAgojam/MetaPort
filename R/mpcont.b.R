# TODO
# 1- check if we need this requireNamespace call, I think no
# 2- Edit internal names and some options names to follow camelCase convention
# 3- Search for solution to the problem of size of saving and not updating
# dynamically with plot size, I doubt it is error from jamovi side
# 4- Ask for avialability of adding dpi to image export
# 5- We would need someone to check what other programs provide for meta
# analysis to give them priority and imitate their GUI
# 6- Check if we still need default widht and height in .r.yaml file but after
# fixing save size problem
# 7- How devices and warning work in jamovi ?

mpcontClass <- if (requireNamespace('jmvcore', quietly = TRUE)) {
  R6::R6Class(
    "mpcontClass",
    inherit = mpcontBase,
    active = list(
      model = function() {
        if (is.null(private$.model)) {
          private$.model <- private$.computeModel()
        }
        private$.model
      }
    ),
    private = list(
      .model = NULL,
      .computeModel = function() {
        # 1. Check availability of names
        if (
          is.null(self$options$meanE) ||
            is.null(self$options$sdE) ||
            is.null(self$options$nE) ||
            is.null(self$options$meanC) ||
            is.null(self$options$sdC) ||
            is.null(self$options$nC)
        ) {
          return(NULL)
        }

        # 2. Extract and Validate Data
        # converting to numeric to strip attributes (jmvcore::toNumeric)
        mean.e <- jmvcore::toNumeric(self$data[[self$options$meanE]])
        sd.e <- jmvcore::toNumeric(self$data[[self$options$sdE]])
        n.e <- jmvcore::toNumeric(self$data[[self$options$nE]])
        mean.c <- jmvcore::toNumeric(self$data[[self$options$meanC]])
        sd.c <- jmvcore::toNumeric(self$data[[self$options$sdC]])
        n.c <- jmvcore::toNumeric(self$data[[self$options$nC]])

        # 3. Optional study labels
        studlab <- NULL
        if (!is.null(self$options$studyLabel)) {
          studlab <- self$data[[self$options$studyLabel]]
        }

        # 4. Options
        label.e <- self$options$groupLabelE
        label.c <- self$options$groupLabelC
        sm <- self$options$sm
        method.tau <- self$options$methodTau
        method.smd <- self$options$methodSmd
        random <- self$options$random
        common <- self$options$common
        prediction <- self$options$prediction
        level <- self$options$confidenceLevel

        # Pass data directly
        OverallMeta <- meta::metacont(
          n.e = n.e,
          mean.e = mean.e,
          sd.e = sd.e,
          n.c = n.c,
          mean.c = mean.c,
          sd.c = sd.c,
          studlab = studlab,
          sm = sm,
          method.tau = method.tau,
          method.smd = method.smd,
          common = common,
          random = random,
          prediction = prediction,
          level.ma = level,
          label.e = label.e,
          label.c = label.c
        )

        OverallMeta
      },

      .run = function() {
        if (is.null(self$model)) {
          return(NULL)
        }

        self$results$text$setContent(summary(self$model))
        private$.prepareForestPlot()

        # End of my part, it would be good if we could clear .run() function
        # and use separate methods/functions rather than listing all of them
        # below

        # sensitivity analysis
        LOO <- self$options$LOO
        OUT <- self$options$OUT
        baujat <- self$options$baujat
        InfluenceCharacteristics <- self$options$InfluenceCharacteristics
        ForestEffectSize <- self$options$ForestEffectSize
        ForestI2 <- self$options$ForestI2

        if (LOO == TRUE) {
          LOOResults <- meta::metainf(self$model)
          self$results$LOOText$setContent(LOOResults)
          LOOData <- self$results$LOOPlot
          LOOData$setState(LOOResults)
        }

        if (OUT == TRUE) {
          OUTResults <- dmetar::find.outliers(self$model)
          self$results$OUTText$setContent(OUTResults)
          OUTData <- self$results$OUTPlot
          OUTData$setState(OUTResults)
        }

        if (
          baujat == TRUE |
            InfluenceCharacteristics == TRUE |
            ForestEffectSize == TRUE |
            ForestI2 == TRUE
        ) {
          infResults <- dmetar::InfluenceAnalysis(self$model)
          self$results$infText$setContent(infResults)

          if (baujat == TRUE) {
            baujatData <- self$results$baujatPlot
            baujatData$setState(infResults)
          }

          if (InfluenceCharacteristics == TRUE) {
            InfluenceCharacteristicsData <- self$results$infPlot
            InfluenceCharacteristicsData$setState(infResults)
          }

          if (ForestEffectSize == TRUE) {
            ForestEffectSizeData <- self$results$ForestEffectSizePlot
            ForestEffectSizeData$setState(infResults)
          }

          if (ForestI2 == TRUE) {
            ForestI2Data <- self$results$ForestI2Plot
            ForestI2Data$setState(infResults)
          }
        }
      },
      .prepareForestPlot = function() {
        if (is.null(self$model)) {
          return(NULL)
        }

        # Dynamic height calculation (Dry Run)
        old_dev <- grDevices::dev.cur()
        grDevices::pdf(file = NULL)

        calculated_height <- tryCatch(
          {
            res <- suppressMessages(suppressWarnings(meta::forest(self$model)))
            res$figheight$total_height * 72
          },
          finally = {
            grDevices::dev.off()
            if (old_dev > 1) grDevices::dev.set(old_dev)
          }
        )

        self$results$plot$setSize(width = 800, height = calculated_height)

        self$results$plot$setState(self$model)
      },
      .forestPlot = function(image, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }

        grid::grid.newpage()
        grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))

        meta::forest(
          image$state,
          new = FALSE,
          leftcols = c(
            "studlab",
            "mean.e",
            "sd.e",
            "n.e",
            "mean.c",
            "sd.c",
            "n.c"
          ),
          leftlabs = c("Study", "Mean", "SD", "Total", "Mean", "SD", "Total"),
          col.diamond = "black",
          col.subgroup = "gray30",
          digits.sd = 2
        )
        TRUE
      },
      .LOOPlot = function(LOOData, ...) {
        if (self$options$LOO == TRUE) {
          meta::forest(
            LOOData$state,
            rightcols = c("effect", "ci", "tau2", "I2"),
            col.diamond = "black",
            col.subgroup = "gray30"
          )
          TRUE
        } else {
          FALSE
        }
      },
      .OUTPlot = function(OUTData, ...) {
        if (self$options$OUT == TRUE) {
          dmetar::forest.find.outliers(
            OUTData$state,
            col.diamond = "black",
            col.subgroup = "gray30"
          )
          TRUE
        } else {
          FALSE
        }
      },
      .baujatPlot = function(baujatData, ...) {
        if (self$options$baujat == TRUE) {
          dmetar::plot.InfluenceAnalysis(baujatData$state, "baujat")
          TRUE
        } else {
          FALSE
        }
      },
      .infPlot = function(InfluenceCharacteristicsData, ...) {
        if (self$options$InfluenceCharacteristics == TRUE) {
          dmetar::plot.InfluenceAnalysis(
            InfluenceCharacteristicsData$state,
            "influence"
          )
          TRUE
        } else {
          FALSE
        }
      },
      .ForestEffectSizePlot = function(ForestEffectSizeData, ...) {
        if (self$options$ForestEffectSize == TRUE) {
          dmetar::plot.InfluenceAnalysis(ForestEffectSizeData$state, "ES")
          TRUE
        } else {
          FALSE
        }
      },
      .ForestI2Plot = function(ForestI2Data, ...) {
        if (self$options$ForestI2 == TRUE) {
          dmetar::plot.InfluenceAnalysis(ForestI2Data$state, "I2")
          TRUE
        } else {
          FALSE
        }
      }
    )
  )
}