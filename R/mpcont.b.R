mpcontClass <- if (requireNamespace('jmvcore', quietly = TRUE)) {
  R6::R6Class(
    "mpcontClass",
    inherit = mpcontBase,
    private = list(
      .run = function() {
        # 1. Check if analysis is possible (all required variables assigned)
        # Check availability of names
        if (
          is.null(self$options$meanE) ||
            is.null(self$options$sdE) ||
            is.null(self$options$nE) ||
            is.null(self$options$meanC) ||
            is.null(self$options$sdC) ||
            is.null(self$options$nC)
        ) {
          return()
        }

        # 2. Extract and Validate Data
        # converting to numeric to strip attributes (jmvcore::toNumeric)
        # I think this is not needed may be it is defensive programming?
        # TODO check more modules about this behaviuor 
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

        # End of my part I would be really glad if you can make the below part
        # as new methods It is too disorganized to me and this is not the
        # default way in R6
        metamodel <- self$results$plot
        metamodel$setState(OverallMeta)
        self$results$text$setContent(OverallMeta)

        # sensitivity analysis
        LOO <- self$options$LOO
        OUT <- self$options$OUT
        baujat <- self$options$baujat
        InfluenceCharacteristics <- self$options$InfluenceCharacteristics
        ForestEffectSize <- self$options$ForestEffectSize
        ForestI2 <- self$options$ForestI2

        if (LOO == TRUE) {
          LOOResults <- meta::metainf(OverallMeta)
          self$results$LOOText$setContent(LOOResults)
          LOOData <- self$results$LOOPlot
          LOOData$setState(LOOResults)
        }

        if (OUT == TRUE) {
          OUTResults <- dmetar::find.outliers(OverallMeta)
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
          infResults <- dmetar::InfluenceAnalysis(OverallMeta)
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
      .plot = function(metamodel, ...) {
        meta::forest(
          metamodel$state,
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
