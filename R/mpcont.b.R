
# This file is a generated template, your changes will not be overwritten

mpcontClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mpcontClass",
    inherit = mpcontBase,
    private = list(
        .run = function() {
          mean.e <- self$options$mean.e
          sd.e <- self$options$sd.e
          n.e <- self$options$n.e
          mean.c <- self$options$mean.c
          sd.c <- self$options$sd.c
          n.c <- self$options$n.c
          id <- self$options$id
          sm <- self$options$sm
          label.e <- self$options$label.e
          label.c <- self$options$label.c
          random <- self$options$random
          common <- self$options$common
          
          data <- self$data
          
          OverallMeta <- meta::metacont(data = data,
                                  mean.e = mean.e, sd.e = sd.e, n.e = as.numeric(n.e),
                                  mean.c = mean.c, sd.c = sd.c, n.c = as.numeric(n.c),
                                  studlab = id, sm = sm,
                                  label.e = label.e, label.c = label.c, random = random, common = common)

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
          
          if (baujat == TRUE | InfluenceCharacteristics == TRUE | ForestEffectSize == TRUE | ForestI2 == TRUE) {
            infResults <- dmetar::InfluenceAnalysis(OverallMeta)
            self$results$infText$setContent(infResults)
            
            if (baujat == TRUE) {
            baujatData <- self$results$baujatPlot
            baujatData$setState(infResults)}
            
            if (InfluenceCharacteristics == TRUE) {
              InfluenceCharacteristicsData <- self$results$infPlot
              InfluenceCharacteristicsData$setState(infResults)}
            
            if (ForestEffectSize == TRUE) {
              ForestEffectSizeData <- self$results$ForestEffectSizePlot
              ForestEffectSizeData$setState(infResults)}
            
            if (ForestI2 == TRUE) {
              ForestI2Data <- self$results$ForestI2Plot
              ForestI2Data$setState(infResults)}
            
          }
        },
        .plot=function(metamodel, ...) {
          
          
          meta::forest(metamodel$state, 
                 leftcols = c("studlab", "mean.e", "sd.e", "n.e", "mean.c", "sd.c", "n.c"),
                 leftlabs = c("Study", "Mean", "SD", "Total", "Mean", "SD", "Total"),
                 col.diamond = "black",col.subgroup ="gray30", digits.sd = 2)
          TRUE
        },
        .LOOPlot=function(LOOData, ...) {
          if (self$options$LOO == TRUE) {
          meta::forest(LOOData$state, rightcols = c("effect","ci","tau2","I2"),
                       col.diamond = "black",col.subgroup ="gray30")
          TRUE
          } else {FALSE}
        },
        .OUTPlot=function(OUTData, ...) {
          if (self$options$OUT == TRUE) {
          dmetar::forest.find.outliers(OUTData$state, col.diamond = "black", col.subgroup ="gray30")
          TRUE
          } else {FALSE}
        },
        .baujatPlot=function(baujatData, ...) {
          if (self$options$baujat == TRUE) {
            dmetar::plot.InfluenceAnalysis(baujatData$state, "baujat")
            TRUE
          } else {FALSE}
        },
        .infPlot=function(InfluenceCharacteristicsData, ...) {
          if (self$options$InfluenceCharacteristics == TRUE) {
            dmetar::plot.InfluenceAnalysis(InfluenceCharacteristicsData$state, "influence")
            TRUE
          } else {FALSE}
        },
        .ForestEffectSizePlot=function(ForestEffectSizeData, ...) {
          if (self$options$ForestEffectSize == TRUE) {
            dmetar::plot.InfluenceAnalysis(ForestEffectSizeData$state, "ES")
            TRUE
          } else {FALSE}
        },
        .ForestI2Plot=function(ForestI2Data, ...) {
          if (self$options$ForestI2 == TRUE) {
            dmetar::plot.InfluenceAnalysis(ForestI2Data$state, "I2")
            TRUE
          } else {FALSE}
        }
        
        )
)
