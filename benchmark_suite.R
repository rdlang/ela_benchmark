library(smoof)
source("functions/smoof_functions.R")

benchmark_suite = function(dimensions, fid) {
  return(switch(fid,
         makeSchwefel1Function(dimensions),
         makeRipple25Function(dimensions),
         makeExponentialFunction(dimensions),
         makeNeedleEyeFunction(dimensions),
         makeGeneralizedDropWaveFunction(dimensions),
         makeStepN3Function(dimensions),
         makeGeneralizedGiuntaFunction(dimensions),
         makeGeneralizedPavianiFunction(dimensions),
         makeBonyadiMichalewiczFunction(dimensions),
         makeBrownFunction(dimensions),
         makeCosineMixtureFunction(dimensions),
         makeMishra07Function(dimensions),
         makeMishra01Function(dimensions),
         makeDiscusFunction(dimensions),
         makeGeneralizedPrice2Function(dimensions),
         makeGeneralizedEggCrateFunction(dimensions),
         makeEllipticFunction(dimensions),
         makeRosenbrockFunction(dimensions),
         makePinter2Function(dimensions),
         makeQingFunction(dimensions),
         makeBBOBFunction(dimensions, 2, 1),
         makeBBOBFunction(dimensions, 6, 1),
         makeBBOBFunction(dimensions, 16, 1),
         makeBBOBFunction(dimensions, 17, 2)
  ))
}
