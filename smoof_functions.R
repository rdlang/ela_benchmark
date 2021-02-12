### smoof interface for benchmark functions ###
library(smoof)
library(zoo)

#' Absolute / De Jong 3 / Schwefel 2.20 Function
#' https://arxiv.org/pdf/1308.4008.pdf (121)
makeAbsoluteFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Absolute Function", sep = ""),
    id = paste0("absolute_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(abs(x))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = -0
  )
}

#' Ackley 1 Function
#'
#' See http://benchmarkfcns.xyz/benchmarkfcns/ackleyfcn.html
#' 
# makeAckleyFunction

#' Ackley 2 Function
#'
#' See http://benchmarkfcns.xyz/benchmarkfcns/ackleyfcn.html
makeAckley02Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Ackley 2 Function", sep = ""),
    id = paste0("ackley02_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      a = 200
      b = 0.2
      c = 2 * pi
      d = mean(x^2)
      -a * exp(-b * sqrt(d))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-32.768, dimensions),
      upper = rep(32.768, dimensions),
      vector = TRUE
    ),
    tags = attr(makeAckleyFunction, "tags"),
    global.opt.params = rep(0, dimensions),
    global.opt.value = -200L
  )
}

#' Ackley 4 Function
#'
#' See http://benchmarkfcns.xyz/benchmarkfcns/ackley4fcn.html
makeAckley04Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Ackley 4 Function", sep = ""),
    id = paste0("ackley04_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      i = 1:(length(x) - 1)
      x1 = x[i]^2
      x2 = x[i + 1]^2
      a = 0.2
      b = 2 * pi
      sum(exp(-a * sqrt(x1 + x2)) + 3 * (cos(b * x1) + sin(b * x2)))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-35.0, dimensions),
      upper = rep(35.0, dimensions),
      vector = TRUE
    )
  )
}

#' Alpine01
#makeAlpine01Function

#' Alpine02
#makeAlpine02Function

#' Bohachevsky 1 Function
#' 
#' Generalization of http://benchmarkfcns.xyz/benchmarkfcns/bohachevskyn1fcn.html
makeBohachevsky01Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Bohachevsky 1 Function", sep = ""),
    id = paste0("bohachevsky01_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      i = 1:(length(x) - 1)
      x1 = x[i]^2
      x2 = x[i + 1]^2
      sum(x1 + 2*x2 - 0.3*cos(3*pi*x1) - 0.4*cos(4*pi*x2) + 0.7)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = -15.0,
      upper = 15.0,
      vector = TRUE
    )
  )
}
# makeBohachevskyN1Function

#' Bohachevsky 2 Function
#' 
#' Generalization of http://benchmarkfcns.xyz/benchmarkfcns/bohachevskyn2fcn.html
makeBohachevsky02Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Bohachevsky 2 Function", sep = ""),
    id = paste0("bohachevsky02_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      i = 1:(length(x) - 1)
      x1 = x[i]^2
      x2 = x[i + 1]^2
      sum(x1 + 2*x2 - 0.3*cos(3*pi*x1) * 0.4*cos(4*pi*x2) + 0.3)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-15.0, dimensions),
      upper = rep(15.0, dimensions),
      vector = TRUE
    )
  )
}

#' Bohachevsky 2 Function
#' 
#' Generalization of http://benchmarkfcns.xyz/benchmarkfcns/bohachevskyn2fcn.html
makeBohachevsky03Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Bohachevsky 3 Function", sep = ""),
    id = paste0("bohachevsky03_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      i = 1:(length(x) - 1)
      x1 = x[i]^2
      x2 = x[i + 1]^2
      sum(x1 + 2*x2 - 0.3*cos(3*pi*x1 + 4*pi*x2) + 0.3)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-15.0, dimensions),
      upper = rep(15.0, dimensions),
      vector = TRUE
    )
  )
}

#' Bonyadi-Michalewicz Function
#' 
#' link?
makeBonyadiMichalewiczFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Bonyadi-Michalewicz Function", sep = ""),
    id = paste0("bonyadi_michalewicz_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      a = prod(x + 1)
      b = prod((x - 1)^2 + 1)
      a / b
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5.0, dimensions),
      upper = rep(5.0, dimensions),
      vector = TRUE
    )
  )
}

#' Brown Function
# makeBrownFunction

#' Chung-Reynolds
#makeChungReynoldsFunction

#' Cigar / Bent-Cigar Function
#makeBentCigarFunction

#' Cosine Mixture
#makeCosineMixtureFunction

#' Cross-in-Tray
#' 
#' Note: generalized version
makeGeneralizedCrossInTrayFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Cross-in-Tray Function", sep = ""),
    id = paste0("cross_in_tray_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      a = prod(sin(x))
      b = exp(abs(100 - sqrt(sum(x^2))/pi))
      -0.0001 * (abs(a * b) + 1)^(0.1)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10.0, dimensions),
      upper = rep(10.0, dimensions),
      vector = TRUE
    )
  )
}

#' Cross Leg Table Function
#' 
#' Note: generalized version
makeGeneralizedCrossLegTableFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Cross Leg Table Function", sep = ""),
    id = paste0("cross_leg_table_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      a = exp(abs(100 - sum(x^2)/pi))
      b = prod(sin(x))
      -1 / (abs(a * b) + 1)^(0.1)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10.0, dimensions),
      upper = rep(10.0, dimensions),
      vector = TRUE
    )
  )
}

#' Crowned Cross Function
#' 
#' Note: generalized version
makeGeneralizedCrownedCrossFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Cross Leg Table Function", sep = ""),
    id = paste0("cross_leg_table_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      a = exp(abs(100 - sum(x^2)/pi))
      b = prod(sin(x))
      0.0001 * (abs(a * b) + 1)^(0.1)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10.0, dimensions),
      upper = rep(10.0, dimensions),
      vector = TRUE
    )
  )
}

#' Deb 1 Function
#' see: http://infinity77.net/global_optimization/test_functions_nd_D.html#go_benchmark.Deb01
makeDeb01Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Deb 1 Function", sep = ""),
    id = paste0("deb01_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      -mean(sin(5*pi*x)^6)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = -1.0,
      upper = 1.0,
      vector = TRUE
    )
  )
}

#' Deb 2 Function
#' see: http://infinity77.net/global_optimization/test_functions_nd_D.html#go_benchmark.Deb02
makeDeb02Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Deb 2 Function", sep = ""),
    id = paste0("deb02_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      -mean(sin(5*pi*(x^(0.75) - 0.05))^6)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
    global.opt.value = 0
  )
}

#' Deflected Corrugated Spring
#makeDeflectedCorrugatedSpringFunction

#' Different Powers / Sum of Different Powers
#makeSumOfDifferentSquaresFunction

#' Discus Function
#' source?
makeDiscusFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Discus Function", sep = ""),
    id = paste0("discus_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      10^6 * x[1]^2 + sum(x^2) - x[1]^2
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = -100.0,
      upper = 100.0,
      vector = TRUE
    )
  )
}

#' Dixon-Price
#makeDixonPriceFunction

#' Drop Wave 
#makeGeneralizedDropWaveFunction

#' Easom 
makeGeneralizedEasomFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Generalized Easom Function", sep = ""),
    id = paste0("easom_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      a = 20
      b = 0.2
      c = 2*pi
      x1 = (mean(x^2)) ^ (1/b)
      x2 = exp(mean(cos(c*x)))
      a - (a/exp(x1)) + exp(1) - x2
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(pi, dimensions),
    global.opt.value = -1
  )
}

#' Egg Crate Function
makeGeneralizedEggCrateFunction = function(dimensions) {
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Generalized Egg Crate Function", sep = ""),
    id = paste0("egg_crate_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(x^2) + 24*sum(sin(x)^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5, dimensions),
      upper = rep(5, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Egg Holder Function
makeGeneralizedEggHolderFunction = function(dimensions) {
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Generalized Egg Holder Function", sep = ""),
    id = paste0("egg_holder_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:(length(x) - 1)
      x1 = x[i]
      x2 = x[i + 1]
      sum(-1 * (x2 + 47) * sin(sqrt(abs(x2 + x1/2 + 47))) - x1*sin(sqrt(abs(x1 - x2 - 47))))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5, dimensions),
      upper = rep(5, dimensions),
      vector = TRUE
    ),
  )
}

#' Elliptic function
makeEllipticFunction = function(dimensions) {
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Elliptic Function", sep = ""),
    id = paste0("elliptic_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      m = length(x) - 1
      i = 1:length(x) - 1 
      sum(x^2 * 10^(6*(i/m)))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Exponential function
#makeExponentialFunction

#' Generalized Giunta
# https://arxiv.org/pdf/1308.4008.pdf (57)
makeGeneralizedGiuntaFunction = function(dimensions) {
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Generalized Giunta Function", sep = ""),
    id = paste0("giunta_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      a = 1.067 * x - 1
      b = sin(a)
      0.6 + sum(b + b^2 + 0.02 * sin(4 * a))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-1, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
  )
}

#' Griewank Function
#makeGriewankFunction

#' Hyper-Ellipsoid Function
#makeHyperEllipsoidFunction

#' Rotated Hyper-Ellisposid Function (Schwefel 1.2)
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/188-schwefel-s-function-no-1-2-double-sum-or-rotated-hyper-ellipsoid-function
#' implementation taken from: https://web.archive.org/web/20170604073331/http://www.sfu.ca/~ssurjano/Code/rothypr.html
makeRotatedHyperEllipsoidFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Rotated Hyper-Ellipsoid function", sep = ""),
    id = paste0("rotated_hyper_ellipsoid_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      xmat = matrix(rep(x, times=n), n, n, byrow=TRUE)
      xmatlow = xmat
      xmatlow[upper.tri(xmatlow)] = 0	
      inner = rowSums(xmatlow^2)
      sum(inner)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-65.536, dimensions),
      upper = rep(65.536, dimensions),
      vector = TRUE
    ),
  )
}

#' Infinity / Csendes / EXP3 function
#' http://infinity77.net/global_optimization/test_functions_nd_I.html
makeCsendesFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Csendes function", sep = ""),
    id = paste0("csendes_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      if (prod(x) == 0) 0
      else {
        sum(x^6 * (sin(1/x) + 2))
      }
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-1, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
  )
}

#' Katsuura Function
# http://infinity77.net/global_optimization/test_functions_nd_K.html
# https://github.com/jakobbossek/smoof/blob/master/todo-files/sof.katsuura.R - note bug in implementation, should be multiply for product
makeKatsuuraFunction = function(dimensions, d = 32) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Katsuura function", sep = ""),
    id = paste0("katsuura_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      r = 1
      #FIXME: quick and dirty implementation. Make it more R like.
      #FIXME: see http://www.geocities.ws/eadorio/mvf.pdf for another definition -.-
      for (i in 1:(length(x) - 1)) {
        t = 0
        for (k in 1:d) {
          t = t + floor(2^k * x[i]) * 2^(-k)
        }
        r = r  * (1 + (i + 1) * t)
      }
      return(r)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-1, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
  )
}

#' Levy-Montalvo Function (Generalization of Levy 13)
#' https://github.com/jakobbossek/smoof/blob/master/todo-files/sof.levy.R - note last term is incorrect in this link
makeLevy03Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Levy 3 function", sep = ""),
    id = paste0("levy03_", dimensions, "d"),
    fn = function(x) {
      n = length(x)
      w = 1 + (x - 1) / 4
      ww = w[-n]
      a = sin(pi * w[1])
      b = sum((ww - 1)^2 * (1 + 10 * sin(pi * ww + 1)^2))
      c = (w[n] - 1)^2
      return(a + b + c)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(1, dimensions),
    global.opt.value = 0
  )
}

#' Levy-Montalvo Function (Generalization of Levy 13)
#' https://github.com/jakobbossek/smoof/blob/master/todo-files/sof.levy.R
makeLevyMontalvo2Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Levy-Montalvo 2 function", sep = ""),
    id = paste0("levy_montalvo2_", dimensions, "d"),
    fn = function(x) {
      n = length(x)
      xx = x[-n]
      i = 1:(length(xx) - 1)
      xx1 = xx[i]
      xx2 = xx[i + 1]
      a = sin(3 * pi * x[1])^2
      b = sum((xx1 - 1)^2 * (1 + sin(3 * pi * xx2)^2))
      c = (x[n] - 1)^2 * (1 + sin(2 * pi * x[n])^2)
      0.1 * (a + b + c)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(1, dimensions),
    global.opt.value = 0
  )
}

#' MaxMod function (Schwefel 2.21)
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/189-schwefel-s-function-no-2-21
#' https://github.com/jakobbossek/smoof/blob/master/todo-files/sof.max.mod.R
makeMaxModFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d MaxMod function", sep = ""),
    fn = function(x) {
      max(abs(x))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Generalized Matyas
#' https://github.com/jakobbossek/smoof/blob/master/R/sof.matyas.R
makeGeneralizedMatyasFunction = function(dimensions) {
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Generalized Matyas Function", sep = ""),
    id = paste0("matyas_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      0.26 * sum(x^2) - 0.48 * prod(x)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Michalewicz
makeMichalewiczFunction = function(dimensions, m = 10) {
  assertCount(dimensions)
  assertNumber(m)
  force(m)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Michalewicz Function (m = ", m, ")", sep = ""),
    id = paste0("michalewicz_", dimensions, "d_m", m),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:length(x)
      (-1) * sum(sin(x) * (sin((i * x^2) / pi)^(2 * m)))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(pi, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = -0.966 * dimensions
  )
}

#' Mishra 1 Function
#' http://infinity77.net/global_optimization/test_functions_nd_M.html#go_benchmark.Mishra01
makeMishra01Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Mishra 1 function", sep = ""),
    id = paste0("mishra01_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      xn = n - sum(x[-n])
      (1 + xn) ^ xn
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
  )
}

#' Mishra 2 Function
#' http://infinity77.net/global_optimization/test_functions_nd_M.html#go_benchmark.Mishra02
makeMishra02Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Mishra 2 function", sep = ""),
    id = paste0("mishra02_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      i = 1:(n - 1)
      x1 = x[i]
      x2 = x[i + 1]
      xn = n - 0.5 * sum(x1 + x2)
      (1 + xn) ^ xn
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
  )
}

#' Mishra 3 Function
#' http://infinity77.net/global_optimization/test_functions_nd_M.html#go_benchmark.Mishra03
makeMishra03Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Mishra 3 function", sep = ""),
    id = paste0("mishra03_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sqrt(abs(cos(sqrt(abs(sum(x^2)))))) + 0.01 * sum(x)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10.0, dimensions),
      upper = rep(10.0, dimensions),
      vector = TRUE
    ),
  )
}

#' Mishra 7 / Factorial Function
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/181-mishra-function-no-7-or-factorial-function
makeMishra07Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Mishra 7 function", sep = ""),
    id = paste0("mishra07_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      (prod(x) - factorial(length(x)))^2
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10.0, dimensions),
      upper = rep(10.0, dimensions),
      vector = TRUE
    ),
  )
}

#' Mishra 11 / AMGM Function
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/182-mishra-s-function-no-11-or-amgm-function
makeMishra11Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Mishra 11 function", sep = ""),
    id = paste0("mishra11_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      a = abs(x)
      (mean(a) - (prod(a))^(1/length(x)))^2
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10.0, dimensions),
      upper = rep(10.0, dimensions),
      vector = TRUE
    ),
  )
}

#' Multi Modal function.
#' https://github.com/jakobbossek/smoof/blob/master/todo-files/sof.multi.mod.R
makeMultiModFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Multi Modal function", sep = ""),
    id = paste0("multimod_", dimensions, "d"),
    fn = function(x) {
      a = abs(x)
      sum(a) * prod(a)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Needle Eye Function
#https://al-roomi.org/benchmarks/unconstrained/n-dimensions/183-needle-eye-function
makeNeedleEyeFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Needle Eye function", sep = ""),
    id = paste0("needle_eye_", dimensions, "d"),
    fn = function(x) {
      eye = 0.0001
      n = length(x)
      a = abs(x)
      if (sum(a < eye) == n) {
        1
      } else if (sum(a > eye) == n) {
        sum(100 + a)
      } else {
        0
      }
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.value = 1
  )
}

#' Norwegian Function
makeNorwegianFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Norwegian function", sep = ""),
    id = paste0("norwegian_", dimensions, "d"),
    fn = function(x) {
      prod(cos(pi * x^3)*(99 + x)/100)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5, dimensions),
      upper = rep(5, dimensions),
      vector = TRUE
    ),
  )
}

#' Pathological Function
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/239-pathological-function
makePathologicalFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Pathological function", sep = ""),
    id = paste0("pathological_", dimensions, "d"),
    fn = function(x) {
      i = 1:(length(x) - 1)
      x1 = x[i]
      x2 = x[i + 1]
      a = sin(sqrt(100 * x1^2 + x2^2))^2 - 0.5
      b = 1 + 0.001 * (x1 - x2)^4 
      sum(a / b + 0.5)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Paviani Function
#' https://arxiv.org/pdf/1308.4008v1.pdf
#' http://infinity77.net/global_optimization/test_functions_nd_P.html#go_benchmark.Paviani
makeGeneralizedPavianiFunction = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Generalized Paviani function", sep = ""),
    id = paste0("paviani_", dimensions, "d"),
    fn = function(x) {
      sum(log(10 - x)^2 + log(x - 2)^2) - prod(x)^(0.2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(2.001, dimensions),
      upper = rep(9.999, dimensions),
      vector = TRUE
    ),
  )
}

#' Penalty 1 Function
#' citation?
makePenalty1Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Penalty 1 function", sep = ""),
    id = paste0("penalty1_", dimensions, "d"),
    fn = function(x) {
      u = function(xi, a, k, m) {
        if (xi > a) {
          k * (xi - a) ^ m
        } else if (xi < -a) {
          k * (-xi - a) ^ m
        } else {
          0
        }
      }
      y = 1 + 0.25 * (x + 1)
      n = length(y)
      yy = y[-n]
      i = 1:(length(yy) - 1)
      y1 = yy[i]
      y2 = yy[i + 1]
      a = 10 * sin(pi * y[1])^2
      b = sum((y1 - 1)^2 * (1 + 10*sin(pi * y2)^2))
      c = (y[n] - 1)^2
      (pi/30)*(a + b + c) + sum(mapply(u, x, a=10, k=100, m=4))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-50, dimensions),
      upper = rep(50, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(-1, dimensions),
    global.opt.value = 0
  )
}

#' Penalty 2 Function
#' citation?
makePenalty2Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Penalty 2 function", sep = ""),
    id = paste0("penalty2_", dimensions, "d"),
    fn = function(x) {
      u = function(xi, a, k, m) {
        if (xi > a) {
          k * (xi - a) ^ m
        } else if (xi < -a) {
          k * (-xi - a) ^ m
        } else {
          0
        }
      }
      n = length(x)
      i = 1:(n - 1)
      x1 = x[i]
      x2 = x[i + 1]
      a = sin(3 * pi * x[1])^2
      b = sum((x1 - 1)^2 * (1 + sin(3 *pi * x2)^2))
      c = (x[n] - 1)^2 * (1 + sin(2 * pi * x[n])^2)
      0.1*(a + b + c) + sum(mapply(u, x, a=5, k=100, m=4))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-50, dimensions),
      upper = rep(50, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(1, dimensions),
    global.opt.value = 0
  )
}

#' Periodic function
makePeriodicFunction = function(dimensions) {
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Periodic function", sep = ""),
    id = paste0("periodic_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      1 + sum(sin(x)^2) - 0.1 * exp(-sum(x^2))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0.9
  )
}

#' Perm 1 Function
# https://web.archive.org/web/20200205010532/https://www.sfu.ca/~ssurjano/permdb.html
makePerm1Function = function(dimensions, beta=0.5) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Perm 1 function", sep = ""),
    id = paste0("perm1_", dimensions, "d"),
    fn = function(x) {
      d <- length(x)
      i <- c(1:d)
      j <- matrix(rep(i,times=d), d, d, byrow=TRUE)
      
      xxmat <- matrix(rep(x, times=d), d, d, byrow=TRUE)
      inner <- rowSums((j^i + beta) * ((xxmat/j)^i-1))	
      sum(inner^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-dimensions, dimensions),
      upper = rep(dimensions, dimensions),
      vector = TRUE
    ),
    #global.opt.params = 1:dimensions,
    global.opt.value = 0
  )
}

#' Perm 2 Function
# https://www.sfu.ca/~ssurjano/perm0db.html
makePerm2Function = function(dimensions, beta=0.5) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Perm 2 function", sep = ""),
    id = paste0("perm2_", dimensions, "d"),
    fn = function(x) {
      n = length(x)
      ii <- c(1:n)
      jj <- matrix(rep(ii, times=n), n, n, byrow=TRUE)

      xxmat <- matrix(rep(x, times=n), n, n, byrow=TRUE)
      inner <- rowSums((jj + beta) * (xxmat^ii - (1/jj)^ii))
      sum(inner^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-dimensions, dimensions),
      upper = rep(dimensions, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(1, dimensions) / 1:dimensions,
    global.opt.value = 0
  )
}

#' Pinter 1 Function
# https://al-roomi.org/benchmarks/unconstrained/n-dimensions/245-pinter-s-function-no-1
# http://infinity77.net/global_optimization/test_functions_nd_P.html#go_benchmark.Pinter
makePinter1Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Pinter 1 function", sep = ""),
    id = paste0("pinter1_", dimensions, "d"),
    fn = function(x) {
      a = sum((1:length(x)) * x^2)
      i = 2:(length(x) - 1)
      b = sum((x[i-1] + 5*sin(x[i]) + x[i+1])^2)
      c = sum(log(1 + abs(i * sin(x[i-1])^2 + 2*x[i] + 3*x[i+1]))^2)
      a + b + c
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Pinter 2 Function
# https://al-roomi.org/benchmarks/unconstrained/n-dimensions/246-pinter-s-function-no-02
# http://infinity77.net/global_optimization/test_functions_nd_P.html#go_benchmark.Pinter
makePinter2Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Pinter 2 function", sep = ""),
    id = paste0("pinter2_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      a = sum(1:n * x^2)
      z = c(x[n], x, x[1])
      i = 2:(length(z) - 1)
      #print(sprintf("x = %s", x))
      #print(sprintf("i = %s", i))
      #print(sprintf("z = %s", z))
      b = sum(20 * i * sin(z[i-1]*sin(z[i]) + sin(z[i + 1]))^2)
      c = sum(i * log(1 + i * (z[i-1]^2 - 2*z[i] + 3*z[i+1] - cos(z[i]) + 1)^2))
      a + b + c
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Generalized Powell Singular 1
# https://web.archive.org/web/20200204072651/https://www.sfu.ca/~ssurjano/powell.html
makePowellSingular1Function = function(dimensions) {
  assertCount(dimensions)
  assert(dimensions %% 4 == 0)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Powell Singular 1 Function", sep = ""),
    id = paste0("powell_singular1_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      g <- function(z) {
        (z[3] + 10*z[1])^2 + 5*(z[2] - z[4])^2 + (z[1] - 2*z[2])^4 + 10*(z[3] - z[4])^4
      }
      sum(rollapply(x, width = 4, by = 4, g))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-4, dimensions),
      upper = rep(5, dimensions),
      vector = TRUE
    ),
    global.opt.value = 0
  )
}

#' Powell Sum
#makePowellSumFunction

#' Generalized Price 1 Function
makeGeneralizedPrice1Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Generalized Price 1 Function", sep = ""),
    id = paste0("price1_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum((abs(x) - 5)^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-500, dimensions),
      upper = rep(500, dimensions),
      vector = TRUE
    ),
    global.opt.value = 0
  )
}

#' Generalized Price 2 Function
makeGeneralizedPrice2Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Generalized Price 2 Function", sep = ""),
    id = paste0("price2_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      1 + sum(sin(x)^2) - 0.1*exp(-sum(x^2))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.value = 0.9
  )
}

#' Generalized Price 4 Function
makeGeneralizedPrice4Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Generalized Price 4 Function", sep = ""),
    id = paste0("price4_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:(length(x) - 1)
      x1 = x[i]
      x2 = x[i + 1]
      sum((2 * x1^3 * x2 - x2^3)^2 + (6 * x1 - x2^2 + x2)^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-500, dimensions),
      upper = rep(500, dimensions),
      vector = TRUE
    ),
  )
}

#' Qing Function
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/185-qing-s-function
makeQingFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Qing Function", sep = ""),
    id = paste0("qing_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:length(x)
      sum((x^2 - i)^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-500, dimensions),
      upper = rep(500, dimensions),
      vector = TRUE
    ),
    global.opt.value = 0
  )
}

#' Quadric (Schwefel 1.2) Function
makeQuadricFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Quadric function", sep = ""),
    id = paste0("quadric_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      xmat = matrix(rep(x, times=n), n, n, byrow=TRUE)
      xmatlow = xmat
      xmatlow[upper.tri(xmatlow)] = 0	
      inner = rowSums(xmatlow)^2
      sum(inner)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
  )
}

#' Quintic Function
makeQuinticFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Quintic function", sep = ""),
    id = paste0("quintic_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(abs(x^5 - 3*x^4 + 4*x^3 + 2*x^2 - 10*x - 4))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.value = 0
  )
}

#' Rana Function
#' https://www.researchgate.net/publication/278769271_Certified_Global_Minima_for_a_Benchmark_of_Difficult_Optimization_Problems/link/5e9494fd92851c2f529f225f/download
makeRanaFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Rana function", sep = ""),
    id = paste0("rana_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:(length(x) - 1)
      x1 = x[i]
      x2 = x[i + 1]
      t1 = sqrt(abs(x2 + x1 + 1))
      t2 = sqrt(abs(x2 - x1 + 1))
      sum(x1*cos(t1)*sin(t2) + (1 + x2)*sin(t1)*cos(t2))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-512, dimensions),
      upper = rep(512, dimensions),
      vector = TRUE
    ),
    global.opt.value = -928.5478
  )
}

#' Rastrigin Function
#makeRastriginFunction

#' Ripple 1 Function
#' http://infinity77.net/global_optimization/test_functions_nd_R.html#go_benchmark.Ripple01
makeRipple1Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Ripple 1 function", sep = ""),
    id = paste0("ripple1_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      u = -2 * log(2) * (((x - 0.1) / 0.8) ** 2)
      v = (sin(5 * pi * x) ** 6) + 0.1 * (cos(500 * pi * x) ** 2)
      sum(-exp(u) * v)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0.1, dimensions),
    global.opt.value = -2.2
  )
}

#' Ripple 25 Function
#' http://infinity77.net/global_optimization/test_functions_nd_R.html#go_benchmark.Ripple01
makeRipple25Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Ripple 25 function", sep = ""),
    id = paste0("ripple25_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      u = -2 * log(2) * (((x - 0.1) / 0.8) ** 2)
      v = (sin(5 * pi * x) ** 6)
      sum(-exp(u) * v)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0.1, dimensions),
    global.opt.value = -2
  )
}

#' Rosenbrock (De Jong 2) Function
#makeRosenbrockFunction

#' Generalized Modified Rosenbrock
#' http://infinity77.net/global_optimization/test_functions_nd_R.html#go_benchmark.RosenbrockModified
makeGeneralizedModifiedRosenbrockFunction = function(dimensions) {
  assertInt(dimensions, lower = 2)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Generalized Modified Rosenbrock Function", sep = ""),
    id = paste0("modified_rosenbrock_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = seq_len(length(x) - 1L)
      74 + sum(100 * (x[i + 1] - x[i]^2)^2 + (1 - x[i])^2 - 400 * exp(-((x[i] + 1)^2 + (x[i + 1] + 1)^2)/0.1))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-2, dimensions),
      upper = rep(2, dimensions),
      vector = TRUE
    ),
  )
}

#' Rotated Ellipse 1 function
# Generalized from https://arxiv.org/pdf/1308.4008.pdf (107)
makeRotatedEllipse1Function = function(dimensions) {
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Rotated Ellipse 1 Function", sep = ""),
    id = paste0("rotated_ellipse1_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:(length(x) - 1)
      sum(7*x[i]^2 - 6*sqrt(3)*x[i]*x[i+1] + 13*x[i+1]^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-500, dimensions),
      upper = rep(500, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Rotated Ellipse 2 function
# Generalized from https://arxiv.org/pdf/1308.4008.pdf (107)
makeRotatedEllipse2Function = function(dimensions) {
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Rotated Ellipse 2 Function", sep = ""),
    id = paste0("rotated_ellipse2_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:(length(x) - 1)
      sum(x[i]^2 - x[i]*x[i+1] + x[i+1]^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-500, dimensions),
      upper = rep(500, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Rump Function
#' https://al-roomi.org/benchmarks/unconstrained/2-dimensions/128-rump-function
makeRumpFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Rump Function", sep = ""),
    id = paste0("rump_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = seq_len(length(x) - 1L)
      a = (333.75 - x[i]^2)*x[i+1]^6
      b = (11*x[i]*x[i+1] - 121*x[i+1]^2 - 2) * x[i]^2
      c = 5.5*x[i + 1]^8 
      d = x[i] / (2 + x[i + 1])
      sum(abs(a + b + c + d))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-2, dimensions),
      upper = rep(2, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0.0
  )
}

#' Salomon Function
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/184-salomon-s-function
makeSalomonFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Salomon Function", sep = ""),
    id = paste0("salomon_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sq = sum(x^2)
      -cos(2*pi*sq) + 0.1*sqrt(sq) + 1
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0.0
  )
}

#' Sargan Function
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/184-salomon-s-function
makeSarganFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Sargan Function", sep = ""),
    id = paste0("sargan_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      r = 0
      n = length(x)
      for (i in 1:n) {
        innerR = 0
        for (j in 1:n) {
          if (i == j) next
          innerR = innerR + x[i] * x[j]
        }
        r = r + n * (x[i]^2 + 0.4 * innerR)
      }
      r
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0.0
  )
}

#' Schaffer 1 Function
#' generalized from https://al-roomi.org/benchmarks/unconstrained/2-dimensions/96-modified-schaffer-s-function-no-1
makeSchafferN1Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schaffer Function N. 1", sep = ""),
    id = paste0("schaffer01_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:(length(x) - 1)
      a = x[i]^2
      b = x[i+1]^2
      sum(0.5 + (sin(a + b)^2 - 0.5) / (1 + 0.001 * (a + b))^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0.0
  )
}

#' Schaffer 2 Function
#' generalized from https://al-roomi.org/benchmarks/unconstrained/2-dimensions/97-modified-schaffer-s-function-no-2
makeGeneralizedSchafferN2Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schaffer Function N. 2", sep = ""),
    id = paste0("schaffer02_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:(length(x) - 1)
      a = x[i]^2
      b = x[i+1]^2
      sum(0.5 + (sin(a - b)^2 - 0.5) / (1 + 0.001 * (a + b))^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0.0
  )
}

#' Schaffer 3 Function
#' generalized from https://al-roomi.org/benchmarks/unconstrained/2-dimensions/97-modified-schaffer-s-function-no-3
makeGeneralizedSchafferN3Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schaffer Function N. 3", sep = ""),
    id = paste0("schaffer03_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:(length(x) - 1)
      a = x[i]^2
      b = x[i+1]^2
      sum(0.5 + (sin(cos(abs(a - b)))^2 - 0.5) / (1 + 0.001 * (a + b))^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.value = 0.001566855
  )
}

#' Schaffer 4 Function
#' generalized from https://al-roomi.org/benchmarks/unconstrained/2-dimensions/97-modified-schaffer-s-function-no-4
makeGeneralizedSchafferN4Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schaffer Function N. 4", sep = ""),
    id = paste0("schaffer04_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:(length(x) - 1)
      a = x[i]^2
      b = x[i+1]^2
      sum(0.5 + (cos(sin(abs(a - b)))^2 - 0.5) / (1 + 0.001 * (a + b))^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.value = 0.2925786
  )
}

#' Schumer-Steiglitz
# https://al-roomi.org/benchmarks/unconstrained/n-dimensions/242-schumer-steiglitz-s-function-no-2
makeSchumerSteiglitzFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schumer-Steiglitz", sep = ""),
    id = paste0("schumer_steiglitz_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(x^4)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Schwefel 1
makeSchwefel1Function = function(dimensions, alpha=sqrt(pi)) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schwefel 1", sep = ""),
    id = paste0("schwefel1_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(x^2)^alpha
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Schwefel 2.4
#' https://arxiv.org/pdf/1308.4008.pdf (120)
makeSchwefel24Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schwefel 2.4", sep = ""),
    id = paste0("schwefel2_4_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum((x - 1)^2 + (x[1] - x^2)^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(1, dimensions),
    global.opt.value = 0
  )
}

#' Schwefel 2.21
makeSchwefel21Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schwefel 2.21", sep = ""),
    id = paste0("schwefel2_21_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      max(abs(x))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Schwefel 2.22
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/190-schwefel-s-function-2-22
#' https://arxiv.org/pdf/1308.4008.pdf (124)
makeSchwefel22Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schwefel 2.22", sep = ""),
    id = paste0("schwefel2_22_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(abs(x)) + prod(abs(x))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Schwefel 2.23
makeSchwefel23Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schwefel 2.23", sep = ""),
    id = paste0("schwefel2_23_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(x^10)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Schwefel 2.26 
#makeSchwefelFunction

#' Modified Schwefel 2.26
makeModifiedSchwefel226Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Modified Schwefel 2.26 function", sep = ""),
    id = paste0("modified_schwefel2_26_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      418.9829 * dimensions - sum(x * sin(sqrt(abs(x))))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-500, dimensions),
      upper = rep(500, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(420.9687, dimensions),
    global.opt.value = 0
  )
}

#' Schwefel 2.36
makeModifiedSchwefel236Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Schwefel 2.36 function", sep = ""),
    id = paste0("schwefel2_36_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      -prod(x) * (72 - 2 * sum(x))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(500, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(12, dimensions),
    global.opt.value = -3456
  )
}

#' Shubert 
#' https://arxiv.org/pdf/1308.4008.pdf
makeGeneralizedSchubertFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Shubert", sep = ""),
    id = paste0("shubert_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      j = 1:5
      a = mapply(function(z) sum(j * cos((j + 1) * z + j)), x)
      prod(a)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
  )
}

#' Shubert 3 
#' https://arxiv.org/pdf/1308.4008.pdf
makeSchubert3Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Shubert 3", sep = ""),
    id = paste0("shubert3_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      j = 1:5
      a = mapply(function(z) sum(j * sin((j + 1) * z + j)), x)
      sum(a)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
  )
}

#' Shubert 4 
#' https://arxiv.org/pdf/1308.4008.pdf
makeSchubert4Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Shubert 4", sep = ""),
    id = paste0("shubert4_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      j = 1:5
      a = mapply(function(z) sum(j * cos((j + 1) * z + j)), x)
      sum(a)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
  )
}

#' Sine Envelope
#' http://infinity77.net/global_optimization/test_functions_nd_S.html#go_benchmark.SineEnvelope
#' 
makeSineEnvelopeFunction = function(dimensions) {
  assertInt(dimensions, lower = 2)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Sine Envelope Function", sep = ""),
    id = paste0("sine_envelope_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = seq_len(length(x) - 1L)
      sum(0.5 + (sin(sqrt(x[i]^2 + x[i+1]^2)) - 0.5)/(1 + 0.001*(x[i]^2 +x[i+1]^2))^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Sinusoidal
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/186-sinusoidal-function
makeSinusoidalFunction = function(dimensions, a=2.5, b=5, z=30) {
  assertInt(dimensions, lower = 2)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Sinusoidal Function", sep = ""),
    id = paste0("sinusiodal_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      r1 = prod(sin(x - z))
      r2 = prod(sin(b * (x - z)))
      -(a * r1 + r2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(180, dimensions),
      vector = TRUE
    ),
    global.opt.value = 90 + z
  )
}

#' Spherical
#makeSphereFunction

#' Step 1
#' https://github.com/jakobbossek/smoof/blob/master/todo-files/sof.step.n1.R
makeStepN1Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Step Function N. 1", sep = ""),
    id = paste0("step1_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(floor(abs(x)))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Step 2
#' fixed from https://github.com/jakobbossek/smoof/blob/master/todo-files/sof.step.n2.R
#' https://arxiv.org/pdf/1308.4008.pdf
makeStepN2Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Step Function N. 2", sep = ""),
    id = paste0("step2_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum((floor(x + 0.5))^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Step 3
#' https://github.com/jakobbossek/smoof/blob/master/todo-files/sof.step.n3.R
#' https://arxiv.org/pdf/1308.4008.pdf
makeStepN3Function = function(dimensions) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Step Function N. 3", sep = ""),
    id = paste0("step3_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(floor(x^2))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-100, dimensions),
      upper = rep(100, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Plateau (Stepint)
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/195-stepint-function
makePlateauFunction = function(dimensions, c=30) {
  assertCount(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Plateau Function", sep = ""),
    id = paste0("plateau_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(floor(x)) + c
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5.12, dimensions),
      upper = rep(5.12, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = c
  )
}

#' Stretched V Sine Wave
#' https://arxiv.org/pdf/1308.4008.pdf
makeStretchedVSineWaveFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Stretched V Sine Wave Function", sep = ""),
    id = paste0("stretched_v_sine_wave_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = seq_len(length(x) - 1L)
      z = x[i]^2 + x[i+1]^2
      sum((z^0.25) * (sin(50 * (z^0.1))^2 + 0.1))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Sum Squares
#' https://arxiv.org/pdf/1308.4008.pdf
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/191-sphere-model-spherical-contours-square-sum-harmonic-or-1st-de-jong-s-function
#' I'm sure this is the same function as one of the above
makeSumSquaresFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Sum Squares Function", sep = ""),
    id = paste0("sum_squares_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:length(x)
      sum(i * x^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Sum of Different Powers
#makeSumOfDifferentSquaresFunction

#' Styblinski-Tang
#' https://arxiv.org/pdf/1308.4008.pdf
makeStyblinkskiTangFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Styblinkski-Tang Function", sep = ""),
    id = paste0("styblinskiTang_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      0.5 * sum(x^4 - 16*x^2 + 5*x)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5, dimensions),
      upper = rep(5, dimensions),
      vector = TRUE
    ),
    global.opt.value = -78.332
  )
}

#' Holder Table 1
#' https://arxiv.org/pdf/1308.4008.pdf
makeHolderTable1Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Holder Table 1 Function", sep = ""),
    id = paste0("holder_table1_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      a = prod(cos(x))
      if (sum(x) < 0) {
        b = exp(1)
      } else {
        b = exp(abs(1 - sqrt(sum(x))/pi))
      }
      -abs(a * b)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Holder Table 3 / Carrom Table
#' https://arxiv.org/pdf/1308.4008.pdf (147) (note errror)
#' https://al-roomi.org/benchmarks/unconstrained/2-dimensions/32-carrom-table-function
makeHolderTable3Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Holder Table 3 Function", sep = ""),
    id = paste0("holder_table3_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      (-1 / 30) * exp(2*abs(1 - (sqrt(sum(x^2))/pi))) * prod(cos(x)^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Trid / Neumaier 4
#' https://arxiv.org/pdf/1308.4008.pdf
#' https://web.archive.org/web/20190103141437/http://www.sfu.ca/~ssurjano/Code/tridr.html
makeTridFunction = function(dimensions) {
  assertInt(dimensions, lower = 2)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Trid Function", sep = ""),
    id = paste0("trid_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 2:length(x)
      x1 = x[i]
      x2 = x[i - 1]
      sum((x - 1)^2) - sum(x1 * x2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-20, dimensions),
      upper = rep(20, dimensions),
      vector = TRUE
    ),
  )
}

#' Trigonometric 1
#' https://arxiv.org/pdf/1308.4008.pdf
makeTrigonometric1Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Trigonometric 1 Function", sep = ""),
    id = paste0("trigonometric1_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      # n = length(x)
      # innerFn = function(x, i) {
      #   xi = x[i]
      #   n - sum(cos(x) + i * (1 - cos(xi) - sin(xi)))^2
      # }
      # sum(mapply(innerFn, x, i=seq_along(x)))
      n = length(x)
      r = 0
      for (i in 1:length(x)) {
        innerR = 0
        for (j in 1:length(x)) {
          innerR = innerR + cos(x[j]) + i * (1 - cos(x[i]) - sin(x[i]))
        }
        r = r + (n - innerR)^2
      }
      r
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(pi, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Trigonometric 2
#' https://arxiv.org/pdf/1308.4008.pdf
makeTrigonometric2Function = function(dimensions, x_star=rep(0.9, dimensions)) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Trigonometric 2 Function", sep = ""),
    id = paste0("trigonometric2_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      1 + sum(8 * sin(7 * (x - x_star)^2)^2 + 6 * sin(14 * (x[1] - x_star[1])^2)^2 + (x - x_star)^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-500, dimensions),
      upper = rep(500, dimensions),
      vector = TRUE
    ),
    global.opt.params = x_star,
    global.opt.value = 1
  )
}

#' Vincent Function
#' similar to https://github.com/jakobbossek/smoof/blob/master/R/sof.vincent.R
makeVincentFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Vincent Function", sep = ""),
    id = paste0("vincent_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      - sum(sin(10 * log(x))) 
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0.25, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(7.70628098, dimensions),
    global.opt.value = -dimensions
  )
}

#' Wavy
#' https://arxiv.org/pdf/1308.4008.pdf (165)
makeWavyFunction = function(dimensions, k=10) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Vincent Function", sep = ""),
    id = paste0("vincent_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      1 - mean(cos(k * x) * exp(-0.5 * x^2))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-pi, dimensions),
      upper = rep(pi, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Weierstrass
#' https://arxiv.org/pdf/1308.4008.pdf (166)
makeWeierstrassFunction = function(dimensions, a=0.5, b=3, jmax=20) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Weierstrass Function", sep = ""),
    id = paste0("weierstrass_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      j = 0:jmax
      n = length(x)
      z = mapply(function(y) sum(a^j * cos(2*pi*(b^j)*(y +0.5))) - n*sum(a^j * cos(pi*b^j)), x)
      sum(z)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-0.5, dimensions),
      upper = rep(0.5, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 4
  )
}

#' Whitley
#' https://arxiv.org/pdf/1308.4008.pdf (167)
makeWhitleyFunction = function(dimensions, a=0.5, b=3, jmax=20) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Whitley Function", sep = ""),
    id = paste0("whitley_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      r = 0
      for (i in 1:length(x)) {
        innerR = 0
        for (j in 1:length(x)) {
          tmp = 100*(x[i]^2 - x[j])^2 + (1 - x[j])^2
          innerR = innerR + (1/4000) * (tmp)^2 - cos(tmp) + 1
        }
        r = r + innerR
      }
      r
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10.24, dimensions),
      upper = rep(10.24, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(1, dimensions),
    global.opt.value = 0
  )
}

#' Xin-She Yang 2 
#' https://arxiv.org/pdf/1308.4008.pdf (170)
makeXinSheYang2Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Xin-She Yang 2 Function", sep = ""),
    id = paste0("xin_she_yang2_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(abs(x)) * exp(-sum(sin(x^2)))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-2*pi, dimensions),
      upper = rep(2*pi, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Xin-She Yang 3 
#' https://arxiv.org/pdf/1308.4008.pdf (171)
makeXinSheYang3Function = function(dimensions, m=5, beta=15) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Xin-She Yang 3 Function", sep = ""),
    id = paste0("xin_she_yang3_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      a = exp(-sum((x / beta)^(2*m)))
      b = exp(-sum(x^2))
      c = prod(cos(x)^2)
      a - 2 * b * c
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-20, dimensions),
      upper = rep(20, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = -1
  )
}

#' Xin-She Yang 4
#' https://arxiv.org/pdf/1308.4008.pdf (171)
makeXinSheYang4Function = function(dimensions, m=5, beta=15) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Xin-She Yang 4 Function", sep = ""),
    id = paste0("xin_she_yang4_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      a = sum(sin(x)^2)
      b = exp(-sum(x^2))
      c = exp(-sum(sin(sqrt(abs(x))))^2)
      (a - b) * c
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = -1
  )
}

#' Yao-Liu 4
#' http://infinity77.net/global_optimization/test_functions_nd_Y.html#go_benchmark.YaoLiu04
makeYaoLiu4Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Yao-Liu 4 Function", sep = ""),
    id = paste0("yao_liu4_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      max(abs(x))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Yao-Liu 9
#' http://infinity77.net/global_optimization/test_functions_nd_Y.html#go_benchmark.YaoLiu09
makeYaoLiu9Function = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Yao-Liu 9 Function", sep = ""),
    id = paste0("yao_liu9_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum(x^2 - 10 * cos(2*pi*x) + 10)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5.12, dimensions),
      upper = rep(5.12, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#' Zakharov
#' http://infinity77.net/global_optimization/test_functions_nd_Y.html#go_benchmark.YaoLiu09
makeZakharovFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Zakharov Function", sep = ""),
    id = paste0("zakharov_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:length(x)
      tmp = 0.5 * sum(i * x)
      sum(x^2) + tmp^2 + tmp^4
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = -1
  )
}

#' Zero Sum
#' http://infinity77.net/global_optimization/test_functions_nd_Z.html#go_benchmark.ZeroSum
makeZeroSumFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Zero Sum Function", sep = ""),
    id = paste0("zero_sum_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      s = sum(x)
      if (s == 0) {
        0
      } else {
        1 + sqrt(10000 * abs(s))
      }
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

all_fns = list(
  makeAbsoluteFunction, #1
  makeAckleyFunction,
  makeAckley02Function,
  makeAckley04Function,
  makeAlpine01Function,
  makeAlpine02Function, 
  makeBohachevsky01Function,
  makeBohachevsky02Function, 
  makeBohachevsky03Function, 
  makeBonyadiMichalewiczFunction, #10
  makeBrownFunction,
  makeChungReynoldsFunction,
  makeBentCigarFunction,
  makeCosineMixtureFunction, 
  makeGeneralizedCrossInTrayFunction,
  makeGeneralizedCrossLegTableFunction,
  makeGeneralizedCrownedCrossFunction,
  makeDeb01Function,
  makeDeb02Function,
  makeDeflectedCorrugatedSpringFunction, #20
  makeSumOfDifferentSquaresFunction,
  makeDiscusFunction,
  makeDixonPriceFunction,
  makeGeneralizedDropWaveFunction,
  makeGeneralizedEasomFunction,
  makeGeneralizedEggCrateFunction,
  makeGeneralizedEggHolderFunction,
  makeEllipticFunction,
  makeExponentialFunction,
  makeGeneralizedGiuntaFunction, #30
  makeGriewankFunction, 
  makeHyperEllipsoidFunction,
  makeRotatedHyperEllipsoidFunction,
  makeCsendesFunction,
  makeKatsuuraFunction,
  makeLevy03Function,
  makeLevyMontalvo2Function,
  makeMaxModFunction,
  makeGeneralizedMatyasFunction,
  makeMichalewiczFunction, #40
  makeMishra01Function,
  makeMishra02Function,
  makeMishra03Function,
  makeMishra07Function,
  makeMishra11Function,
  makeMultiModFunction,
  makeNeedleEyeFunction,
  makeNorwegianFunction,
  makePathologicalFunction,
  makeGeneralizedPavianiFunction, #50
  makePenalty1Function,
  makePenalty2Function,
  makePeriodicFunction,
  makePerm1Function,
  makePerm2Function,
  makePinter1Function,
  makePinter2Function,
  makePowellSingular1Function,
  makePowellSumFunction,
  makeGeneralizedPrice1Function, #60
  makeGeneralizedPrice2Function,
  makeGeneralizedPrice4Function,
  makeQingFunction,
  makeQuadricFunction,
  makeQuinticFunction,
  makeRanaFunction,
  makeRastriginFunction,
  makeRipple1Function,
  makeRipple25Function,
  makeRosenbrockFunction, #70
  makeGeneralizedModifiedRosenbrockFunction,
  makeRotatedEllipse1Function,
  makeRotatedEllipse2Function,
  makeRumpFunction,
  makeSalomonFunction,
  makeSarganFunction,
  makeSchafferN1Function,
  makeGeneralizedSchafferN2Function,
  makeGeneralizedSchafferN3Function,
  makeGeneralizedSchafferN4Function,
  makeSchumerSteiglitzFunction,
  makeSchwefel1Function, #80
  makeSchwefel24Function,
  makeSchwefel21Function,
  makeSchwefel22Function,
  makeSchwefel23Function,
  makeSchwefelFunction,
  makeModifiedSchwefel226Function,
  makeModifiedSchwefel236Function,
  makeGeneralizedSchubertFunction,
  makeSchubert3Function,
  makeSchubert4Function, #90
  makeSineEnvelopeFunction,
  makeSinusoidalFunction,
  makeSphereFunction,
  makeStepN1Function,
  makeStepN2Function,
  makeStepN3Function,
  makePlateauFunction,
  makeStretchedVSineWaveFunction,
  makeSumSquaresFunction,
  makeSumOfDifferentSquaresFunction, #100
  makeStyblinkskiTangFunction,
  makeHolderTable1Function,
  makeHolderTable3Function,
  makeTridFunction,
  makeTrigonometric1Function,
  makeTrigonometric2Function,
  makeVincentFunction,
  makeWavyFunction,
  makeWeierstrassFunction,
  makeWhitleyFunction, #110
  makeXinSheYang2Function,
  makeXinSheYang3Function,
  makeXinSheYang4Function,
  makeYaoLiu4Function,
  makeYaoLiu9Function,
  makeZakharovFunction,
  makeZeroSumFunction
)


#' Quartic / Holzman's Function No. 02
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/272-holzman-s-function-no-02
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/161-quartic-or-modified-4th-de-jong-s-function
makeQuarticFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Quartic function", sep = ""),
    id = paste0("quartic_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:length(x)
      sum(i * x^4)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = 0
  )
}

#'  Inverted Cosine-Wave Function 
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/178-inverted-cosine-wave-function
makeInvertedCosineWaveFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d  Inverted Cosine-Wave function", sep = ""),
    id = paste0("inverted_cosine_wave_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:(length(x) - 1)
      tmp = (x[i]^2 + x[i+1]^2 + 0.5 * x[i] * x[i+1])
      -sum(exp(-tmp / 8) * cos(4 * sqrt(tmp)))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5, dimensions),
      upper = rep(5, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0, dimensions),
    global.opt.value = -dimensions + 1
  )
}

#' L / F2 Function 
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/276-l-or-f2-function
makeLFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d  L function", sep = ""),
    id = paste0("l_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      k = 6
      l1 = 5.1
      l2 = 0.5
      l3 = 4*log(2)
      l4 = 0.066832364099628
      l5 = 0.64
      a = sin(l1*pi*x + l2)^k
      b = exp(-l3 * ((x - l4)/l5)^2)
      -prod(a * b)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(0.066832364099628, dimensions),
    global.opt.value = -1
  )
}

#' Lunacek's bi-Rastrigin Function 
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/229-lunacek-s-bi-rastrigin-function
makeLunacekBiRastriginFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d  Lunacek's bi-Rastrigin function", sep = ""),
    id = paste0("lunacek_bi_rastrigin_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      d = 1
      s = 1 - 1/(2*sqrt(n + 20) - 8.2)
      meu1 = 2.5
      meu2 = sqrt((meu1^2 - d)/s)
      a = sum((x - meu1)^2)
      b = d * n + s * sum((x - meu2)^2)
      c = 10 * sum(1 - cos(2*pi*(x - meu1)))
      min(a, b) + c
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-5.12, dimensions),
      upper = rep(5.12, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(2.5, dimensions),
    global.opt.value = 0
  )
}

#' Lunacek's bi-Sphere Function 
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/228-lunacek-s-bi-sphere-function
makeLunacekBiSphereFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d  Lunacek's bi-Sphere function", sep = ""),
    id = paste0("lunacek_bi_sphere_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      n = length(x)
      d = 1
      s = 1 - 1/(2*sqrt(n + 20) - 8.2)
      meu1 = 2.5
      meu2 = sqrt((meu1^2 - d)/s)
      a = sum((x - meu1)^2)
      b = d * n + s * sum((x - meu2)^2)
      min(a, b)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(2.5, dimensions),
    global.opt.value = 0
  )
}

#' Hyper-Grid / M Function 
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/274-m-or-hyper-grid-function
makeHyperGridFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d  Hyper-Grid function", sep = ""),
    id = paste0("hyper_grid_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      c = 5
      alpha = 6
      -mean(sin(c * pi * x)^alpha)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
    global.opt.value = -1
  )
}

#' Moved-Axis Parallel Hyper-Ellipsoid Function 
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/230-moved-axis-parallel-hyper-ellipsoid-function
makeMovedAxisHyperEllipsoidFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Moved-Axis Parallel Hyper-Ellipsoid function", sep = ""),
    id = paste0("moved_axis_hyper_ellipsoid_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      i = 1:length(x)
      sum((i * (x - 5 * i))^2)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-500, dimensions),
      upper = rep(500, dimensions),
      vector = TRUE
    ),
    global.opt.value = 0
  )
}

#' Simpleton-n Function
#' https://al-roomi.org/benchmarks/unconstrained/n-dimensions/273-simpleton-n-function
makeSimpletonFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Simpleton function", sep = ""),
    id = paste0("simpleton_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      -sum(x)
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(10, dimensions),
    global.opt.value = -10 * dimensions
  )
}

#'  Type-I Simple Deceptive Problem 
#'  https://al-roomi.org/benchmarks/unconstrained/n-dimensions/233-type-i-simple-deceptive-problem
#'  Flipped sign to transform into a minimization problem
makeTypeIFunction = function(dimensions) {
  assertCount(dimensions)
  force(dimensions)
  makeSingleObjectiveFunction(
    name = paste(dimensions, "-d Type-I Simple Deceptive Problem function", sep = ""),
    id = paste0("typeI_", dimensions, "d"),
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      g = function(y, alpha=0.8) {
        if (y >= 0 && y <= alpha) {
          alpha - y
        } else {
          (y - alpha) / (1 - alpha)
        }
      }
      -mean(mapply(g, x))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = TRUE
    ),
    global.opt.params = rep(1, dimensions),
    global.opt.value = -1
  )
}

#'  Generalized Brent Function
#'  Generalized from fn 24 here: https://arxiv.org/pdf/1308.4008.pdf
makeBrentFunction = function() {
  makeSingleObjectiveFunction(
    name = "Brent Function",
    id = "brent_",
    fn = function(x) {
      assertNumeric(x, len = dimensions, any.missing = FALSE, all.missing = FALSE)
      sum((x + 10)^2) + exp(-sum(x^2))
    },
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(-10, dimensions),
      upper = rep(10, dimensions),
      vector = TRUE
    ),
    tags = attr(makeBrentFunction, "tags"),
    global.opt.params = rep(-10, dimensions),
    global.opt.value = 0
  )
}

