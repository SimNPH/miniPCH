 dmstate_fun <- function(t, Q, pi, abs){
   check_t_Q_pi_abs(t, Q, pi, abs)
   function(x){
    .Call("_miniPCH_pdfFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, x)
   }
 }

 pmstate_fun <- function(t, Q, pi, abs){
   check_t_Q_pi_abs(t, Q, pi, abs)
   function(q){
     .Call("_miniPCH_cdfFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, q)
   }
 }

 hmstate_fun <- function(t, Q, pi, abs){
   check_t_Q_pi_abs(t, Q, pi, abs)
   function(x){
     .Call("_miniPCH_hazFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, x)
   }
 }

chmstate_fun <- function(t, Q, pi, abs){
  check_t_Q_pi_abs(t, Q, pi, abs)
  function(x){
    .Call("_miniPCH_cumhazFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, x)
  }
}

 smstate_fun <- function(t, Q, pi, abs){
   check_t_Q_pi_abs(t, Q, pi, abs)
   function(q){
     .Call("_miniPCH_survFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, q)
   }
 }

multistate_functions <- function(t, Q, pi, abs){
  res <- list(
    d  =  dmstate_fun(t, Q, pi, abs),
    p  =  pmstate_fun(t, Q, pi, abs),
    h  =  hmstate_fun(t, Q, pi, abs),
    ch = chmstate_fun(t, Q, pi, abs),
    s  =  smstate_fun(t, Q, pi, abs),
    t = t,
    Q = lambda,
    pi = pi,
    abs = abs
  )
  class(res) <- c("miniPCH")
  res
}

