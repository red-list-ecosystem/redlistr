getRelSev <- function(val.t1, val.t2, coll.theshold){
  relsev <- (val.t1 - val.t2)/(val.t1 - coll.theshold)*100
  return (relsev)
}
