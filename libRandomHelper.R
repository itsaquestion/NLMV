# randomHelper

# random number between a range ====
randomIntRange = function(a,b){
  floor(runif(1,a,b+1))
}

randomFloatRange = function(a,b){
  runif(1,a,b)
}

randomFloatRangeTri = function(a,b){
  rtriangle(1,a,b)
}

randomIntRangeTri = function(a,b){
  floor(rtriangle(1,a,b+1))
}


# pick one number
pickOne = function(choices){
  sample(choices,1)
}
