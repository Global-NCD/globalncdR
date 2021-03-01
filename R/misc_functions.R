
var.missmatch = function(a,b){
  print("Present in dataset A but absent in dataset B")
  names(a)[!(names(a) %in% names(b))]
  
  print("Present in dataset B but absent in dataset A")
  names(b)[!(names(b) %in% names(a))]
}