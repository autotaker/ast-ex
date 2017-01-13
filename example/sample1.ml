let fix = fun f -> fun x -> (fun z -> f (fun y -> z z y)) (fun z -> f (fun y -> z z y)) x in 
let fact = fix (fun f -> fun x -> 
    if x <= 0 
      then 1 
      else x * f (x - 1)) in 
fact 10
