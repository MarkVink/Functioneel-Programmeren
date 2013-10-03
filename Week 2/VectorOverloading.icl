implementation module VectorOverloading

import StdEnv

:: Vector2 a = {x0 :: a, x1 :: a}
instance ==   (Vector2 a) | == a   
where (==) x y = (x.x0 == y.x0 && x.x1 == y.x1)

instance zero (Vector2 a) | zero a 
where zero = {x0 = zero,x1 = zero}

instance one  (Vector2 a) | one a  
where one = {x0 = one,x1 = one}

instance ~    (Vector2 a) | ~ a    
where (~) x = {x0 = ~x.x0, x1 = ~x.x1} 

instance +    (Vector2 a) | + a    
where (+) x y = {x0 = x.x0 + y.x0,x1 = x.x1 + y.x1}

instance -    (Vector2 a) | - a    
where (-) x y = {x0 = x.x0 - y.x0,x1 = x.x1 - y.x1}

instance *    (Vector2 a) | * a    
where (*) x y = {x0 = x.x0 * y.x0,x1 = x.x1 * y.x1}

instance /    (Vector2 a) | / a    
where (/) x y = {x0 = x.x0 / y.x0,x1 = x.x1 / y.x1}
