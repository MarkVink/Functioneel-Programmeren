implementation module TupleOverloading

import StdEnv

instance +    (a,b)   | + a & + b                
where (+) (x0,y0) (x1,y1) = (x0 + x1,y0 + y1)
instance +    (a,b,c) | + a & + b & + c          
where (+) (x0,y0,z0) (x1,y1,z1) = (x0 + x1,y0 + y1,z0 + z1)

instance -    (a,b)   | - a & - b                
where (-) (x0,y0) (x1,y1) = (x0 - x1,y0 - y1)
instance -    (a,b,c) | - a & - b & - c          
where (-) (x0,y0,z0) (x1,y1,z1) = (x0 - x1,y0 - y1,z0 - z1)

instance *    (a,b)   | * a & * b                
where (*) (x0,y0) (x1,y1) = (x0 * x1,y0 * y1)
instance *    (a,b,c) | * a & * b & * c
where (*) (x0,y0,z0) (x1,y1,z1) = (x0 * x1,y0 * y1,z0 * z1) 

instance /    (a,b)   | / a & / b                
where (/) (x0,y0) (x1,y1) = (x0 / x1,y0 / y1)
instance /    (a,b,c) | / a & / b & / c                        
where (/) (x0,y0,z0) (x1,y1,z1) = (x0 / x1,y0 / y1,z0 / z1)

instance zero (a,b)   | zero a & zero b          
where zero = (zero,zero)
instance zero (a,b,c) | zero a & zero b & zero c 
where zero = (zero,zero,zero)

instance one  (a,b)   | one a & one b            
where one = (one,one)
instance one  (a,b,c) | one a & one b & one c    
where one = (one,one,one)

instance ~    (a,b)   | ~ a & ~ b                
where (~) (x,y) = (~x,~y)
instance ~    (a,b,c)   | ~ a & ~ b & ~ c              
where (~) (x,y,z) = (~x,~y,~z)

Start  = (test (1,2), test (1,2,3))

test a = ( zero + a == a    && a    == a + zero
         , a - zero == a    && a    == ~ (zero - a)
         ,  one * a == a    && a    == a * one
         , zero * a == zero && zero == a * zero
         ,  a / one == a
         ,  ~ (~ a) == a
         )