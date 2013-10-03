definition module TupleOverloading

import StdEnv

instance +    (a,b)   | + a & + b
instance +    (a,b,c) | + a & + b & + c

instance -    (a,b)   | - a & - b
instance -    (a,b,c) | - a & - b & - c

instance *    (a,b)   | * a & * b
instance *    (a,b,c) | * a & * b & * c

instance /    (a,b)   | / a & / b
instance /    (a,b,c) | / a & / b & / c

instance zero (a,b)   | zero a & zero b
instance zero (a,b,c) | zero a & zero b & zero c

instance one  (a,b)   | one a & one b
instance one  (a,b,c) | one a & one b & one c

instance ~    (a,b)   | ~ a & ~ b
instance ~    (a,b,c) | ~ a & ~ b & ~ c

