definition module VectorOverloading

import StdEnv

:: Vector2 a = {x0 :: a, x1 :: a}

instance ==   (Vector2 a) | == a
instance zero (Vector2 a) | zero a
instance one  (Vector2 a) | one a
instance ~    (Vector2 a) | ~ a
instance +    (Vector2 a) | + a
instance -    (Vector2 a) | - a
instance *    (Vector2 a) | * a
instance /    (Vector2 a) | / a