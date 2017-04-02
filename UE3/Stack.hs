module Stack (Stack,push,pop,top,emptyStack,stackEmpty) where
push       :: a -> Stack a -> Stack a
pop        :: Stack a -> Stack a
top        :: Stack a -> a
emptyStack :: Stack a

data Stack a = EmptyStk | Stk a (Stack a)

push x s            = Stk x s
pop EmptyStk        = error "pop from empty stack"
pop (Stk _ s)       = s
top EmptyStk        = error "top from empty stack"
top (Stk x _)       = x
emptyStack          = EmptyStk
stackEmpty EmptyStk = True
stackEmpty _        = False
