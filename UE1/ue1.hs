-- patrick's stuff

-- provided by lecture:
-- easydiff
easydiff f x h = (f (x+h) - f x) / h
-- differentiate (added parenthesis - ghci problem?)
differentiate h0 f x = map (easydiff f x) (repeat (halve h0))
-- helper function:
halve x = x/2



-- own work ... hopefully
-- diff
-- analogous to sqrt:
-- sqrt a0 eps N = within eps (repeat (next N) a0)



-- relativediff
-- analogous to relativesqrt:
-- relativesqrt a0 eps N = relative eps (repeat (next N) a0)
-- needing relative:
-- relative eps (cons a (cons b rest))
-- = b, if abs(a-b) <= eps * abs b
-- = relative eps (cons b rest), otherwise