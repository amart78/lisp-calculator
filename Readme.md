# Lisp Calculator (lc)

I normally live in shell so when I want to do math, I'm fairly limited
in my ability to understand the semantic meaning of a line of numbers 
and math operators.

the existing command line calculators are fairly limited in their capacity
to give results so I wrote this little tool to solve this problem
the readibility of: `1+2/2^(1+1/2)/(51+22^4)+1^(3/2+1)/(22+1/2^5)/55`
is fairly horrible so I wrote this program to make these things readable 
when I do math.

I use terminal formatting tricks to inject underlines underneath the 
numerators, but markdown doesn't play nice with these.

```
lc> 1+2/2^(1+1/2)/(51+22^4)+1^(3/2+1)/(22+1/2^5)/55
________________________________________________

              (3+1)
               2
1+2        +1^       = 1.0008283 [1.0008283]
    (1+1)   (22+1  )
       2          5
  2^            2^
         4  55
  (51+22^ )
________________________________________________
```
