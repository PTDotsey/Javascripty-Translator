3b i)  
  
    re ::= union  
    union ::= union `|` intersect | intersect  
    intersect ::= intersect '&' concat | concat  
    concat ::= concat not | not  
    not ::= '~' not | star  
    star ::= star'*' | star'+' | star'?' | atom  
    atom ::= '!' | '#' | '.' | c | '('re')'  

3b ii)  

The reason that a resursive descent parser would lead to an infinte loop with the above grammar is that in a top down parser such as a recursive descent parser, top down parsers use left-most derivation to derive the required string by using the start symbol of the grammar. As it always uses the left most driviation, it will recursivley keep using the deriviation recursively. lets us look at concat ::= concat not | not. In a recursive descent parser with left associativity, the rule becomes concat -> concat not | not, which resuts in an infinte recursion on concat, to fix this, the grammar could be written into a form that is a weakly equivalent right-hand form.



3b iii)

    re ::= union  
    union ::= intersect {`|` intersect}  
    intersect ::= concat {`&` concat}  
    concat ::= not {not}  
    not ::= {~} star  
    star ::= atom | atom {*} | atom {+} | atom {?}  
    atom ::= ! | # | . | c  

3b iv)  
  
    re ::= union  
    union ::= intersect unions  
    unions ::= € | `|` intersect unions  
    intersect ::= concat intersects  
    intersects ::= € | `&` concat intersects  
    concat ::= not concats  
    concats ::= € | not concats  
    not ::= nots star  
    nots ::= € | `~` nots  
    star ::= atom stars  
    stars ::= € | stars {*} | stars {+} | stars {?}  
    atom ::= ! | # | . | c | `(`re`)`  

3c i)

TYPEREGEX:
_____________________
Γ⊢/^re$/:	RegExp


TYPECALLTEST:

Γ⊢e1:	RegExp	Γ⊢e2:	string
_______________________________
Γ⊢e1.test(e2):	bool


SEARCHCALLREGEX1:

e1	→ e1’
__________________________
e1.f	(e2)	→ e1’.f	(e2)


SEARHCALLREGEX2:

r	=	/^re$/	e2	→ e2’
__________________________
r.f	(e2)	→ r.f	( e2’)


DOCALLREGEX:

r	=	/^re$/	s	=	str	b’	=	test(r,	s)
__________________________________
r.“test”(s)	→ b'