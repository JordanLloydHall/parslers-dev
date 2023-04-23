# Parslers
An implementation of staged-selective parser combinators in Rust
# Grammar
```
<spec> ::= <statement> 
            | <statement> <program>
<statement> ::= ( 'pub' ) ? 'let' <ident> '=' <parser> ';'
<parser> ::= <ident> 
            | 'pure' '(' <pure_val> ')
            | 'satisfy' '(' <ident> ')'
            | 'try' '(' <parser> ')'
            | 'look' '(' <parser> ')'
            | 'neg_look' '(' <parser> ')'
            | 'ap' '(' <parser> ',' <parser> ')'
            | 'then' '(' <parser> ( ',' <parser> ) + ')'
            | 'before' '(' <parser> ( ',' <parser> ) + ')'
            | 'or' '(' <parser> ( ',' <parser> ) + ')'
            | 'empty'
            | 'branch' '(' <parser>, <parser>, <parser> ')'
<ident> ::= [a-zA-Z_][a-zA-Z0-9_]*
<pure_val> ::= 'func' '(' <ident> ')' 
            | 'val' '(' <string> ')'
<string> ::= [^"]*
```