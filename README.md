# readysetboole
An introduction to boolean algebra and set theory, through exercises implemented in a functional language (I chose ocaml).
You can find the main.ml file with tests in the bin folder, and the exercises are in the lib folder.

# Learning
This project serves multiple purposes for me.
1. Learning ocaml
2. Understanding boolean algebra, bitwise operations, logic. Things I've always faded, and am now trying to grasp fully.
3. Discovering set theory.

## Ressources I found on the way, some useful, some just interesting
0. adder and 01 multiplier
    - doing module 00 and 01 of nand2tetris.org helped me get an intuition for how logic gates work, and easily understand how to do addition with only boolean operators
    - https://en.wikipedia.org/wiki/Binary_multiplier
    - https://en.wikipedia.org/wiki/Bitwise_operation
    - https://en.wikipedia.org/wiki/Bit_manipulation
2. Gray code a.k.a reflected binary code (RBC or RB)
    - This seems very confusing at first, but once again [wikipedia saves the day](https://en.wikipedia.org/wiki/Gray_code)
    - llama with groq inference has an [interesting explanation](https://poe.com/s/iRiSTKUMqsMogBzanFU1) for it
    - I didn't really get this one, saving in case I come back to gray codes https://zerobone.net/blog/cs/gray-codes/
3. Boolean evaluation
    - Time to get into trees !
    - Input is boolean expression in reverse polish notation (RPN)
    - pdf recommends starting to build an abstract tree.
    - I parsed by putting operands and operators in two queues, then processing one operator at a time, and adding each new node at the end of the operands queue.
        - That approach actually led to a wrong AST, the solution was even simpler.
            1. loop over the string, put operands in a stack
            2. as soon as you encounter an operator, take out 1 (unary operator, like NOT) or 2 operands from the stack, make a node (operator with two operands) and put this node back on the stack
            3. continue looping until end of string
            4. if stack has more than one operand, the expression is invalid. Else, take the operand out, which is now the root node of the tree
    - wikipedia goated again
        - https://en.wikipedia.org/wiki/Reverse_Polish_notation
        - https://en.wikipedia.org/wiki/Shunting_yard_algorithm
4. print truth tables
    - this is a bit annoying. I have to generate all possible combinations for up to 27 variables, evaluate every possible expression, etc. Not sure how to do this in a simple way
5. negation normal form (NNF)
    - for this one you have to convert the ast back to RPN
    - then it's a pretty simple procedure, you have to recursively
        - remove all double negation
        - apply de morgan laws
6. conjunctive normal form (CNF)
    - not sure how yet
- ocaml. Not easy to learn when coming from imperative and OOP. Also just lots of quite curious syntax/design decisions, but it has a strong toolkit, big community and plenty ressources. And it's fast like c, and compiles to elf, and has a pretty REPL
    - [This course by Cornell](https://cs3110.github.io/textbook) is pretty good, I would start there and complement with the tutorials/guides/pages on ocaml.org.
    - Start coding simple stuff in the REPL, you can do these [exercises](https://ocaml.org/exercises) and then go hunt for the syntax you need

- to read list. Still not sure if it's relevant or not, so I might remove them later or include above
    - Some university of waterloo professor is writing these interesting "flaneries", covering one specific topic in depth, often using functional languages (but he also has an intro to c)
        - coding in racket a program to construct/verify proof statements about programs https://cs.uwaterloo.ca/~plragde/flaneries/LACI/Introduction.html . Might or might not be relevant to this project
        - functional data structures, in ocaml, will vet and add above if it's good https://cs.uwaterloo.ca/~plragde/flaneries/FDS/
        - functional introduction to computer science. Altho it uses racket, this covers trees and lists which are more relevant to readysetboole, so might be better https://cs.uwaterloo.ca/~plragde/flaneries/FICS

- [ ] add more, remove unecessary words
