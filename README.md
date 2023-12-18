This program runs a recursive algorithm to determine both a perfectly optimal move with no
restricted depth, and an estimated optimal move based on a restricted depth. 

To run this program, simply make the function, then use the executable ./halloqueen with your flags.
To find info on your flags, use the 'help' flag -h or --help

# Project Grade:         104/100
## Functionality               77/73
* Game mechanics:              20
* Exact game solver:           15
* Cut-off depth solver:        13
* Evaluation function:         2
* Avoiding unnecessary work:   3
  * Yup! Still not entirely comfy with the "kings as poitns" thing, but it's hard to argue.
* Command-line interface:      10
  * Move is not output in the format it's expected to be read in, and reading requries quotes
    because you have spaces instead of comma. Such a quibble.
* Move and verbose flags:      5
  * You are a bit too verbose without -v, but otherwise exceleltn
  * Beautiful interactive mode. (+5)
* Error-handling:              5
  *readMove should probably check that the piece specified is the color of the current player.
  Especially since makeUnsafeMove exists.

## Design                      27/27
* Well-designed data types:    8
* Well-decomposed functions:   10
  * Hard to find something to quible about
  * allNextGame/gameMoveAssociation are too similar to not have similar names, and honestly both
    could be inlined. allNextGame is never used, and 
    `[(makeUnSafeMove game move, move) | move <- allLegalMoves game]` occuring twice is probably just fine.
* Good module decomposition:   2
* Good variable names:         2
* Efficient/idiomatic code:    5
  * Some cases that could be pattern matching on the input.

