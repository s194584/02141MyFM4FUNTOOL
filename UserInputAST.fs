module UserInputAST

type ArrayElem = | Elem of int * ArrayElem 
                 | EMP
                 
type UserAssignment = 
    | UVar of string * int
    | UArr of string * ArrayElem
    | UAss of UserAssignment * UserAssignment
