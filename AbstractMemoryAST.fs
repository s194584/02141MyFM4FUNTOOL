module AbstractMemoryAST

type Sign = P | M | Z
type Bool = TT | FF
type AbstractMemory = Map<string, Sign> * Map<string, Set<Sign>> // variable memory, array memory
type AnalysisAssignment = Map<Node, Set<AbstractMemory>>

type AbstractArrayElem = | AElem of Sign * AbstractArrayElem
                         | AEMP
                 
type AbstractAssignment =
    | AVar of string * Sign
    | AArr of string * AbstractArrayElem
    | AAss of AbstractAssignment * AbstractAssignment

type AbstractExpression =
    | Abs of AbstractExpression * AbstractExpression
    | AbsE of AbstractAssignment