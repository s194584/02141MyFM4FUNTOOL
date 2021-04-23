module LatticeAST

type LatticeAssignment = | LatAss of LatticeAssignment * LatticeAssignment
                         | LatRel of string * string