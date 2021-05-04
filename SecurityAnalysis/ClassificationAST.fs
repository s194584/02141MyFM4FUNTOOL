module ClassificationAST

type ClassificationAssignment = | ClassAss of ClassificationAssignment * ClassificationAssignment
                                | Class of string*string