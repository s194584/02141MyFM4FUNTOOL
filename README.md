<!---
# Key files
FM4FUN.fsx \
FM4FUNAST.fs \
FM4FUNLexer.fsl \
FM4FUNParser.fsp \
FM4FUNCompiler.fs \
FM4FUNInterpreter.fs \
UserInputAST.fs \
UserInputLexer.fsl \
UserInputParser.fsp \
-->
# Installing
(We have included the necessary files, so you can jump straight to "Running")\ 
To install you need to compile the Lexer and Parser.
## Lexer
To compile the lexer use the following command: \
{path-to "fslex.exe"} FM4FUNLexer.fsl --unicode
## Parser
To compile the parser use the following command: \
{path-to "fsyacc.exe"} FM4FUNParser.fsp --module FM4FUNParser

# Running
First update the path at the top of the FM4FUN.fsx file to match your path to the FsLexYacc.Runtime.dll \
To run the program, you need to run the FM4FUN.fsx like you normally would.
We used: \
dotnet fsi FM4FUN.fsx
<!---
# Using
When you have opened FM4FUN.fsx, you will be promted to enter a GCL-command.
Here you can copy-paste the command of your choice. \
The command can be inline or multi-line, if it is multi-line it CANNOT contain double newlines. \
Then you need to initialize the memory with every assignment seperated by ',' \
You will then have the option to select whether a program graph should be computed. 
   * Det for deterministic.
   * NonDet for non-deterministic. (Note: The execution will deterministic, but the output graph will be non-deterministic)  
   * No or everything else for no. 

After this you will get both the status of the execution and a Graphvis representation of the chosen type of program graph.\

There will be hinting at syntax error by showing the position of the lexbuffer, when the parsing fails.
-->