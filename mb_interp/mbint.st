#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f
"$Id: mbint.st,v 1.10 2020-11-09 23:34:52-08 - - $"

Object subclass: NumberStream [
   |atEndFlag stream buffer|
   NumberStream class >> new [
      ^ super new init: stdin.
   ]
   NumberStream class >> new: aStream [
      ^ super new init: aStream.
   ]
   init: aStream [
      atEndFlag := false.
      stream := aStream.
      buffer := OrderedCollection new.
   ]
   atEnd [ ^ atEndFlag ]
   nextNumber [
      [atEndFlag] whileFalse: [
         buffer size > 0 ifTrue: [
            ^ buffer removeFirst asNumber asFloatD.
         ].
         stdout flush.
         stderr flush.
         stream atEnd "This condition fills the input buffer."
            ifTrue: [atEndFlag := true]
            ifFalse: [buffer := stream nextLine substrings: ' ']
      ].
      ^ 0.0 / 0.0 "NaN".
   ]
].

Object subclass: Debug [
   level := 0. "multiple -d, -dd, -ddd options set debug level"
   Debug class >> incr [level := level + 1]
   Debug class >> > value [^ level > value]
].

Object subclass: MiniBasic [
   nl := Character nl.
   MiniBasic class >> unimplemented: object [
      |caller|
      caller := thisContext parentContext.
      stdout << object class << ' inherits '
             << caller method << ': ' << object << nl.
   ]
   prefix [^ '(', self class printString, ' ']
   assertionFailed: message [
      stderr << 'Assertion Failed: ' << message << nl.
      stderr << self << nl.
      ObjectMemory quit: 1.
   ]
].

MiniBasic subclass: Expr [ "abstract base class"
   nan := 0.0 / 0.0.
   printOn: file [^ file << self prefix << ')']
   value [MiniBasic unimplemented: self. ^ nan]

   "code from the TA notes"
   isArrayExpr [^ false]
].

Expr subclass: NumExpr [
   |value|
   NumExpr class >> new: val [^ super new init: val]
   init: val [value := val + 0.0d0]
   printOn: file [^ file << self prefix << value << ')']
   value [^ value]
].

Expr subclass: VarExpr [
   |variable|
   varDict := Dictionary from: {
      #e   -> 1.0 exp.
      #eof -> 0.0.
      #nan -> (0.0 / 0.0).
      #pi  -> -1.0 arcCos.
   }.
   "insert vaar value "
   VarExpr class >> insert: key with: value [
      varDict at: key put: value.
   ]
   VarExpr class >> new: var [^ super new init: var]
   init: var [variable := var]
   printOn: file [^ file << self prefix << variable << ')']
   put: val [varDict at: variable put: val]
   value [^ varDict at: variable ifAbsent: [0.0]]
   var   [^ variable]
].

Expr subclass: UnopExpr [
   |oper opnd|
   UnopExpr class >> new: op with: opn [
      ^ super new init: op init: opn.
   ]
   init: op init: opn [
      oper := op. opnd := opn.
   ]
   printOn: file [
      ^ file << self prefix << oper << ' ' << opnd << ')'.
   ]

   value [^ opnd value perform: oper]
].

Expr subclass: BinopExpr [
   |oper opnd1 opnd2|
   BinopExpr class >> new: op with: opn1 with: opn2 [
      ^ super new init: op with: opn1 with: opn2.
   ]
   init: op with: opn1 with: opn2 [
      oper := op. opnd1 := opn1. opnd2 := opn2.
   ]
   printOn: file [
      ^ file << self prefix << oper << ' ' << opnd1
             << ' ' << opnd2 << ')'.
   ]
   getArrayExpr [^ opnd1]
   getArrayIndex [^ opnd2]
   value [
       opnd1 isArrayExpr "Handle #( #aget #a #i)"
       ifTrue: [
           ^ opnd1 aget: (opnd2 value rounded) + 1 "opnd2 is the index"
       ]
       ifFalse: [
           ^ opnd1 value perform: oper with: opnd2 value
       ].
   ]
].

Expr subclass: ArrayExpr [
   |arrayName|
   |index|
   arrayDict := Dictionary new.
   ArrayExpr class >> new: var [^ super new init: var]
   "insert arr value "
   ArrayExpr class >> insert: key with: value [
      arrayDict at: key put: value.
   ]
   init: var [arrayName := var]
   printOn: file [^ file << self prefix << arrayName << ')']
   "array ref "
   isArrayExpr [^ true]
   aget: index [^ (arrayDict at: arrayName) at: index]
   aput: index with: val [(arrayDict at: arrayName) at: index put: val]
   addNewArray: newArr [ arrayDict at: arrayName put: newArr]
].

Expr extend [
   Expr class >> parse: expr [
      expr isNumber ifTrue: [^ NumExpr new: expr].
      expr isSymbol ifTrue: [^ VarExpr new: expr].
      expr isArray ifTrue: [
         expr size = 2 ifTrue: [
            ^ UnopExpr new: (expr at: 1)
                       with: (Expr parse: (expr at: 2)).
         ].
         expr size = 3 ifTrue: [
            ((expr at: 1) = #aget) | ((expr at: 1) = #aput)
            ifTrue: [
               ^ BinopExpr new: (expr at: 1)
                           with: (ArrayExpr new: (expr at: 2))
                           with: (Expr parse: (expr at: 3)).
            ]
            ifFalse: [
               ^ BinopExpr new: (expr at: 1)
                           with: (Expr parse: (expr at: 2))
                           with: (Expr parse: (expr at: 3)).
            ]
         ].
         self assertionFailed: 'Expr>>parse: isArray wrong size'.
      ].
      self assertionFailed: 'Expr>>parse: expr wrong type'.
   ]
].

MiniBasic subclass: Stmt [ "abstract base class"
   stmtNr := 1.
   Stmt class >> getStmtNr [^ stmtNr]
   Stmt class >> incrStmtNr [stmtNr := stmtNr + 1]
   labelDict := Dictionary new.

   "label table code "
   Stmt class >> insert: key with: value [
      labelDict at: key put: value.
   ]

   printOn: file [^ file << self prefix << ')']
   interp [MiniBasic unimplemented: self.]
].

Stmt subclass: DimStmt [
   |name size|
   DimStmt class >> new: stmt [^ super new init: stmt]
   init: stmt [
      name := ArrayExpr new: (stmt at: 2).  
      size := Expr parse: (stmt at: 3). 
   ]
   printOn: file [^ file << self prefix << name << ' ' << size << ')']
   interp [
      |newArr|
      newArr := Array new: (size value rounded) withAll: 0.0d0.
      name addNewArray: newArr.
   ]
].

Stmt subclass: LetStmt [
   |name expr|
   LetStmt class >> new: stmt [^ super new init: stmt]
   init: stmt [
      name := Expr parse: (stmt at: 2).
      expr := Expr parse: (stmt at: 3). 
   ]
   printOn: file [^ file << self prefix << name << ' ' << expr << ')']
   interp[
      name class = BinopExpr 
         ifTrue: [
            |arrExpr index|
            arrExpr := name getArrayExpr.
            index := name getArrayIndex.
            arrExpr aput: (index value rounded) + 1 with: (expr value)
         ]
         ifFalse: [VarExpr insert: (name var) with: (expr value)]
   ]
].

Stmt subclass: GotoStmt [
   |label|
   GotoStmt class >> new: stmt [^ super new init: stmt]
   init: stmt [label := stmt at: 2.]
   printOn: file [^ file << self prefix << label << ')']
   interp [
      stmtNr := labelDict at: label.
      stmtNr.
   ]
].

Stmt subclass: IfStmt [
   |expr label|
   IfStmt class >> new: stmt [^ super new init: stmt]
   init: stmt [
      expr := Expr parse: (stmt at: 2).
      label := stmt at: 3.
   ]
   printOn: file [^ file << self prefix << expr << ' ' << label << ')']
   interp [
      expr value ifTrue:[stmtNr := labelDict at: label.].
      stmtNr.
   ]
].

Stmt subclass: InputStmt [
   |inputVars|
   stdinNumbers := NumberStream new.
   InputStmt class >> new: stmt [^ super new init: stmt]
   init: stmt [
      |args|
      args := stmt at: 2.
      inputVars := Array new: args size.
      1 to: args size do: [:index|
         inputVars at: index put: (Expr parse: (args at: index)).
      ]
   ]
   printOn: file [^ file << self prefix << inputVars << ')']
   interp [
      inputVars do: [:var|
         |value|
         value := stdinNumbers nextNumber.
         stdinNumbers atEnd
            ifTrue:  [VarExpr insert: #eof with: 1.0]
            ifFalse: [VarExpr insert: (var var) with: value].
         stdout << thisContext method << ' '
                << (var var) << ': ' << value << nl.
      ].
   ]
].

Stmt subclass: PrintStmt [
   |printables|
   PrintStmt class >> new: stmt [^ super new init: stmt]
   init: stmt [
      |args|
      args := stmt at: 2.
      printables := Array new: args size.
      1 to: args size do: [:index|
         |arg|
         arg := args at: index.
         arg isString ifFalse: [ arg := Expr parse: arg].
         printables at: index put: arg.
      ]
   ]
   printOn: file [^ file << self prefix << printables << ')']
   interp [
      Debug > 2 ifTrue: [printables inspect].
      printables do: [:obj|
         obj isString ifTrue: [stdout << obj]
                      ifFalse: [stdout << ' ' << obj value].
      ].
      stdout << nl.
   ]
].

Stmt subclass: NullStmt [
   NullStmt class >> new: stmt [^ super new]
   printOn: file [^ file << self prefix << ')']
   interp []
].

Stmt extend [
   stmtDict := Dictionary from: {
      #dim   -> DimStmt.
      #let   -> LetStmt.
      #goto  -> GotoStmt.
      #if    -> IfStmt.
      #input -> InputStmt.
      #print -> PrintStmt.
      #null  -> NullStmt.
   }.
   Stmt class >> parse: stmt [
      |stmtSym stmtClass|
      stmt size = 0 ifTrue: [stmtSym := #null]
                    ifFalse: [stmtSym := stmt at: 1].
      stmtClass := stmtDict at: stmtSym.
      ^ stmtClass new: stmt.
   ]
 ]

MiniBasic subclass: Interpreter [
   |statements|
   Interpreter class >> new: program [^ super new init: program]
   init: program [
      statements := Array new: program size.
      1 to: program size do: [:index|
         |label stmt parsed|
         stmt := (program at: index) at: 3.
         label := (program at: index) at: 2.
         label isNil ifFalse: [Stmt insert: label with: index].

         statements at: index put: (Stmt parse: stmt).
      ].
   ]

   print [
      stdout << 'Interpreter statements: [' << nl.
      1 to: statements size do: [:index|
         stdout << '   ' << index << ': '
                << (statements at: index) << nl.
      ].
      stdout << '].' << nl.
   ]

   interpret [
      |stmtNr|
      [stmtNr := Stmt getStmtNr. stmtNr <= statements size]
      whileTrue: [
         |stmtObj|
         Stmt incrStmtNr.
         stmtObj := statements at: stmtNr.
         Debug > 1 ifTrue: [stdout << stmtNr << ': ' << stmtObj << nl].
         stmtObj interp.
      ]
   ]
].

Object subclass: Main [
   nl := Character nl.
   scriptName := thisContext currentFileName.

   Main class >> usage [
      stderr << 'Usage: ' << scriptName << ' [-d] fileName' << nl.
      ObjectMemory quit: 1.
   ]

   Main class >> print: program [
      stdout << 'Main program: [' << nl.
      1 to: program size do: [:index|
         stdout << '   ' << index << ': ' << (program at: index) << nl.
      ].
      stdout << '].' << nl.
   ]

   Main class >> getopts [
      |fileName|
      Smalltalk arguments: '-d' do: [:opt :arg|
         opt = $d ifTrue: [Debug incr].
         opt = nil ifTrue: [
            fileName isNil ifFalse: [Main usage value].
            fileName := arg.
         ].
      ] ifError: [Main usage value].
      Debug > 0 ifTrue: [
         stdout << scriptName << ': ' << Smalltalk arguments << nl.
      ].
      ^ fileName.
   ]

   Main class >> main [
      |fileName|
      fileName := Main getopts.
      fileName isNil ifTrue: [Main usage value].
      [
         |program interpreter|
         FileStream fileIn: fileName.
         program := Program get.
         Debug > 0 ifTrue: [Main print: program].
         interpreter := Interpreter new: program.
         Debug > 0 ifTrue: [interpreter print].


         interpreter interpret.
      ] on: SystemExceptions.FileError do: [:signal|
         stderr << scriptName << ': ' << fileName << ': '
                << signal messageText << nl.
         ObjectMemory quit: 1.
      ].
   ]
].

Main main.
