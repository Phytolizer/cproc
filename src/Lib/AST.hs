{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Lib.AST (
  NodeKind (..),
  Node (..),
  ArrayDeclNode (..),
  ArrayRefNode (..),
  AssignmentNode (..),
  AlignasNode (..),
  BinaryOpNode (..),
  BreakNode (..),
  CaseNode (..),
  CastNode (..),
  CompoundNode (..),
  CompoundLiteralNode (..),
  ConstantNode (..),
  ContinueNode (..),
  DeclNode (..),
  DeclListNode (..),
  DefaultNode (..),
  DoWhileNode (..),
  EllipsisParamNode (..),
  EmptyStatementNode (..),
  EnumNode (..),
  EnumeratorNode (..),
  EnumeratorListNode (..),
  ExprListNode (..),
  ForNode (..),
  FuncCallNode (..),
  FuncDeclNode (..),
  FuncDefNode (..),
  GotoNode (..),
  IdNode (..),
  IdentifierTypeNode (..),
  IfNode (..),
  InitListNode (..),
  LabelNode (..),
  NamedInitializerNode (..),
  ParamListNode (..),
  PtrDeclNode (..),
  ReturnNode (..),
  StaticAssertNode (..),
  StructNode (..),
  StructRefType (..),
  StructRefNode (..),
  SwitchNode (..),
  TernaryOpNode (..),
  TranslationUnitNode (..),
  TypeDeclNode (..),
  TypedefNode (..),
  TypenameNode (..),
  UnaryOpNode (..),
  UnionNode (..),
  WhileNode (..),
  PragmaNode (..),
)
where

import Lib.Token (Token)

data NodeKind
  = NodeArrayDecl ArrayDeclNode
  | NodeArrayRef ArrayRefNode
  | NodeAssignment AssignmentNode
  | NodeAlignas AlignasNode
  | NodeBinaryOp BinaryOpNode
  | NodeBreak BreakNode
  | NodeCase CaseNode
  | NodeCast CastNode
  | NodeCompound CompoundNode
  | NodeCompoundLiteral CompoundLiteralNode
  | NodeConstant ConstantNode
  | NodeContinue ContinueNode
  | NodeDecl DeclNode
  | NodeDeclList DeclListNode
  | NodeDefault DefaultNode
  | NodeDoWhile DoWhileNode
  | NodeEllipsisParam EllipsisParamNode
  | NodeEmptyStatement EmptyStatementNode
  | NodeEnum EnumNode
  | NodeEnumerator EnumeratorNode
  | NodeEnumeratorList EnumeratorListNode
  | NodeExprList ExprListNode
  | NodeFor ForNode
  | NodeFuncCall FuncCallNode
  | NodeFuncDecl FuncDeclNode
  | NodeFuncDef FuncDefNode
  | NodeGoto GotoNode
  | NodeId IdNode
  | NodeIdentifierType IdentifierTypeNode
  | NodeIf IfNode
  | NodeInitList InitListNode
  | NodeLabel LabelNode
  | NodeNamedInitializer NamedInitializerNode
  | NodeParamList ParamListNode
  | NodePtrDecl PtrDeclNode
  | NodeReturn ReturnNode
  | NodeStaticAssert StaticAssertNode
  | NodeStruct StructNode
  | NodeStructRef StructRefNode
  | NodeSwitch SwitchNode
  | NodeTernaryOp TernaryOpNode
  | NodeTranslationUnit TranslationUnitNode
  | NodeTypeDecl TypeDeclNode
  | NodeTypedef TypedefNode
  | NodeTypename TypenameNode
  | NodeUnaryOp UnaryOpNode
  | NodeUnion UnionNode
  | NodeWhile WhileNode
  | NodePragma PragmaNode

data Node = Node
  { nodeKind :: NodeKind
  , nodeToken :: Token
  }

data ArrayDeclNode = ArrayDeclNode
  { arrayDeclType :: Node
  , arrayDeclDim :: Node
  , arrayDeclDimQuals :: String
  }

data ArrayRefNode = ArrayRefNode
  { arrayRefName :: Node
  , arrayRefSubscript :: Node
  }

data AssignmentNode = AssignmentNode
  { op :: String
  , assignmentLhs :: Node
  , assignmentRhs :: Node
  }

data AlignasNode = AlignasNode
  { alignasAlignment :: Node
  }

data BinaryOpNode = BinaryOpNode
  { binaryOpOp :: String
  , binaryOpLhs :: Node
  , binaryOpRhs :: Node
  }

data BreakNode = BreakNode

data CaseNode = CaseNode
  { caseExpr :: Node
  , caseStmts :: [Node]
  }

data CastNode = CastNode
  { castToType :: Node
  , castExpr :: Node
  }

data CompoundNode = CompoundNode
  { compoundBlockItems :: [Node]
  }

data CompoundLiteralNode = CompoundLiteralNode
  { compoundType :: Node
  , compoundInit :: Node
  }

data ConstantNode = ConstantNode
  { constantType :: String
  , constantValue :: String
  }

data ContinueNode = ContinueNode

data DeclNode = DeclNode
  { declName :: String
  , declQuals :: String
  , declAlign :: String
  , declStorage :: String
  , declFuncSpec :: String
  , declType :: Node
  , declInit :: Maybe Node
  , declBitSize :: Maybe Node
  }

data DeclListNode = DeclListNode
  { declListDecls :: [Node]
  }

data DefaultNode = DefaultNode
  { defaultStmts :: [Node]
  }

data DoWhileNode = DoWhileNode
  { doWhileCond :: Node
  , doWhileStmt :: Node
  }

data EllipsisParamNode = EllipsisParamNode

data EmptyStatementNode = EmptyStatementNode

data EnumNode = EnumNode
  { enumName :: Maybe String
  , enumValues :: Node
  }

data EnumeratorNode = EnumeratorNode
  { enumeratorName :: String
  , enumeratorValue :: Maybe Node
  }

data EnumeratorListNode = EnumeratorListNode
  { enumeratorListEnumerators :: [Node]
  }

data ExprListNode = ExprListNode
  { exprListExprs :: [Node]
  }

data ForNode = ForNode
  { forInit :: Maybe Node
  , forCond :: Maybe Node
  , forNext :: Maybe Node
  , forStmt :: Node
  }

data FuncCallNode = FuncCallNode
  { name :: Node
  , args :: Node
  }

data FuncDeclNode = FuncDeclNode
  { funcDeclArgs :: Node
  , funcDeclType :: Node
  }

data FuncDefNode = FuncDefNode
  { funcDefDecl :: Node
  , funcDefParamDecls :: Maybe [Node]
  -- ^ K&R style param decls
  , funcDefBody :: Node
  }

data GotoNode = GotoNode
  { gotoName :: String
  }

data IdNode = IdNode
  { idName :: String
  }

data IdentifierTypeNode = IdentifierTypeNode
  { identifierTypeNames :: String
  }

data IfNode = IfNode
  { ifCond :: Node
  , ifThen :: Node
  , ifElse :: Maybe Node
  }

data InitListNode = InitListNode
  { initListExprs :: [Node]
  }

data LabelNode = LabelNode
  { labelName :: String
  , labelStmt :: Node
  }

data NamedInitializerNode = NamedInitializerNode
  { namedInitializerName :: [Node]
  -- ^ Names can be hierarchical and contain constant expressions.
  , namedInitializerExpr :: Node
  }

data ParamListNode = ParamListNode
  { paramListParams :: [Node]
  }

data PtrDeclNode = PtrDeclNode
  { ptrDeclQuals :: String
  , ptrDeclType :: Node
  }

data ReturnNode = ReturnNode
  { returnExpr :: Maybe Node
  }

data StaticAssertNode = StaticAssertNode
  { staticAssertCond :: Node
  , staticAssertMsg :: Node
  }

data StructNode = StructNode
  { structName :: Maybe String
  , structDecls :: [Node]
  }

data StructRefType = StructRefTypeDot | StructRefTypeArrow

data StructRefNode = StructRefNode
  { structRefName :: Node
  , structRefType :: StructRefType
  , structRefField :: Node
  }

data SwitchNode = SwitchNode
  { switchExpr :: Node
  , switchStmt :: Node
  }

data TernaryOpNode = TernaryOpNode
  { ternaryOpCond :: Node
  , ternaryOpThen :: Node
  , ternaryOpElse :: Node
  }

data TranslationUnitNode = TranslationUnitNode
  { translationUnitExternalDecls :: [Node]
  }

data TypeDeclNode = TypeDeclNode
  { typeDeclName :: String
  , typeDeclQuals :: String
  , typeDeclAlign :: String
  , typeDeclType :: Node
  }

data TypedefNode = TypedefNode
  { typedefName :: String
  , typedefQuals :: String
  , typedefStorage :: String
  , typedefType :: Node
  }

data TypenameNode = TypenameNode
  { typenameName :: String
  , typenameQuals :: String
  , typenameAlign :: String
  , typenameType :: Node
  }

data UnaryOpNode = UnaryOpNode
  { unaryOpOp :: String
  , unaryOpExpr :: Node
  }

data UnionNode = UnionNode
  { unionName :: Maybe String
  , unionDecls :: [Node]
  }

data WhileNode = WhileNode
  { whileCond :: Node
  , whileStmt :: Node
  }

data PragmaNode = PragmaNode
  { pragmaName :: String
  }
