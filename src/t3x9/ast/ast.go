package ast

import (
	"text/scanner"

	"t3x9/lex"
)

type Type int

const (
	CONST Type = 1 << iota
	VAR
	VECT
	BYTE
	DECL
	STRUCT
	FUNC
	FNARG
)

type Node interface{}

type Decl interface {
	Node
}

type Expr interface {
	Node
}

type Stmt interface {
	Node
}

type Comment struct {
	Pos  scanner.Position
	Text string
}

type Scope struct {
	Parent *Scope
	Symbol []*Symbol
}

type Prog struct {
	Scope   *Scope
	Comment []*Comment
	Decl    []Decl
	Stmt    *CompoundStmt

	Defs map[*Symbol]Decl
	Uses map[Expr]*Symbol
}

type Symbol struct {
	Pos   scanner.Position
	Type  Type
	Len   int
	Ident string
	Value Expr
}

type PackageDecl struct {
	Name    *Symbol
	Version *Symbol
}

type ModuleDecl PackageDecl
type ObjectDecl PackageDecl

type GenDecl struct {
	TypePos scanner.Position
	Type    lex.Token
	Decl    []Decl
}

type VarDecl struct {
	Name  *Symbol
	Type  Expr
	Value Expr
}

type ConstDecl struct {
	Name   *Symbol
	X      Expr
	Result int
}

type StructDecl struct {
	Name  *Symbol
	Field []*Symbol
}

type FwdDecl struct {
	Name  *Symbol
	X     *ConstExpr
	Arity int
}

type FuncDecl struct {
	Fwd    *FwdDecl
	Scope  *Scope
	Name   *Symbol
	Lparen scanner.Position
	Arg    []*Symbol
	Rparen scanner.Position
	Body   Stmt
}

type ArrayType struct {
	Byte bool
	X    *ConstExpr
	Len  int
}

type CompoundStmt struct {
	Scope *Scope
	Decl  []*GenDecl
	Do    scanner.Position
	Stmt  []Stmt
	End   scanner.Position
}

type AssignStmt struct {
	Name *Symbol
	Lhs  Expr
	Rhs  Expr
}

type ForStmt struct {
	For    scanner.Position
	Lparen scanner.Position
	Name   *Symbol
	Init   Expr
	Cond   Expr
	Post   *ConstExpr
	Rparen scanner.Position
	Body   Stmt
}

type IfStmt struct {
	IfPos  scanner.Position
	If     lex.Token
	Lparen scanner.Position
	Cond   Expr
	Rparen scanner.Position
	Body   Stmt
	Else   Stmt
}

type BranchStmt struct {
	TokPos scanner.Position
	Tok    lex.Token
}

type ReturnStmt struct {
	TokPos scanner.Position
	Tok    lex.Token
	Value  Expr
}

type WhileStmt struct {
	While  scanner.Position
	Lparen scanner.Position
	Cond   Expr
	Rparen scanner.Position
	Body   Stmt
}

type EmptyStmt struct {
	Semi scanner.Position
}

type ExprStmt struct {
	X Expr
}

type IndexExpr struct {
	Byte   bool
	X      Expr
	Lbrack scanner.Position
	Index  Expr
	Rbrack scanner.Position
}

type TableExpr struct {
	Lbrack scanner.Position
	Member []Expr
	Rbrack scanner.Position
}

type ByteVecExpr struct {
	Lbrack scanner.Position
	Data   []int
	Rbrack scanner.Position
}

type UnaryExpr struct {
	OpPos scanner.Position
	Op    lex.Token
	X     Expr
}

type BinaryExpr struct {
	X     Expr
	OpPos scanner.Position
	Op    lex.Token
	Y     Expr
}

type LogicalExpr struct {
	X     Expr
	Depth int
}

type ConstExpr struct {
	X     Expr
	Value int
}

type ParenExpr struct {
	Lparen scanner.Position
	X      Expr
	Rparen scanner.Position
}

type CallExpr struct {
	Name   *Symbol
	Fun    Expr
	Lparen scanner.Position
	Arg    []Expr
	Rparen scanner.Position
}

type CondExpr struct {
	Op scanner.Position
	X  Expr
	Y  Expr
	Z  Expr
}

type BasicLit struct {
	Pos   scanner.Position
	Type  lex.Token
	Value string
}
