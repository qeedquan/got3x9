package lex

type Token int

const (
	EOF Token = iota

	COMMENT
	SYMBOL
	INTEGER
	STRING

	ADD
	SUB
	MUL
	DIV
	AND
	OR
	XOR
	SEMI
	COMMA
	LPAREN
	RPAREN
	LBRACK
	RBRACK
	LBRACE
	RBRACE
	ADDROF
	UNOP
	COLON
	BYTEOP
	LNOT
	INV
	DISJ
	NEQ
	LT
	LE
	SHL
	SHR
	GT
	GE
	EQ
	ASSIGN
	COND
	CONJ

	CONST
	DO
	DECL
	ELSE
	END
	FOR
	HALT
	IF
	IE
	LEAVE
	LOOP
	MOD
	MODULE
	OBJECT
	PACKED
	RETURN
	STRUCT
	VAR
	WHILE
)

func (t Token) IsUnary() bool {
	switch t {
	case INV, LNOT, SUB:
		return true
	}
	return false
}

func (t Token) IsBinary() bool {
	switch t {
	case MOD, ADD, MUL, EQ, AND, OR, XOR, NEQ,
		LT, LE, SHL, GT, GE, SHR, SUB, DIV:
		return true
	}
	return false
}

func (t Token) Precedence() int {
	switch t {
	case MOD, MUL, DIV:
		return 7
	case ADD, SUB:
		return 6
	case AND, OR, XOR, SHL, SHR:
		return 5
	case LT, LE, GT, GE:
		return 4
	case EQ, NEQ:
		return 3
	case CONJ:
		return 2
	case DISJ:
		return 1
	}
	return 0
}
