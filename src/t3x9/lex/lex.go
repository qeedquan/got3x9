package lex

import (
	"bufio"
	"fmt"
	"io"
	"strings"
	"text/scanner"
	"unicode"
)

type Lexer struct {
	pos0      scanner.Position
	pos       scanner.Position
	b         *bufio.Reader
	errh      func(scanner.Position, error)
	NumErrors int
}

func (l *Lexer) Init(name string, r io.Reader, errh func(scanner.Position, error)) {
	l.pos = scanner.Position{
		Filename: name,
		Line:     1,
		Column:   1,
	}
	l.pos0 = l.pos
	l.b = bufio.NewReader(r)
	l.errh = errh
	l.NumErrors = 0
}

func (l *Lexer) errf(pos scanner.Position, format string, args ...interface{}) {
	l.NumErrors++
	if l.errh != nil {
		l.errh(pos, fmt.Errorf(format, args...))
	}
}

func (l *Lexer) ungetr() {
	l.b.UnreadRune()
	l.pos = l.pos0
}

func (l *Lexer) getr() rune {
	l.pos0 = l.pos

	r, w, err := l.b.ReadRune()
	if err != nil {
		if err != io.EOF {
			l.errf(l.pos0, "I/O error: %v", err)
		}
		return -1
	}

	l.pos.Offset += w
	l.pos.Column += w
	if r == '\n' {
		l.pos.Line++
		l.pos.Column = 1
	}
	return r
}

func (l *Lexer) whitespace() rune {
	var r rune
	for {
		r = l.getr()
		if !unicode.IsSpace(r) {
			break
		}
	}
	return r
}

func (l *Lexer) comment(r rune) (tok Token, lit string) {
	tok = COMMENT
	for {
		r = l.getr()
		if r == '\n' {
			break
		}
		lit += string(r)
	}
	return
}

func (l *Lexer) number(r rune) (tok Token, lit string, err error) {
	tok = INTEGER
	lit = string(r)

	if r == '%' {
		r = l.getr()
		if !unicode.IsDigit(r) {
			l.ungetr()
			tok = SUB
			return
		}
		lit += string(r)
	}

	hex := false
	if r == '0' {
		r = l.getr()
		if r == 'x' || r == 'X' {
			hex = true
			lit += string(r)
			r = l.getr()
			if !isDigit(r, hex) {
				l.ungetr()
				err = fmt.Errorf("missing digits after %v", lit)
				return
			}
		}
		l.ungetr()
	}

	for {
		r = l.getr()
		if !isDigit(r, hex) {
			break
		}
		lit += string(r)
	}
	l.ungetr()

	return
}

func isDigit(r rune, hex bool) bool {
	switch {
	case '0' <= r && r <= '9':
		return true
	case hex && (('a' <= r && r <= 'f') || ('A' <= r && r <= 'F')):
		return true
	}
	return false
}

func isSymbol(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_' || r == '.'
}

func (l *Lexer) symbol(r rune) (tok Token, lit string) {
	tok = SYMBOL
	for isSymbol(r) {
		lit += string(r)
		r = l.getr()
	}
	l.ungetr()

	switch strings.ToLower(lit) {
	case "const":
		tok = CONST
	case "decl":
		tok = DECL
	case "do":
		tok = DO
	case "end":
		tok = END
	case "else":
		tok = ELSE
	case "for":
		tok = FOR
	case "halt":
		tok = HALT
	case "ie":
		tok = IE
	case "if":
		tok = IF
	case "leave":
		tok = LEAVE
	case "loop":
		tok = LOOP
	case "mod":
		tok = MOD
	case "module":
		tok = MODULE
	case "object":
		tok = OBJECT
	case "packed":
		tok = PACKED
	case "return":
		tok = RETURN
	case "struct":
		tok = STRUCT
	case "var":
		tok = VAR
	case "while":
		tok = WHILE
	}
	return
}

func (l *Lexer) readec() rune {
	r := l.getr()
	if r != '\\' {
		return r
	}
	r = l.getr()
	switch r {
	case 'a':
		r = '\a'
	case 'b':
		r = '\b'
	case 'e':
		r = '\033'
	case 'f':
		r = '\f'
	case 'n':
		r = '\n'
	case 'q':
		r = '"' | 0x100
	case 'r':
		r = '\r'
	case 's':
		r = ' '
	case 't':
		r = '\t'
	case 'v':
		r = '\v'
	}
	return r
}

func (l *Lexer) char(r rune) (tok Token, lit string, err error) {
	tok = INTEGER
	lit = fmt.Sprintf("%d", l.readec())
	if l.readec() != '\'' {
		err = fmt.Errorf("missing ''' in character")
	}
	return
}

func (l *Lexer) string(r rune) (tok Token, lit string, err error) {
	tok = STRING
	lit = string(r)
	for {
		r := l.readec()
		lit += string(r)
		if r == '"' {
			break
		}
		if r == -1 {
			err = fmt.Errorf("unterminated string")
			break
		}
	}
	return
}

func (l *Lexer) sym3(r1 rune, tok1 Token, r2 rune, tok2 Token, r3 rune, tok3 Token) (tok Token, lit string) {
	tok = tok1
	lit = string(r1)
	switch l.getr() {
	case r2:
		tok = tok2
		lit += string(r2)
	case r3:
		tok = tok3
		lit += string(r3)
	default:
		l.ungetr()
	}
	return
}

func (l *Lexer) Next() (pos scanner.Position, tok Token, lit string) {
	var err error

redo:
	if err != nil {
		l.errf(pos, "%v", err)
		err = nil
	}
	r := l.whitespace()
	pos = l.pos0

	if r == '!' {
		tok, lit = l.comment(r)
		return
	}
	if unicode.IsDigit(r) || r == '%' {
		tok, lit, err = l.number(r)
		if err != nil {
			goto redo
		}
		return
	}
	if isSymbol(r) {
		tok, lit = l.symbol(r)
		return
	}
	if r == '\'' {
		tok, lit, err = l.char(r)
		if err != nil {
			goto redo
		}
		return
	}
	if r == '"' {
		tok, lit, err = l.string(r)
		if err != nil {
			goto redo
		}
		return
	}

	lit = string(r)
	switch r {
	case -1:
		tok = EOF
	case '+':
		tok = ADD
	case '-':
		tok, lit = l.sym3(r, SUB, '>', COND, '>', COND)
	case '*':
		tok = MUL
	case '/':
		tok, lit = l.sym3(r, DIV, '\\', CONJ, '\\', CONJ)
	case '(':
		tok = LPAREN
	case ')':
		tok = RPAREN
	case '[':
		tok = LBRACK
	case ']':
		tok = RBRACK
	case '{':
		tok = LBRACE
	case '}':
		tok = RBRACE
	case ';':
		tok = SEMI
	case ',':
		tok = COMMA
	case '=':
		tok = EQ
	case '~':
		tok = INV
	case '@':
		tok = ADDROF
	case '\\':
		tok, lit = l.sym3(r, LNOT, '/', DISJ, '=', NEQ)
	case '<':
		tok, lit = l.sym3(r, LT, '<', SHL, '=', LE)
	case '>':
		tok, lit = l.sym3(r, GT, '>', SHR, '=', GE)
	case ':':
		tok, lit = l.sym3(r, COLON, ':', BYTEOP, '=', ASSIGN)
	case '&':
		tok = AND
	case '|':
		tok = OR
	case '^':
		tok = XOR
	default:
		err = fmt.Errorf("unrecognized character %q", r)
		goto redo
	}
	return
}
