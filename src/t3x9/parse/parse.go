package parse

import (
	"fmt"
	"strconv"
	"strings"
	"text/scanner"

	"t3x9/ast"
	"t3x9/lex"
)

type Mode uint64

const (
	DeclarationErrors Mode = 1 << iota // report declaration errors
)

type Parser struct {
	lex  *lex.Lexer
	errh func(scanner.Position, error)
	mode Mode

	pos scanner.Position
	tok lex.Token
	lit string

	comment   *[]*ast.Comment
	calls     []*ast.CallExpr
	scope     *ast.Scope
	defs      map[*ast.Symbol]ast.Decl
	uses      map[ast.Expr]*ast.Symbol
	infun     bool
	inloop    bool
	NumErrors int
}

func (p *Parser) Init(lex *lex.Lexer, errh func(scanner.Position, error), mode Mode) {
	p.lex = lex
	p.errh = errh
	p.mode = mode
	p.NumErrors = 0
}

func (p *Parser) errf(format string, args ...interface{}) {
	p.NumErrors++
	if p.errh != nil {
		p.errh(p.pos, fmt.Errorf(format, args...))
	}
}

func (p *Parser) declerrf(format string, args ...interface{}) {
	if p.mode&DeclarationErrors != 0 {
		p.errf(format, args...)
	}
}

func (p *Parser) next() (scanner.Position, lex.Token) {
scan:
	pos, tok := p.pos, p.tok
	p.pos, p.tok, p.lit = p.lex.Next()
	if p.tok == lex.COMMENT {
		*p.comment = append(*p.comment, &ast.Comment{p.pos, p.lit})
		goto scan
	}
	return pos, tok
}

func (p *Parser) expect(t lex.Token) (scanner.Position, lex.Token) {
	if p.tok != t {
		p.errf("expected %v, got %v", t, p.tok)
	}
	return p.next()
}

func (p *Parser) sync(typ int) {
	for p.tok != lex.EOF {
		switch typ {
		case 's':
			switch p.tok {
			case lex.LOOP, lex.IF, lex.DO, lex.WHILE,
				lex.HALT, lex.RETURN:
				return
			}
		case 'e':
			switch p.tok {
			case lex.END:
				return
			}
		}
		p.next()
	}
}

func (p *Parser) symbol() *ast.Symbol {
	lit := p.lit
	pos, _ := p.expect(lex.SYMBOL)
	return &ast.Symbol{
		Pos:   pos,
		Ident: lit,
	}
}

func (p *Parser) find(s string) (*ast.Scope, *ast.Symbol) {
	s = strings.ToLower(s)
	lp := p.scope
	for lp != nil {
		for i := len(lp.Symbol) - 1; i >= 0; i-- {
			if s == strings.ToLower(lp.Symbol[i].Ident) {
				return lp, lp.Symbol[i]
			}
		}
		lp = lp.Parent
	}
	return nil, nil
}

func (p *Parser) lookup(s *ast.Symbol, f ast.Type) *ast.Symbol {
	_, y := p.find(s.Ident)
	if y == nil {
		p.declerrf("%s: undefined", s.Ident)
		return nil
	}
	if y.Type&f != f {
		p.declerrf("%s: unexpected type", s.Ident)
	}
	return y
}

func (p *Parser) add(s *ast.Symbol, f ast.Type) {
	u, y := p.find(s.Ident)
	if u != nil && ((u == p.scope && u.Parent == nil) || u.Parent != nil) {
		if !(y.Type&ast.DECL != 0 && f&ast.FUNC != 0) {
			p.declerrf("%s: redefined (declared at %v)", s.Ident, y.Pos)
			return
		}
	}

	s.Type = f
	p.scope.Symbol = append(p.scope.Symbol, s)
}

func (p *Parser) Parse() *ast.Prog {
	return p.program()
}

func (p *Parser) program() *ast.Prog {
	pg := &ast.Prog{}
	pg.Defs = make(map[*ast.Symbol]ast.Decl)
	pg.Uses = make(map[ast.Expr]*ast.Symbol)
	pg.Scope = &ast.Scope{}

	p.scope = pg.Scope
	p.defs = pg.Defs
	p.uses = pg.Uses
	p.comment = &pg.Comment

	p.add(&ast.Symbol{Ident: "t.memcomp", Len: 3, Value: &ast.ConstExpr{Value: 3}}, ast.FUNC)
	p.add(&ast.Symbol{Ident: "t.memcopy", Len: 3, Value: &ast.ConstExpr{Value: 3}}, ast.FUNC)
	p.add(&ast.Symbol{Ident: "t.memfill", Len: 3, Value: &ast.ConstExpr{Value: 3}}, ast.FUNC)
	p.add(&ast.Symbol{Ident: "t.memscan", Len: 3, Value: &ast.ConstExpr{Value: 3}}, ast.FUNC)
	p.add(&ast.Symbol{Ident: "t.create", Len: 1, Value: &ast.ConstExpr{Value: 1}}, ast.FUNC)
	p.add(&ast.Symbol{Ident: "t.open", Len: 2, Value: &ast.ConstExpr{Value: 2}}, ast.FUNC)
	p.add(&ast.Symbol{Ident: "t.close", Len: 1, Value: &ast.ConstExpr{Value: 1}}, ast.FUNC)
	p.add(&ast.Symbol{Ident: "t.read", Len: 3, Value: &ast.ConstExpr{Value: 3}}, ast.FUNC)
	p.add(&ast.Symbol{Ident: "t.write", Len: 3, Value: &ast.ConstExpr{Value: 3}}, ast.FUNC)
	p.add(&ast.Symbol{Ident: "t.rename", Len: 2, Value: &ast.ConstExpr{Value: 2}}, ast.FUNC)
	p.add(&ast.Symbol{Ident: "t.remove", Len: 1, Value: &ast.ConstExpr{Value: 1}}, ast.FUNC)

	p.next()
	if p.tok == lex.MODULE {
		pg.Decl = append(pg.Decl, p.moduledecl())
	}
	if p.tok == lex.OBJECT {
		pg.Decl = append(pg.Decl, p.objectdecl())
	}
	for p.tok == lex.VAR || p.tok == lex.CONST || p.tok == lex.SYMBOL || p.tok == lex.DECL || p.tok == lex.STRUCT {
		pg.Decl = append(pg.Decl, p.declaration())
	}
	if p.tok != lex.DO {
		p.errf("DO or declaration expected, got %v", p.tok)
		return nil
	}
	pg.Stmt = p.compound(true)

	for _, c := range p.calls {
		_, y := p.find(c.Name.Ident)
		if y == nil {
			p.declerrf("%v: undefined function", c.Name.Ident)
		}
	}

	return pg
}

func (p *Parser) evalconst(e ast.Expr) int {
	r := 0
	switch e := e.(type) {
	case *ast.ConstExpr:
		r = e.Value
	case *ast.BinaryExpr:
		x := p.evalconst(e.X)
		y := p.evalconst(e.Y)
		switch e.Op {
		case lex.ADD:
			r = x + y
		case lex.SUB:
			r = x - y
		case lex.MUL:
			r = x * y
		case lex.DIV:
			r = x / y
		case lex.MOD:
			r = x % y
		default:
			panic(fmt.Errorf("unsupported op %v", e.Op))
		}
	case *ast.UnaryExpr:
		x := p.evalconst(e.X)
		switch e.Op {
		case lex.SUB:
			r = -x
		default:
			panic(fmt.Errorf("unsupported op %v", e.Op))
		}
	case *ast.BasicLit:
		switch e.Type {
		case lex.INTEGER:
			v := e.Value
			if strings.HasPrefix(v, "%") {
				v = "-" + v[1:]
			}
			ri, _ := strconv.ParseInt(v, 0, 32)
			r = int(ri)
		case lex.SYMBOL:
			ri, _ := strconv.ParseInt(e.Value, 0, 32)
			r = int(ri)
		default:
			panic(fmt.Errorf("unsupported type %#v", e))
		}
	}

	return r
}

func (p *Parser) constfac() *ast.BasicLit {
	n := &ast.BasicLit{}
	switch p.tok {
	case lex.INTEGER:
		n.Value = p.lit
		n.Pos, n.Type = p.next()

	case lex.SYMBOL:
		y := p.symbol()
		yd := p.lookup(y, ast.CONST)
		n.Pos, n.Type = y.Pos, lex.SYMBOL
		if yd != nil {
			if ce, ok := yd.Value.(*ast.ConstExpr); ok {
				n.Value = fmt.Sprint(ce.Value)
			}
		}

	default:
		p.errf("constant value expected, got %v", p.tok)
	}
	return n
}

func (p *Parser) constval() *ast.ConstExpr {
	n := ast.Expr(p.constfac())
	switch p.tok {
	case lex.MUL, lex.ADD:
		e := &ast.BinaryExpr{}
		e.OpPos, e.Op = p.next()
		e.X = n
		e.Y = p.constfac()
		n = e
	}
	return &ast.ConstExpr{
		X:     n,
		Value: p.evalconst(n),
	}
}

func (p *Parser) moduledecl() *ast.ModuleDecl {
	d := &ast.ModuleDecl{}
	p.next()
	d.Name = p.symbol()
	p.expect(lex.LPAREN)
	d.Version = p.symbol()
	p.expect(lex.RPAREN)
	p.expect(lex.SEMI)
	return d
}

func (p *Parser) objectdecl() *ast.ObjectDecl {
	d := &ast.ObjectDecl{}
	p.next()
	d.Name = p.symbol()
	p.expect(lex.LBRACK)
	d.Version = p.symbol()
	p.expect(lex.RBRACK)
	p.expect(lex.SEMI)
	return d
}

func (p *Parser) declaration() ast.Decl {
	switch p.tok {
	case lex.VAR:
		return p.vardecl()
	case lex.CONST:
		return p.constdecl()
	case lex.STRUCT:
		return p.stcdecl()
	case lex.DECL:
		return p.fwddecl()
	default:
		return p.funcdecl()
	}
}

func (p *Parser) vardecl() *ast.GenDecl {
	d := &ast.GenDecl{}
	d.TypePos, d.Type = p.next()
	for {
		n := &ast.VarDecl{}
		n.Name = p.symbol()
		p.add(n.Name, ast.VAR)
		p.defs[n.Name] = n

		if p.tok == lex.LBRACK {
			n.Name.Type |= ast.VECT
			a := &ast.ArrayType{}
			p.next()
			a.X = p.constval()
			a.Len = a.X.Value
			p.expect(lex.RBRACK)
			n.Type = a
			n.Name.Len = a.Len
		} else if p.tok == lex.BYTEOP {
			n.Name.Type |= ast.VECT | ast.BYTE
			a := &ast.ArrayType{}
			a.Byte = true
			p.next()
			a.X = p.constval()
			a.Len = a.X.Value
			n.Type = a
			n.Name.Len = a.Len
		}
		d.Decl = append(d.Decl, n)

		if p.tok != lex.COMMA {
			break
		}
		p.next()
	}
	p.expect(lex.SEMI)
	return d
}

func (p *Parser) constdecl() *ast.GenDecl {
	d := &ast.GenDecl{}
	d.TypePos, d.Type = p.next()
	for {
		n := &ast.ConstDecl{}
		n.Name = p.symbol()
		p.expect(lex.EQ)
		n.Name.Value = p.constval()
		d.Decl = append(d.Decl, n)
		p.add(n.Name, ast.CONST)
		p.defs[n.Name] = n
		if p.tok != lex.COMMA {
			break
		}
		p.next()
	}
	p.expect(lex.SEMI)
	return d
}

func (p *Parser) stcdecl() *ast.GenDecl {
	d := &ast.GenDecl{}
	d.TypePos, d.Type = p.next()
	n := &ast.StructDecl{}
	n.Name = p.symbol()
	p.add(n.Name, ast.CONST)
	p.defs[n.Name] = n
	p.expect(lex.EQ)
	for i := 0; ; i++ {
		f := p.symbol()
		f.Value = &ast.ConstExpr{Value: i}
		p.add(f, ast.CONST)
		p.defs[n.Name] = n
		n.Field = append(n.Field, f)
		if p.tok != lex.COMMA {
			break
		}
		p.next()
	}
	n.Name.Value = &ast.ConstExpr{Value: len(n.Field)}
	n.Name.Len = len(n.Field)
	d.Decl = append(d.Decl, n)
	p.expect(lex.SEMI)
	return d
}

func (p *Parser) fwddecl() *ast.GenDecl {
	d := &ast.GenDecl{}
	d.TypePos, d.Type = p.next()
	for {
		n := &ast.FwdDecl{}
		n.Name = p.symbol()
		p.add(n.Name, ast.DECL)
		p.defs[n.Name] = n
		p.expect(lex.LPAREN)
		n.X = p.constval()
		n.Arity = p.evalconst(n.X)
		if n.Arity < 0 {
			p.declerrf("invalid arity %q", n.Arity)
		}
		n.Name.Len = n.Arity
		p.expect(lex.RPAREN)
		d.Decl = append(d.Decl, n)
		if p.tok != lex.COMMA {
			break
		}
		p.next()
	}
	p.expect(lex.SEMI)
	return d
}

func (p *Parser) funcdecl() *ast.FuncDecl {
	d := &ast.FuncDecl{}
	d.Name = p.symbol()
	_, fwd := p.find(d.Name.Ident)
	p.add(d.Name, ast.FUNC)
	p.defs[d.Name] = d
	d.Lparen, _ = p.expect(lex.LPAREN)

	d.Scope = &ast.Scope{Parent: p.scope}
	p.scope = d.Scope
	for p.tok == lex.SYMBOL {
		y := p.symbol()
		p.add(y, ast.FNARG)
		d.Arg = append(d.Arg, y)
		if p.tok != lex.COMMA {
			break
		}
		p.next()
	}
	d.Rparen, _ = p.expect(lex.RPAREN)
	d.Name.Len = len(d.Arg)

	if fwd != nil {
		d.Fwd = p.defs[fwd].(*ast.FwdDecl)
		if fwd.Type == ast.DECL && fwd.Len != len(d.Arg) {
			p.errf("%v: redefinition with a different type", d.Name.Ident)
		}
	}

	p.infun = true
	d.Body = p.stmt()
	p.infun = false
	p.scope = p.scope.Parent
	return d
}

func (p *Parser) compound(glob bool) *ast.CompoundStmt {
	n := &ast.CompoundStmt{}
	n.Do, _ = p.expect(lex.DO)

	n.Scope = &ast.Scope{Parent: p.scope}
	p.scope = n.Scope

	for p.tok == lex.VAR || p.tok == lex.CONST || p.tok == lex.STRUCT {
		n.Decl = append(n.Decl, p.declaration().(*ast.GenDecl))
	}

	for p.tok != lex.END && p.tok != lex.EOF {
		n.Stmt = append(n.Stmt, p.stmt())
	}
	n.End, _ = p.next()

	p.scope = p.scope.Parent
	return n
}

func (p *Parser) stmt() ast.Stmt {
	switch p.tok {
	case lex.FOR:
		return p.forstmt()
	case lex.HALT:
		return p.haltstmt()
	case lex.IE:
		return p.ifstmt(true)
	case lex.IF:
		return p.ifstmt(false)
	case lex.LEAVE:
		return p.leavestmt()
	case lex.LOOP:
		return p.loopstmt()
	case lex.RETURN:
		return p.returnstmt()
	case lex.WHILE:
		return p.whilestmt()
	case lex.DO:
		return p.compound(false)
	case lex.SYMBOL:
		return p.asgorcall()
	case lex.SEMI:
		semi, _ := p.next()
		return &ast.EmptyStmt{semi}
	default:
		p.errf("expected statement, got %v", p.tok)
		p.sync('s')
	}
	return nil
}

func (p *Parser) forstmt() *ast.ForStmt {
	s := &ast.ForStmt{}
	s.For, _ = p.next()
	s.Lparen, _ = p.expect(lex.LPAREN)
	s.Name = p.symbol()
	y := p.lookup(s.Name, 0)
	if y != nil && y.Type&(ast.CONST|ast.FUNC|ast.DECL) != 0 {
		p.declerrf("%v: unexpected type", s.Name.Ident)
	}
	p.expect(lex.EQ)
	s.Init = p.expr()
	p.expect(lex.COMMA)
	s.Cond = p.expr()
	if p.tok == lex.COMMA {
		p.next()
		s.Post = p.constval()
	}
	s.Rparen, _ = p.expect(lex.RPAREN)
	inloop := p.inloop
	p.inloop = true
	s.Body = p.stmt()
	p.inloop = inloop
	return s
}

func (p *Parser) haltstmt() *ast.ReturnStmt {
	s := &ast.ReturnStmt{}
	s.TokPos, s.Tok = p.next()
	s.Value = p.constval()
	p.expect(lex.SEMI)
	return s
}

func (p *Parser) ifstmt(alt bool) *ast.IfStmt {
	s := &ast.IfStmt{}
	s.IfPos, s.If = p.next()
	s.Lparen, _ = p.expect(lex.LPAREN)
	s.Cond = p.expr()
	s.Rparen, _ = p.expect(lex.RPAREN)
	s.Body = p.stmt()
	if alt {
		p.expect(lex.ELSE)
		s.Else = p.stmt()
	} else if p.tok == lex.ELSE {
		p.errf("ELSE without IE")
	}
	return s
}

func (p *Parser) leavestmt() *ast.BranchStmt {
	if !p.inloop {
		p.errf("LEAVE not in loop context")
	}
	s := &ast.BranchStmt{}
	s.TokPos, s.Tok = p.next()
	p.expect(lex.SEMI)
	return s
}

func (p *Parser) loopstmt() *ast.BranchStmt {
	if !p.inloop {
		p.errf("LOOP not in loop context")
	}
	s := &ast.BranchStmt{}
	s.TokPos, s.Tok = p.next()
	p.expect(lex.SEMI)
	return s
}

func (p *Parser) returnstmt() *ast.ReturnStmt {
	s := &ast.ReturnStmt{}
	s.TokPos, s.Tok = p.next()
	if !p.infun {
		p.errf("can't return from main body")
	}
	if p.tok != lex.SEMI {
		s.Value = p.expr()
	}
	p.expect(lex.SEMI)
	return s
}

func (p *Parser) whilestmt() *ast.WhileStmt {
	s := &ast.WhileStmt{}
	s.While, _ = p.next()
	s.Lparen, _ = p.expect(lex.LPAREN)
	s.Cond = p.expr()
	s.Rparen, _ = p.expect(lex.RPAREN)
	inloop := p.inloop
	p.inloop = true
	s.Body = p.stmt()
	p.inloop = inloop
	return s
}

func (p *Parser) asgorcall() ast.Stmt {
	y, n := p.address(1)
	switch p.tok {
	case lex.LPAREN:
		n = &ast.ExprStmt{p.fncall(y, n)}
	case lex.ASSIGN:
		s := &ast.AssignStmt{}
		s.Name = y
		s.Lhs = n
		p.next()
		s.Rhs = p.expr()
		n = s
	default:
		p.errf("syntax error: invalid assignment/call, got %q", p.lit)
	}
	p.expect(lex.SEMI)
	return n
}

func (p *Parser) expr() ast.Expr {
	n := p.disjn()
	if p.tok == lex.COND {
		e := &ast.CondExpr{}
		e.Op, _ = p.next()
		e.X = n
		e.Y = p.expr()
		p.expect(lex.COLON)
		e.Z = p.expr()
		n = e
	}
	return n
}

func (p *Parser) disjn() ast.Expr {
	n := p.conjn()
	dp := 0
	for p.tok == lex.DISJ {
		e := &ast.BinaryExpr{}
		e.OpPos, e.Op = p.next()
		e.X = n
		e.Y = p.conjn()
		n = e
		dp++
	}
	if dp > 0 {
		n = &ast.LogicalExpr{
			X:     n,
			Depth: dp,
		}
	}
	return n
}

func (p *Parser) conjn() ast.Expr {
	n := p.arith()
	dp := 0
	for p.tok == lex.CONJ {
		e := &ast.BinaryExpr{}
		e.OpPos, e.Op = p.next()
		e.X = n
		e.Y = p.arith()
		n = e
		dp++
	}
	if dp > 0 {
		n = &ast.LogicalExpr{
			X:     n,
			Depth: dp,
		}
	}
	return n
}

func (p *Parser) arith() ast.Expr {
	var op []*ast.BinaryExpr
	var ep []ast.Expr

	ep = append(ep, p.factor())
	for p.tok.IsBinary() {
		for len(op) > 0 && p.tok.Precedence() <= op[len(op)-1].Op.Precedence() {
			if len(ep) < 2 {
				op = op[:len(op)-1]
				continue
			}

			op[len(op)-1].X = ep[len(ep)-2]
			op[len(op)-1].Y = ep[len(ep)-1]
			ep[len(ep)-2] = op[len(op)-1]

			ep = ep[:len(ep)-1]
			op = op[:len(op)-1]
		}

		e := &ast.BinaryExpr{}
		e.OpPos, e.Op = p.next()
		op = append(op, e)
		ep = append(ep, p.factor())
	}

	for len(op) > 0 {
		if len(ep) < 2 {
			break
		}

		op[len(op)-1].X = ep[len(ep)-2]
		op[len(op)-1].Y = ep[len(ep)-1]
		ep[len(ep)-2] = op[len(op)-1]

		ep = ep[:len(ep)-1]
		op = op[:len(op)-1]
	}

	return ep[0]
}

func (p *Parser) factor() ast.Expr {
	var n ast.Expr
	switch t := p.tok; t {
	case lex.INTEGER, lex.STRING:
		n = &ast.BasicLit{
			Pos:   p.pos,
			Type:  p.tok,
			Value: p.lit,
		}
		p.next()
	case lex.SYMBOL:
		var y *ast.Symbol
		y, n = p.address(0)
		p.uses[n] = y
		if p.tok == lex.LPAREN {
			n = p.fncall(y, n)
			p.uses[n] = y
		}
	case lex.LBRACK:
		n = p.mktable()
	case lex.PACKED:
		n = p.mkbytevec()
	case lex.ADDROF:
		e := &ast.UnaryExpr{}
		e.OpPos, e.Op = p.next()
		_, e.X = p.address(2)
		n = e
	case lex.LPAREN:
		e := &ast.ParenExpr{}
		e.Lparen, _ = p.next()
		e.X = p.expr()
		e.Rparen, _ = p.expect(lex.RPAREN)
		n = e
	default:
		switch {
		case t.IsUnary():
			e := &ast.UnaryExpr{}
			e.OpPos, e.Op = p.next()
			e.X = p.factor()
			n = e
		default:
			p.errf("syntax error: invalid expression")
			p.sync('e')
		}
	}

	return n
}

func (p *Parser) mktable() ast.Expr {
	var (
		lpe     []scanner.Position
		dynamic bool
	)

	n := &ast.TableExpr{}
	n.Lbrack, _ = p.next()
	for p.tok != lex.RBRACK {
		if p.tok == lex.LPAREN {
			par, _ := p.next()
			lpe = append(lpe, par)
			dynamic = true
			continue
		} else if dynamic {
			pe := &ast.ParenExpr{}
			if len(lpe) > 0 {
				pe.Lparen, lpe = lpe[len(lpe)-1], lpe[:len(lpe)-1]
			}
			pe.X = p.expr()
			if p.tok == lex.RPAREN {
				pe.Rparen, _ = p.next()
				dynamic = false
			}
			n.Member = append(n.Member, pe)
		} else if p.tok == lex.INTEGER || p.tok == lex.SYMBOL {
			n.Member = append(n.Member, p.constval())
		} else if p.tok == lex.STRING {
			n.Member = append(n.Member, &ast.BasicLit{p.pos, p.tok, p.lit})
			p.next()
		} else if p.tok == lex.LBRACK {
			n.Member = append(n.Member, p.mktable())
		} else if p.tok == lex.PACKED {
			n.Member = append(n.Member, p.mkbytevec())
		} else {
			p.errf("%v: invalid table element", p.tok)
		}
		if p.tok != lex.COMMA {
			break
		}
		p.next()
	}
	n.Rbrack, _ = p.expect(lex.RBRACK)
	return n
}

func (p *Parser) mkbytevec() ast.Expr {
	n := &ast.ByteVecExpr{}
	p.next()
	n.Lbrack, _ = p.expect(lex.LBRACK)
	for p.tok != lex.RBRACK {
		if p.tok == lex.INTEGER {
			val, err := strconv.ParseInt(p.lit, 0, 32)
			if err != nil {
				p.errf("failed to parse integer inside packed: %v", err)
			}
			n.Data = append(n.Data, int(val))

			p.next()
			if p.tok == lex.COMMA {
				p.next()
			} else if p.tok != lex.RBRACK {
				p.errf("invalid element inside packed: %v", p.tok)
				return n
			}
		}
	}
	n.Rbrack, _ = p.expect(lex.RBRACK)
	return n
}

func (p *Parser) fncall(name *ast.Symbol, fun ast.Expr) *ast.CallExpr {
	n := &ast.CallExpr{}
	n.Name = name
	n.Fun = fun
	n.Lparen, _ = p.next()
	for p.tok != lex.RPAREN {
		n.Arg = append(n.Arg, p.expr())
		if p.tok != lex.COMMA {
			break
		}
		p.next()
		if p.tok == lex.RPAREN {
			p.errf("syntax error: invalid function call")
		}
	}
	n.Rparen, _ = p.expect(lex.RPAREN)

	_, y := p.find(name.Ident)
	if y == nil {
		p.declerrf("%v: function not defined", name.Ident)
		return n
	}
	if y.Type != ast.FUNC && y.Type != ast.DECL {
		p.declerrf("%v: call of non-function", name.Ident)
	}
	if len(n.Arg) != y.Len {
		p.declerrf("%v: function expected %d arguments, but got %d arguments", name.Ident, y.Len, len(n.Arg))
	}

	p.calls = append(p.calls, n)
	return n
}

func (p *Parser) address(lv int) (*ast.Symbol, ast.Expr) {
	y := p.symbol()
	y.Value = lv
	t := p.lookup(y, 0)
	if t != nil {
		if t.Type&ast.CONST != 0 && lv > 0 {
			p.errf("%v: invalid address", t.Ident)
		} else if t.Type&(ast.FUNC|ast.DECL) != 0 && lv == 2 {
			p.errf("%v: invalid address", t.Ident)
		}
		if p.tok == lex.LBRACK {
			if t.Type&(ast.FUNC|ast.DECL|ast.CONST) != 0 {
				p.errf("%v: bad subscript", t.Ident)
			}
		}
	}

	n := ast.Expr(y)
	for p.tok == lex.LBRACK {
		i := &ast.IndexExpr{}
		i.X = n
		i.Lbrack, _ = p.next()
		i.Index = p.expr()
		i.Rbrack, _ = p.expect(lex.RBRACK)
		n = i
	}
	if p.tok == lex.BYTEOP {
		p.next()
		i := &ast.IndexExpr{}
		i.Byte = true
		i.X = n
		i.Index = p.factor()
		n = i
	}
	return y, n
}
