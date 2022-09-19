package tc

import (
	"bufio"
	"encoding/binary"
	"fmt"
	"io"
	"strconv"
	"strings"

	"t3x9/ast"
	"t3x9/lex"
)

const bpw = 4

const (
	opush     = "00"
	oclear    = "01"
	oldval    = "02,w"
	oldaddr   = "03,a"
	oldlref   = "04,w"
	oldglob   = "05,a"
	oldlocl   = "06,w"
	ostglob   = "07,a"
	ostlocl   = "08,w"
	ostindr   = "09"
	ostindb   = "0a"
	oincglob  = "0b,a"
	oinclocl  = "0c,w"
	oalloc    = "0d,w"
	odealloc  = "0e,w"
	oloclvec  = "0f"
	oglobvec  = "10,a"
	oindex    = "11"
	oderef    = "12"
	oindxb    = "13"
	odrefb    = "14"
	omark     = ",m"
	oresolv   = ",r"
	ocall     = "17,W"
	ojumpfwd  = "18,>"
	ojumpback = "19,<"
	ojmpfalse = "1a,>"
	ojmptrue  = "1b,>"
	ofor      = "1c,>"
	ofordown  = "1d,>"
	oenter    = "1e"
	oexit     = "1f"
	ohalt     = "20,w"
	oneg      = "21"
	oinv      = "22"
	olognot   = "23"
	oadd      = "24"
	osub      = "25"
	omul      = "26"
	odiv      = "27"
	omod      = "28"
	oand      = "29"
	oor       = "2a"
	oxor      = "2b"
	oshl      = "2c"
	oshr      = "2d"
	oeq       = "2e"
	oneq      = "2f"
	olt       = "30"
	ogt       = "31"
	ole       = "32"
	oge       = "33"
	oword     = ",W"
	tcomp     = "3500"
	tcopy     = "3501"
	tfill     = "3502"
	tscan     = "3503"
	tcreate   = "3504"
	topen     = "3505"
	tclose    = "3506"
	tread     = "3507"
	twrite    = "3508"
	trename   = "3509"
	tremove   = "350a"
)

type scope struct {
	*ast.Scope
	parent *scope
	symbol map[string]*symbol
}

type symbol struct {
	*ast.Symbol
	value int
}

type reloc struct {
	addr int
	seg  int
}

type CG struct {
	text   []byte
	data   []byte
	rel    []reloc
	stack  []int
	lp     int
	loaded bool
	gsp    *scope
	csp    *scope
	leaves []int
	loops  []int
	loop0  int
	defs   map[*ast.Symbol]ast.Decl
}

func NewCG() *CG {
	return &CG{}
}

func (c *CG) Compile(w io.Writer, p *ast.Prog) error {
	c.program(p)

	c.text = c.pad(c.text, 4)
	c.relocate()
	b := bufio.NewWriter(w)
	c.writehdr(b)
	b.Write(c.text)
	b.Write(c.data)

	return b.Flush()
}

func (c *CG) relocate() {
	dist := len(c.text)
	for _, r := range c.rel {
		switch r.seg {
		case 't':
			a := c.tfetch(r.addr) + dist
			c.tpatch(r.addr, a)
		case 'd':
			a := c.dfetch(r.addr) + dist
			c.dpatch(r.addr, a)
		default:
			panic(fmt.Errorf("unknown segment %c", r.seg))
		}
	}
}

func (c *CG) builtin(name, code string) {
	c.gen(ojumpfwd, 0)
	c.add(&ast.Symbol{Ident: name, Type: ast.FUNC}, len(c.text))
	c.gen(code, 0)
	c.gen(oresolv, 0)
}

func (c *CG) writehdr(w io.Writer) {
	fmt.Fprintf(w, "#! /u/bin/tcvm\n")
	fmt.Fprintf(w, "T3X9")
	binary.Write(w, binary.LittleEndian, uint32(len(c.text)))
	binary.Write(w, binary.LittleEndian, uint32(len(c.data)))
}

func (c *CG) spill() {
	if c.loaded {
		c.gen(opush, 0)
	} else {
		c.loaded = true
	}
}

func (c *CG) clear() {
	c.loaded = false
}

func (c *CG) swap() {
	sp := len(c.stack)
	c.stack[sp-1], c.stack[sp-2] = c.stack[sp-2], c.stack[sp-1]
}

func (c *CG) tos() int {
	return c.stack[len(c.stack)-1]
}

func (c *CG) push(v int) {
	c.stack = append(c.stack, v)
}

func (c *CG) pop() int {
	l := len(c.stack) - 1
	v := c.stack[l]
	c.stack = c.stack[:l]
	return v
}

func (c *CG) tag(seg int) {
	addr := len(c.data) - bpw
	if seg == 't' {
		addr = len(c.text) - bpw
	}
	c.rel = append(c.rel, reloc{addr, seg})
}

func (c *CG) tpatch(a, x int) {
	binary.LittleEndian.PutUint32(c.text[a:], uint32(x))
}

func (c *CG) dpatch(a, x int) {
	binary.LittleEndian.PutUint32(c.data[a:], uint32(x))
}

func (c *CG) store(u *scope, y *symbol) {
	if u == c.gsp {
		c.gen(ostglob, y.value)
	} else {
		c.gen(ostlocl, y.value)
	}
}

func (c *CG) load(u *scope, y *symbol) {
	if u == c.gsp {
		c.gen(oldglob, y.value)
	} else {
		c.gen(oldlocl, y.value)
	}
}

func (c *CG) tfetch(a int) int {
	return int(binary.LittleEndian.Uint32(c.text[a:]))
}

func (c *CG) dfetch(a int) int {
	return int(binary.LittleEndian.Uint32(c.data[a:]))
}

func (c *CG) resolvefwd(loc, fn int) {
	for loc != 0 {
		nloc := c.tfetch(loc)
		c.tpatch(loc, fn-loc-bpw)
		loc = nloc
	}
}

func (c *CG) emit(x uint8) {
	c.text = append(c.text, x)
}

func (c *CG) emitw(x int) {
	c.emit(uint8(x))
	c.emit(uint8(x >> 8))
	c.emit(uint8(x >> 16))
	c.emit(uint8(x >> 24))
}

func (c *CG) datab(x uint8) {
	c.data = append(c.data, x)
}

func (c *CG) dataw(x int) {
	c.datab(uint8(x))
	c.datab(uint8(x >> 8))
	c.datab(uint8(x >> 16))
	c.datab(uint8(x >> 24))
}

func (c *CG) mkstring(s string) int {
	if len(s) >= 2 {
		if s[0] == '"' && s[len(s)-1] == '"' {
			s = s[1 : len(s)-1]
		}
	}
	a := len(c.data)
	for i := range s {
		c.datab(s[i])
	}
	c.datab(0)
	for len(c.data)%bpw != 0 {
		c.datab(0)
	}
	return a
}

func (c *CG) mkbytevec(b *ast.ByteVecExpr) int {
	a := len(c.data)
	for _, v := range b.Data {
		if v > 255 {
			panic(fmt.Errorf("packed byte value too large: %v", v))
		}
		c.datab(uint8(v))
	}
	for len(c.data)%bpw != 0 {
		c.datab(0)
	}
	return a
}

func (c *CG) mktable(t *ast.TableExpr) int {
	var tbl, af []int
	for _, m := range t.Member {
		switch m := m.(type) {
		case *ast.BasicLit:
			switch m.Type {
			case lex.STRING:
				tbl = append(tbl, c.mkstring(m.Value))
				af = append(af, 1)
			default:
				panic(fmt.Errorf("unknown table type %s", m.Type))
			}
		case *ast.ConstExpr:
			tbl = append(tbl, m.Value)
			af = append(af, 0)
		case *ast.ParenExpr:
			c.expr(m.X, true)
			c.gen(ostglob, 0)
			tbl = append(tbl, 0)
			af = append(af, len(c.text)-bpw)
		case *ast.TableExpr:
			tbl = append(tbl, c.mktable(m))
			af = append(af, 1)
		case *ast.ByteVecExpr:
			tbl = append(tbl, c.mkbytevec(m))
			af = append(af, 1)
		default:
			panic(fmt.Errorf("unknown table type %T", m))
		}
	}

	loc := len(c.data)
	for i := range tbl {
		c.dataw(tbl[i])
		if af[i] == 1 {
			c.tag('d')
		} else if af[i] > 1 {
			c.tpatch(af[i], len(c.data)-4)
		}
	}
	return loc
}

func (c *CG) gen(s string, v int) {
	var op uint16
	var mode rune
	var x16, nop bool

	if strings.HasPrefix(s, ",") {
		fmt.Sscanf(s, ",%c", &mode)
		nop = true
	} else if strings.IndexRune(s, ',') > 0 {
		fmt.Sscanf(s, "%x,%c", &op, &mode)
	} else {
		fmt.Sscanf(s, "%x", &op)
	}

	if op&0xff00 != 0 {
		op = (op>>8)&0xff | (op<<8)&0xff00
		x16 = true
	}
	if -0x80 <= v && v <= 0x7f {
		if mode == 'w' {
			mode = 'b'
		}
		op |= 0x80
	}
	if !nop {
		c.emit(uint8(op))
		if x16 {
			c.emit(uint8(op >> 8))
		}
	}

	switch mode {
	case 'b':
		c.emit(uint8(v))
	case 'w', 'W':
		c.emitw(v)
	case 'a':
		c.emitw(v)
		c.tag('t')
	case 'm':
		c.push(len(c.text))
	case '>':
		c.push(len(c.text))
		c.emitw(0)
	case '<':
		c.emitw(c.pop() - len(c.text) - bpw)
	case 'r':
		x := c.pop()
		c.tpatch(x, len(c.text)-x-bpw)
	case 0:
	default:
		panic(fmt.Errorf("unsupported op %q", s))
	}
}

func (c *CG) newscope(p *scope, u *ast.Scope) *scope {
	v := &scope{
		Scope:  u,
		parent: p,
		symbol: make(map[string]*symbol),
	}
	return v
}

func (c *CG) add(s *ast.Symbol, v int) (*scope, *symbol) {
	name := strings.ToLower(s.Ident)
	u, y := c.lookup(name)
	if u == c.csp && y != nil {
		return u, y
	}

	y = &symbol{Symbol: s, value: v}
	c.csp.symbol[name] = y
	return c.csp, y
}

func (c *CG) lookup(name string) (*scope, *symbol) {
	name = strings.ToLower(name)
	u := c.csp
	for u != nil {
		y := u.symbol[name]
		if y != nil {
			return u, y
		}
		u = u.parent
	}
	return nil, nil
}

func (c *CG) program(p *ast.Prog) {
	c.gsp = c.newscope(nil, p.Scope)
	c.csp = c.gsp
	c.defs = p.Defs

	c.builtin("t.memcomp", tcomp)
	c.builtin("t.memcopy", tcopy)
	c.builtin("t.memfill", tfill)
	c.builtin("t.memscan", tscan)
	c.builtin("t.create", tcreate)
	c.builtin("t.open", topen)
	c.builtin("t.close", tclose)
	c.builtin("t.read", tread)
	c.builtin("t.write", twrite)
	c.builtin("t.rename", trename)
	c.builtin("t.remove", tremove)

	c.declaration(c.csp)
	c.gen(oenter, 0)
	c.compound(p.Stmt)
	c.gen(ohalt, 0)
}

func (c *CG) declaration(u *scope) {
	for _, s := range u.Symbol {
		switch {
		case s.Type&ast.CONST != 0:
			c.add(s, s.Value.(*ast.ConstExpr).Value)
		case s.Type&ast.DECL != 0:
			c.add(s, 0)
		case s.Type&ast.FUNC != 0:
			c.funcdecl(s)
		case s.Type&ast.VAR != 0:
			c.vardecl(s)
		}
	}
}

func (c *CG) funcdecl(s *ast.Symbol) {
	f, ok := c.defs[s].(*ast.FuncDecl)
	if !ok {
		return
	}

	c.gen(ojumpfwd, 0)
	_, y := c.add(s, len(c.text))

	c.csp = c.newscope(c.csp, f.Scope)

	laddr := 2 * bpw
	for _, a := range f.Arg {
		c.add(a, 12+len(f.Arg)*bpw-laddr)
		laddr += bpw
	}
	if y.Type&ast.DECL != 0 {
		c.resolvefwd(y.value, len(c.text))
		y.Symbol = s
		y.value = len(c.text)
	}
	c.gen(oenter, 0)
	c.stmt(f.Body)
	c.gen(oclear, 0)
	c.gen(oexit, 0)
	c.gen(oresolv, 0)
	c.csp = c.csp.parent
}

func (c *CG) vardecl(s *ast.Symbol) {
	u, y := c.add(s, c.lp)

	size := 1
	switch {
	case y.Type&ast.BYTE != 0:
		size = (y.Len + bpw - 1) / bpw
	case y.Type&ast.VECT != 0:
		size = y.Len
	}

	if u == c.gsp {
		y.value = len(c.data)
		if y.Type&ast.VECT != 0 {
			c.gen(oalloc, size*bpw)
			c.gen(oglobvec, len(c.data))
		}
		c.dataw(0)
	} else {
		c.gen(oalloc, size*bpw)
		c.lp -= size * bpw
		if y.Type&ast.VECT != 0 {
			c.gen(oloclvec, 0)
			c.lp -= bpw
		}
		y.value = c.lp
	}
}

func (c *CG) compound(s *ast.CompoundStmt) {
	c.csp = c.newscope(c.csp, s.Scope)
	lp := c.lp
	c.declaration(c.csp)
	for _, s := range s.Stmt {
		c.stmt(s)
	}
	if lp != c.lp {
		c.gen(odealloc, lp-c.lp)
	}
	c.lp = lp
	c.csp = c.csp.parent
}

func (c *CG) stmt(s ast.Stmt) {
	switch s := s.(type) {
	case *ast.CompoundStmt:
		c.compound(s)
	case *ast.AssignStmt:
		c.assignstmt(s)
	case *ast.ExprStmt:
		switch e := s.X.(type) {
		case *ast.CallExpr:
			c.callstmt(e)
		default:
			panic(fmt.Errorf("unsupported statement %T", s))
		}
	case *ast.IfStmt:
		c.ifstmt(s)
	case *ast.WhileStmt:
		c.whilestmt(s)
	case *ast.ForStmt:
		c.forstmt(s)
	case *ast.BranchStmt:
		switch s.Tok {
		case lex.LEAVE:
			c.leavestmt(s)
		case lex.LOOP:
			c.loopstmt(s)
		}
	case *ast.ReturnStmt:
		switch s.Tok {
		case lex.HALT:
			c.haltstmt(s)
		case lex.RETURN:
			c.returnstmt(s)
		default:
			panic(fmt.Errorf("unsupported statement %T", s))
		}
	case *ast.EmptyStmt:
	default:
		panic(fmt.Errorf("unsupported statement %T", s))
	}
}

func (c *CG) assignstmt(s *ast.AssignStmt) {
	c.clear()
	u, y, b := c.index(s.Lhs)
	c.expr(s.Rhs, false)
	if y == nil {
		if b {
			c.gen(ostindb, 0)
		} else {
			c.gen(ostindr, 0)
		}
	} else {
		c.store(u, y)
	}
}

func (c *CG) callstmt(s *ast.CallExpr) {
	c.clear()
	c.index(s.Fun)
	c.fncall(s)
}

func (c *CG) leavestmt(s *ast.BranchStmt) {
	c.gen(ojumpfwd, 0)
	c.leaves = append(c.leaves, c.pop())
}

func (c *CG) loopstmt(s *ast.BranchStmt) {
	if c.loop0 > 0 {
		c.push(c.loop0)
		c.gen(ojumpback, 0)
	} else {
		c.gen(ojumpfwd, 0)
		c.loops = append(c.loops, c.pop())
	}
}

func (c *CG) ifstmt(s *ast.IfStmt) {
	c.expr(s.Cond, true)
	c.gen(ojmpfalse, 0)
	c.stmt(s.Body)
	if s.Else != nil {
		c.gen(ojumpfwd, 0)
		c.swap()
		c.gen(oresolv, 0)
		c.stmt(s.Else)
	}
	c.gen(oresolv, 0)
}

func (c *CG) whilestmt(s *ast.WhileStmt) {
	lp0 := c.loop0
	lvp := len(c.leaves)

	c.gen(omark, 0)
	c.loop0 = c.tos()
	c.expr(s.Cond, true)
	c.gen(ojmpfalse, 0)
	c.stmt(s.Body)
	c.swap()
	c.gen(ojumpback, 0)
	c.gen(oresolv, 0)
	for len(c.leaves) > lvp {
		c.push(c.leaves[len(c.leaves)-1])
		c.gen(oresolv, 0)
		c.leaves = c.leaves[:len(c.leaves)-1]
	}
	c.loop0 = lp0
}

func (c *CG) forstmt(s *ast.ForStmt) {
	llp := len(c.loops)
	lvp := len(c.leaves)
	lp0 := c.loop0
	c.loop0 = 0

	u, y := c.lookup(s.Name.Ident)

	c.expr(s.Init, true)
	c.store(u, y)

	c.gen(omark, 0)
	c.load(u, y)
	c.expr(s.Cond, false)

	step := 1
	if s.Post != nil {
		step = s.Post.Value
	}
	if step < 0 {
		c.gen(ofordown, 0)
	} else {
		c.gen(ofor, 0)
	}

	c.stmt(s.Body)
	for len(c.loops) > llp {
		c.push(c.loops[len(c.loops)-1])
		c.gen(oresolv, 0)
		c.loops = c.loops[:len(c.loops)-1]
	}

	if u == c.gsp {
		c.gen(oincglob, y.value)
	} else {
		c.gen(oinclocl, y.value)
	}
	c.gen(oword, step)
	c.swap()
	c.gen(ojumpback, 0)
	c.gen(oresolv, 0)

	for len(c.leaves) > lvp {
		c.push(c.leaves[len(c.leaves)-1])
		c.gen(oresolv, 0)
		c.leaves = c.leaves[:len(c.leaves)-1]
	}

	c.loop0 = lp0
}

func (c *CG) haltstmt(s *ast.ReturnStmt) {
	c.gen(ohalt, s.Value.(*ast.ConstExpr).Value)
}

func (c *CG) returnstmt(s *ast.ReturnStmt) {
	if s.Value == nil {
		c.gen(oclear, 0)
	} else {
		c.expr(s.Value, true)
	}
	if c.lp != 0 {
		c.gen(odealloc, -c.lp)
	}
	c.gen(oexit, 0)
}

func (c *CG) expr(e ast.Expr, clr bool) {
	if clr {
		c.clear()
	}
	c.sexpr(e)
}

func (c *CG) factor(e ast.Expr) {
	switch e := e.(type) {
	case *ast.BasicLit:
		switch e.Type {
		case lex.INTEGER:
			str := e.Value
			if strings.HasPrefix(str, "%") {
				str = "-" + str[1:]
			}
			val, _ := strconv.ParseInt(str, 0, 64)
			c.spill()
			c.gen(oldval, int(val))
		case lex.STRING:
			c.spill()
			c.gen(oldaddr, c.mkstring(e.Value))
		}
	case *ast.Symbol:
		c.address(e.Ident, 0, false)
	case *ast.ParenExpr:
		c.expr(e.X, false)
	case *ast.CallExpr:
		c.fncall(e)
	case *ast.TableExpr:
		c.spill()
		c.gen(oldaddr, c.mktable(e))
	case *ast.ByteVecExpr:
		c.spill()
		c.gen(oldaddr, c.mkbytevec(e))
	default:
		panic(fmt.Errorf("unsupported expression %T", e))
	}
}

func (c *CG) fncall(e *ast.CallExpr) {
	_, y := c.lookup(e.Name.Ident)
	for _, a := range e.Arg {
		c.expr(a, false)
	}
	if c.loaded {
		c.spill()
	}
	if y.Type&ast.DECL != 0 {
		c.gen(ocall, y.value)
		y.value = len(c.text) - bpw
	} else {
		c.gen(ocall, y.value-len(c.text)-5)
	}
	if len(e.Arg) > 0 {
		c.gen(odealloc, len(e.Arg)*bpw)
	}
	c.loaded = true
}

func (c *CG) lexpr(op string, e *ast.BinaryExpr) {
	c.sexpr(e.X)
	c.gen(op, 0)
	c.clear()
	c.sexpr(e.Y)
}

func (c *CG) index(e ast.Expr) (u *scope, y *symbol, bp bool) {
	var p []ast.Expr
loop:
	for {
		p = append(p, e)
		switch t := e.(type) {
		case *ast.IndexExpr:
			e = t.X
		default:
			break loop
		}
	}

	var lv int
	for i := len(p) - 1; i >= 0; i-- {
		switch t := p[i].(type) {
		case *ast.Symbol:
			lv = t.Value.(int)
			u, y = c.address(t.Ident, lv, len(p) > 1)
		case *ast.IndexExpr:
			if t.Byte {
				c.factor(t.Index)
				c.gen(oindxb, 0)
				if lv == 0 {
					c.gen(odrefb, 0)
				}
				bp = true
				break
			}
			c.expr(t.Index, false)
			c.gen(oindex, 0)
			if i > 0 || lv == 0 {
				c.gen(oderef, 0)
			}
		default:
			panic(fmt.Errorf("unsupported index expression: %T", t))
		}
	}
	if len(p) > 1 {
		y = nil
	}

	return
}

func (c *CG) cond(e *ast.CondExpr) {
	c.sexpr(e.X)
	c.gen(ojmpfalse, 0)
	c.expr(e.Y, true)
	c.gen(ojumpfwd, 0)
	c.swap()
	c.gen(oresolv, 0)
	c.expr(e.Z, true)
	c.gen(oresolv, 0)
}

func (c *CG) sexpr(e ast.Expr) {
	switch e := e.(type) {
	case *ast.IndexExpr:
		c.index(e)
	case *ast.CondExpr:
		c.cond(e)
	case *ast.LogicalExpr:
		c.sexpr(e.X)
		for i := 0; i < e.Depth; i++ {
			c.gen(oresolv, 0)
		}
	case *ast.BinaryExpr:
		if e.Op == lex.CONJ {
			c.lexpr(ojmpfalse, e)
			break
		} else if e.Op == lex.DISJ {
			c.lexpr(ojmptrue, e)
			break
		}
		c.sexpr(e.X)
		c.sexpr(e.Y)
		switch e.Op {
		case lex.ADD:
			c.gen(oadd, 0)
		case lex.SUB:
			c.gen(osub, 0)
		case lex.MUL:
			c.gen(omul, 0)
		case lex.DIV:
			c.gen(odiv, 0)
		case lex.MOD:
			c.gen(omod, 0)
		case lex.LT:
			c.gen(olt, 0)
		case lex.LE:
			c.gen(ole, 0)
		case lex.GT:
			c.gen(ogt, 0)
		case lex.GE:
			c.gen(oge, 0)
		case lex.NEQ:
			c.gen(oneq, 0)
		case lex.EQ:
			c.gen(oeq, 0)
		case lex.AND:
			c.gen(oand, 0)
		case lex.OR:
			c.gen(oor, 0)
		case lex.XOR:
			c.gen(oxor, 0)
		case lex.SHL:
			c.gen(oshl, 0)
		case lex.SHR:
			c.gen(oshr, 0)
		default:
			panic(fmt.Errorf("unsupported binary op %v", e.Op))
		}
	case *ast.UnaryExpr:
		if e.Op == lex.ADDROF {
			u, y, _ := c.index(e.X)
			if y == nil {
				// nop
			} else if u == c.gsp {
				c.spill()
				c.gen(oldaddr, y.value)
			} else {
				c.spill()
				c.gen(oldlref, y.value)
			}
			break
		}
		c.sexpr(e.X)
		switch e.Op {
		case lex.SUB:
			c.gen(oneg, 0)
		case lex.INV:
			c.gen(oinv, 0)
		case lex.LNOT:
			c.gen(olognot, 0)
		default:
			panic(fmt.Errorf("unsupported binary op %v", e.Op))
		}
	default:
		c.factor(e)
	}
}

func (c *CG) address(name string, lv int, idx bool) (u *scope, y *symbol) {
	u, y = c.lookup(name)
	if y.Type&ast.CONST != 0 {
		c.spill()
		c.gen(oldval, y.value)
	} else if lv == 0 || idx {
		c.spill()
		c.load(u, y)
	}

	return
}

func (c *CG) pad(b []byte, a int) []byte {
	n := (len(b) + a) &^ (a - 1)
	return append(b, make([]byte, n-len(b))...)
}
