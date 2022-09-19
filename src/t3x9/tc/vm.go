package tc

import (
	"bufio"
	"encoding/binary"
	"fmt"
	"io"
	"math"
	"os"
	"strings"
)

type Cell int32

type Inst struct {
	Enc    [9]byte
	I      Cell
	Op     Cell
	A1, A2 Cell
	Len    uint32
}

type VM struct {
	Verbose int               // tracing
	A       Cell              // accumulator
	F       Cell              // status register
	I       Cell              // instruction pointer
	P       Cell              // stack pointer
	M       []byte            // memory
	MS      uint32            // stack size
	MD      uint32            // code length
	MR      uint32            // code + data length
	MM      Cell              // memory mask
	ST      Cell              // status
	HLT     Cell              // exception status
	FD      map[Cell]*os.File // file descriptor table
}

func NewVM(stacksize, memsize uint32) *VM {
	if stacksize <= 0 {
		stacksize = 0x10000
	}

	progsize := uint32(memsize)
	if progsize != 0 {
		progsize += uint32(stacksize)
		progsize = nextpow2(progsize)
	}

	vm := &VM{
		M:  make([]byte, progsize),
		MS: stacksize,
	}
	vm.Reset()
	return vm
}

func (vm *VM) Reset() {
	vm.A = 0
	vm.F = 0
	vm.I = 0
	vm.P = Cell(len(vm.M) - 4)
	vm.HLT = 0
	vm.ST = 0
	vm.MM = Cell(len(vm.M) - 1)
	for _, f := range vm.FD {
		if f == os.Stdin || f == os.Stdout || f == os.Stderr {
			continue
		}
		f.Close()
	}
	vm.FD = map[Cell]*os.File{
		0: os.Stdin,
		1: os.Stdout,
		2: os.Stderr,
	}
}

func (vm *VM) ExceptionString(v Cell) string {
	switch v {
	case 0x00:
		return "program running"
	case 0x01:
		return "program completed"
	case 0x55:
		return "invalid syscall"
	case 0x88:
		return "stack overflow"
	case 0xcc:
		return "invalid opcode"
	}
	return fmt.Sprintf("unknown exception(%d)", v)
}

func (vm *VM) Load(r io.Reader) error {
	b := bufio.NewReader(r)
	for {
		v, err := b.ReadByte()
		if err != nil {
			return err
		}
		if v == '\n' {
			break
		}
	}

	var sig uint32
	binary.Read(b, binary.LittleEndian, &sig)
	if sig != 0x39583354 {
		return fmt.Errorf("not a tcvm program")
	}

	var tp, dp uint32
	binary.Read(b, binary.LittleEndian, &tp)
	binary.Read(b, binary.LittleEndian, &dp)

	progsize := nextpow2(16*(tp+dp) + vm.MS)
	if len(vm.M) == 0 {
		vm.M = make([]byte, progsize)
	}

	if progsize > uint32(len(vm.M)) {
		return fmt.Errorf("program too large, expected %v but only have %v bytes", progsize, len(vm.M))
	}
	vm.MD = tp
	vm.MR = tp + dp
	binary.Read(b, binary.LittleEndian, vm.M[:vm.MR])

	_, err := b.ReadByte()
	if err != nil && err != io.EOF {
		return fmt.Errorf("trailing garbage")
	}

	vm.Reset()
	return nil
}

func (vm *VM) Run() error {
	if vm.Verbose >= 2 {
		fmt.Fprintf(os.Stdout, "code %x program %x memsize %x memmask %x\n", vm.MD, vm.MR, len(vm.M), vm.MM)
	}
	for vm.HLT == 0 {
		vm.Step()
	}
	if vm.HLT != 1 {
		return fmt.Errorf("program died with exception %x: %s\n", vm.HLT, vm.ExceptionString(vm.HLT))
	}
	return nil
}

func (vm *VM) Step() {
	if vm.P < Cell(vm.MR) {
		vm.HLT = 0x88
		return
	}
	if vm.HLT != 0 {
		return
	}

	if vm.Verbose >= 2 {
		inst := vm.Disasm(vm.I)
		fmt.Fprintf(os.Stdout, "%v ", inst)
	}
	if vm.Verbose >= 1 {
		const mask = Cell(^0)
		fmt.Fprintf(os.Stdout, "F=%08x P=%08x I=%08x %2x a=%08x A=%08x S0=%08x\n",
			vm.F&mask, vm.P&mask, vm.I&mask, vm.lb(vm.I), vm.lw(vm.I+1)&mask, vm.A&mask, vm.lw(vm.P)&mask)
	}

	op := vm.fetchb()
	switch op {
	case 0x80, 0x00:
		vm.push(vm.A)
	case 0x81, 0x01:
		vm.A = 0
	case 0x82:
		vm.A = vm.fetchsb()
	case 0x02, 0x83, 0x03:
		vm.A = vm.fetchw()
	case 0x84:
		vm.A = vm.F + vm.fetchsb()
	case 0x04:
		vm.A = vm.F + vm.fetchw()
	case 0x85, 0x05:
		vm.A = vm.lw(vm.fetchw())
	case 0x86:
		vm.A = vm.lw(vm.F + vm.fetchsb())
	case 0x06:
		vm.A = vm.lw(vm.F + vm.fetchw())
	case 0x87, 0x07:
		vm.sw(vm.fetchw(), vm.A)
	case 0x88:
		vm.sw(vm.F+vm.fetchsb(), vm.A)
	case 0x08:
		vm.sw(vm.F+vm.fetchw(), vm.A)
	case 0x89, 0x09:
		vm.sw(vm.pop(), vm.A)
	case 0x8a, 0x0a:
		vm.sb(vm.pop(), byte(vm.A))
	case 0x8b, 0x0b:
		t := vm.fetchw()
		vm.sw(t, vm.lw(t)+vm.fetchw())
	case 0x8c:
		t := vm.fetchsb()
		vm.sw(vm.F+t, vm.lw(vm.F+t)+vm.fetchw())
	case 0x0c:
		t := vm.fetchw()
		vm.sw(vm.F+t, vm.lw(vm.F+t)+vm.fetchw())
	case 0x8d:
		vm.P -= vm.fetchsb()
	case 0x0d:
		vm.P -= vm.fetchw()
	case 0x8e:
		vm.P += vm.fetchsb()
	case 0x0e:
		vm.P += vm.fetchw()
	case 0x8f, 0x0f:
		vm.push(vm.P)
	case 0x90, 0x10:
		vm.sw(vm.fetchw(), vm.P)
	case 0x91, 0x11:
		vm.A = (vm.A << 2) + vm.pop()
	case 0x92, 0x12:
		vm.A = vm.lw(vm.A)
	case 0x93, 0x13:
		vm.A = vm.A + vm.pop()
	case 0x94, 0x14:
		vm.A = Cell(vm.lb(vm.A))
	case 0x97, 0x17:
		vm.push(vm.I + 3)
		vm.I = (vm.I + vm.fetchw()) & vm.MM
	case 0x98, 0x18, 0x99, 0x19:
		vm.I = (vm.I + vm.fetchw()) & vm.MM
	case 0x9a, 0x1a:
		vm.branch(vm.A == 0)
	case 0x9b, 0x1b:
		vm.branch(vm.A != 0)
	case 0x9c, 0x1c:
		vm.branch(vm.pop() >= vm.A)
	case 0x9d, 0x1d:
		vm.branch(vm.pop() <= vm.A)
	case 0x9e, 0x1e:
		vm.push(vm.F)
		vm.F = vm.P
	case 0x9f, 0x1f:
		vm.F = vm.pop()
		vm.I = (vm.pop() + 1) & vm.MM
	case 0xa0:
		vm.ST = vm.fetchsb()
		vm.HLT = 1
	case 0x20:
		vm.ST = vm.fetchw()
		vm.HLT = 1
	case 0xa1, 0x21:
		vm.A = -vm.A
	case 0xa2, 0x22:
		vm.A = ^vm.A
	case 0xa3, 0x23:
		if vm.A == 0 {
			vm.A = -1
		} else {
			vm.A = 0
		}
	case 0xa4, 0x24:
		vm.A = vm.pop() + vm.A
	case 0xa5, 0x25:
		vm.A = vm.pop() - vm.A
	case 0xa6, 0x26:
		vm.A = vm.pop() * vm.A
	case 0xa7, 0x27:
		t := vm.pop()
		if vm.A != 0 {
			vm.A = t / vm.A
		} else {
			vm.A = 0
		}
	case 0xa8, 0x28:
		t := vm.pop()
		if vm.A != 0 {
			vm.A = Cell(uint32(t) % uint32(vm.A))
		} else {
			vm.A = 0
		}
	case 0xa9, 0x29:
		vm.A = vm.pop() & vm.A
	case 0xaa, 0x2a:
		vm.A = vm.pop() | vm.A
	case 0xab, 0x2b:
		vm.A = vm.pop() ^ vm.A
	case 0xac, 0x2c:
		vm.A = vm.pop() << uint(vm.A)
	case 0xad, 0x2d:
		vm.A = Cell(uint32(vm.pop()) >> uint(vm.A))
	case 0xae, 0x2e:
		vm.truth(vm.pop() == vm.A)
	case 0xaf, 0x2f:
		vm.truth(vm.pop() != vm.A)
	case 0xb0, 0x30:
		vm.truth(vm.pop() < vm.A)
	case 0xb1, 0x31:
		vm.truth(vm.pop() > vm.A)
	case 0xb2, 0x32:
		vm.truth(vm.pop() <= vm.A)
	case 0xb3, 0x33:
		vm.truth(vm.pop() >= vm.A)
	case 0xb5, 0x35:
		vm.A = vm.syscall(Cell(vm.lb(vm.I)))
		vm.I = (vm.pop() + 1) & vm.MM
	default:
		vm.HLT = 0xcc
	}
}

func (vm *VM) readstrz(o Cell) string {
	var sb strings.Builder
	for i := 0; i < len(vm.M); i++ {
		ch := vm.lb(o + Cell(i))
		if ch == 0 {
			break
		}
		sb.WriteByte(ch)
	}
	return sb.String()
}

func (vm *VM) memslice(a Cell) []byte {
	return vm.M[a&vm.MM:]
}

func (vm *VM) newfd() Cell {
	for i := Cell(0); i < math.MaxInt32; i++ {
		if vm.FD[i] == nil {
			return i
		}
	}
	return -1
}

func (vm *VM) sysargmem() (a, b []byte) {
	a = vm.memslice(vm.lw(vm.P + 12))
	b = vm.memslice(vm.lw(vm.P + 8))
	n := int(vm.lw(vm.P + 4))
	if len(a) < n {
		n = len(a)
	}
	if len(b) < n {
		n = len(b)
	}
	a = a[:n]
	b = b[:n]

	return
}

func (vm *VM) sysmemset() {
	a := vm.lw(vm.P + 12)
	v := vm.lw(vm.P + 8)
	n := vm.lw(vm.P + 4)
	for i := Cell(0); i < n; i++ {
		vm.sb(a+i, byte(v))
	}
}

func (vm *VM) sysmemscan() Cell {
	a := vm.lw(vm.P + 12)
	c := vm.lw(vm.P + 8)
	k := vm.lw(vm.P + 4)
	for i := Cell(0); i < k; i++ {
		if Cell(vm.lb(a+i)) == c {
			return i
		}
	}
	return -1
}

func (vm *VM) syscreate() Cell {
	i := vm.newfd()
	if i < 0 {
		return -1
	}

	s := vm.readstrz(vm.lw(vm.P + 4))
	f, err := os.Create(s)
	if err != nil {
		return -1
	}

	vm.FD[i] = f
	return i
}

func (vm *VM) sysopen() Cell {
	i := vm.newfd()
	if i < 0 {
		return -1
	}

	s := vm.readstrz(vm.lw(vm.P + 8))
	m := vm.lw(vm.P + 4)
	f, err := os.OpenFile(s, int(m), 0644)
	if err != nil {
		return -1
	}

	vm.FD[i] = f
	return i
}

func (vm *VM) sysclose() Cell {
	i := vm.lw(vm.P + 4)
	if vm.FD[i] == nil {
		return -1
	}

	err := vm.FD[i].Close()
	delete(vm.FD, i)
	if err != nil {
		return -1
	}

	return 0
}

func (vm *VM) sysio(rw int) Cell {
	i := vm.lw(vm.P + 12)
	if vm.FD[i] == nil {
		return -1
	}
	p := vm.memslice(vm.lw(vm.P + 8))
	n := vm.lw(vm.P + 4)
	if len(p) >= int(n) {
		p = p[:n]
	}

	var nn int
	var err error
	switch rw {
	case 0:
		nn, err = vm.FD[i].Read(p)
	case 1:
		nn, err = vm.FD[i].Write(p)
	default:
		panic("unreachable")
	}

	if err != nil {
		return -1
	}

	return Cell(nn)
}

func (vm *VM) sysrename() Cell {
	f := vm.readstrz(vm.lw(vm.P + 8))
	t := vm.readstrz(vm.lw(vm.P + 4))
	err := os.Rename(f, t)
	if err != nil {
		return -1
	}
	return 0
}

func (vm *VM) sysremove() Cell {
	s := vm.readstrz(vm.lw(vm.P + 4))
	err := os.Remove(s)
	if err != nil {
		return -1
	}
	return 0
}

func (vm *VM) syscall(n Cell) Cell {
	var r Cell
	switch n {
	case 0:
		r = memcmp(vm.sysargmem())
	case 1:
		a, b := vm.sysargmem()
		copy(a, b)
	case 2:
		vm.sysmemset()
	case 3:
		r = vm.sysmemscan()
	case 4:
		r = vm.syscreate()
	case 5:
		r = vm.sysopen()
	case 6:
		r = vm.sysclose()
	case 7:
		r = vm.sysio(0)
	case 8:
		r = vm.sysio(1)
	case 9:
		r = vm.sysrename()
	case 10:
		r = vm.sysremove()
	default:
		vm.HLT = 0x55
	}
	if vm.Verbose > 0 {
		fmt.Fprintf(os.Stdout, "SYSCALL(%d): %x %x %x %x %d\n",
			n, vm.lw(vm.P+12), vm.lw(vm.P+8), vm.lw(vm.P+4), vm.lw(vm.P), r)
	}
	return r
}

func (vm *VM) branch(cond bool) {
	t := vm.fetchw()
	if cond {
		vm.I = (vm.I + t) & vm.MM
	}
}

func (vm *VM) truth(cond bool) {
	vm.A = 0
	if cond {
		vm.A = -1
	}
}

func (vm *VM) push(x Cell) {
	vm.P = (vm.P - 4) & vm.MM
	vm.sw(vm.P, x)
}

func (vm *VM) pop() Cell {
	v := vm.lw(vm.P)
	vm.P = (vm.P + 4) & vm.MM
	return v
}

func (vm *VM) fetchb() Cell {
	v := vm.lb(vm.I)
	vm.I = (vm.I + 1) & vm.MM
	return Cell(v)
}

func (vm *VM) fetchsb() Cell {
	return Cell(int8(vm.fetchb()))
}

func (vm *VM) fetchw() Cell {
	arg := vm.lw(vm.I)
	vm.I = (vm.I + 4) & vm.MM
	return arg
}

func (vm *VM) sw(a, w Cell) {
	vm.sb(a, byte(w&0xff))
	vm.sb(a+1, byte((w>>8)&0xff))
	vm.sb(a+2, byte((w>>16)&0xff))
	vm.sb(a+3, byte((w>>24)&0xff))
}

func (vm *VM) lw(a Cell) Cell {
	v := int32(vm.lb(a))
	v |= int32(vm.lb(a+1)) << 8
	v |= int32(vm.lb(a+2)) << 16
	v |= int32(vm.lb(a+3)) << 24
	return Cell(v)
}

func (vm *VM) sb(a Cell, b byte) {
	vm.M[a&vm.MM] = b
}

func (vm *VM) lb(a Cell) byte {
	return vm.M[a&vm.MM]
}

func (vm *VM) lsb(a Cell) Cell {
	return Cell(int8(vm.M[a&vm.MM]))
}

var openc = [256]string{
	0x80: "push",
	0x00: "push",
	0x81: "clear",
	0x01: "clear",
	0x82: "ldval s",
	0x02: "ldval a",
	0x83: "ldaddr a",
	0x03: "ldaddr a",
	0x84: "ldlref s",
	0x04: "ldlref a",
	0x85: "ldglob a",
	0x05: "ldglob a",
	0x86: "ldlocl s",
	0x06: "ldlocl a",
	0x87: "stglob a",
	0x07: "stglob a",
	0x08: "stlocl a",
	0x88: "stlocl s",
	0x89: "stindr",
	0x09: "stindr",
	0x8a: "stindb",
	0x0a: "stindb",
	0x8b: "incglob aa",
	0x0b: "incglob aa",
	0x8c: "inclocl sa",
	0x0c: "inclocl aa",
	0x8d: "alloc s",
	0x0d: "alloc a",
	0x8e: "dealloc s",
	0x0e: "dealloc a",
	0x8f: "loclvec",
	0x0f: "loclvec",
	0x90: "globvec a",
	0x10: "globvec a",
	0x91: "index",
	0x11: "index",
	0x92: "deref",
	0x12: "deref",
	0x93: "indxb",
	0x13: "indxb",
	0x94: "drefb",
	0x14: "drefb",
	0x97: "call a",
	0x17: "call a",
	0x98: "jumpfwd a",
	0x18: "jumpfwd a",
	0x99: "jumpback a",
	0x19: "jumpback a",
	0x9a: "jmpfalse a",
	0x1a: "jmpfalse a",
	0x9b: "jmptrue a",
	0x1b: "jmptrue a",
	0x9c: "for a",
	0x1c: "for a",
	0x9d: "fordown a",
	0x1d: "fordown a",
	0x9e: "enter",
	0x1e: "enter",
	0x9f: "exit",
	0x1f: "exit",
	0xa0: "halt s",
	0x20: "halt a",
	0xa1: "neg",
	0x21: "neg",
	0xa2: "inv",
	0x22: "inv",
	0xa3: "lognot",
	0x23: "lognot",
	0xa4: "add",
	0x24: "add",
	0xa5: "sub",
	0x25: "sub",
	0xa6: "mul",
	0x26: "mul",
	0xa7: "div",
	0x27: "div",
	0xa8: "mod",
	0x28: "mod",
	0xa9: "and",
	0x29: "and",
	0xaa: "or",
	0x2a: "or",
	0xab: "xor",
	0x2b: "xor",
	0xac: "shl",
	0x2c: "shl",
	0xad: "shr",
	0x2d: "shr",
	0xae: "eq",
	0x2e: "eq",
	0xaf: "neq",
	0x2f: "neq",
	0xb0: "lt",
	0x30: "lt",
	0xb1: "gt",
	0x31: "gt",
	0xb2: "le",
	0x32: "le",
	0xb3: "ge",
	0x33: "ge",
	0xb5: "syscall s",
}

func (vm *VM) Disasm(ip Cell) Inst {
	inst := Inst{}
	inst.Op = Cell(vm.lb(ip))
	inst.Len = 1
	inst.I = ip
	inst.Enc[0] = byte(inst.Op)

	if openc[inst.Op] == "" {
		return inst
	}

	args := ""
	if enc := strings.Split(openc[inst.Op], " "); len(enc) > 1 {
		args = enc[1]
	}

	pargs := []*Cell{&inst.A1, &inst.A2}
	for i, arg := range args {
		switch arg {
		case 's':
			*pargs[i] = vm.lsb(ip + Cell(inst.Len))
			inst.Len += 1
		case 'a':
			*pargs[i] = vm.lw(ip + Cell(inst.Len))
			inst.Len += 4
		default:
			panic("unreachable")
		}
	}
	for i := uint32(1); i < inst.Len; i++ {
		inst.Enc[i] = vm.lb(ip + Cell(i))
	}

	return inst
}

func (vm *VM) Objdump(w io.Writer) {
	fmt.Fprintf(w, "Code %x Program %x Mem %x MemMask %x\n", vm.MD, vm.MR, len(vm.M), vm.MM)

	i := uint32(0)
	for i < vm.MD {
		inst := vm.Disasm(Cell(i))
		fmt.Fprintf(w, "%07x %v\n", i, inst)
		i += inst.Len
	}
	fmt.Fprintf(w, "\n")
	for ; i < vm.MR; i += 16 {
		fmt.Fprintf(w, "%07x ", i)
		for j := uint32(0); j < 16; j++ {
			if j != 0 && j%4 == 0 {
				fmt.Fprintf(w, " ")
			}
			fmt.Fprintf(w, " %02x", vm.lb(Cell(i+j)))
		}
		fmt.Fprintf(w, "  ")
		for j := uint32(0); j < 16; j++ {
			c := vm.lb(Cell(i + j))
			if !(' ' <= c && c <= '~') {
				c = '.'
			}
			fmt.Fprintf(w, "%c", c)
		}
		fmt.Fprintf(w, "\n")
	}
}

func (i Inst) String() string {
	const mask = Cell(^0)

	mc := fmt.Sprintf("% 02x", i.Enc[:i.Len])
	op := "halt"
	arg := "cc"
	if i.Op < Cell(len(openc)) && openc[i.Op] != "" {
		enc := strings.Split(openc[i.Op], " ")
		op = enc[0]
		arg = ""
		if len(enc) > 1 {
			a1 := i.A1
			a2 := i.A2
			switch op {
			case "call", "jumpfwd", "jumpback", "jmpfalse", "jmptrue",
				"for", "fordown":
				a1 += i.I + 5
			}
			a1 &= mask
			a2 &= mask

			switch len(enc[1]) {
			case 2:
				arg += fmt.Sprintf("%x %x", a1, a2)
			case 1:
				arg += fmt.Sprintf("%x", a1)
			default:
				panic("unreachable")
			}
		}
	}

	return fmt.Sprintf("%-17s %-10s %-12s", mc, op, arg)
}

func nextpow2(x uint32) uint32 {
	p := uint32(1)
	for p < x {
		p <<= 1
	}
	return p
}

func memcmp(a, b []byte) Cell {
	for i := range a {
		if i >= len(b) {
			break
		}
		if a[i] != b[i] {
			return Cell(a[i]) - Cell(b[i])
		}
	}
	return 0
}
