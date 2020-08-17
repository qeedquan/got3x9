package main

import (
	"bytes"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"text/scanner"

	"t3x9/ast"
	"t3x9/lex"
	"t3x9/parse"
	"t3x9/tc"
)

var flags struct {
	Disasm      bool
	DumpCode    bool
	DumpLexemes bool
	DumpAST     bool
	RunProgram  bool
	Verbose     int
	MaxErrors   int
	OutputFile  string
}

func main() {
	log.SetPrefix("tc: ")
	log.SetFlags(0)
	parseFlags()

	f, err := os.Open(flag.Arg(0))
	ck(err)
	defer f.Close()

	switch {
	case flags.Disasm || flags.DumpCode:
		dumpExec(f)
	case flags.DumpLexemes:
		dumpLexemes(f)
	case flags.DumpAST:
		dumpAST(f)
	default:
		b, err := ioutil.ReadAll(f)
		ck(err)
		err = runVM(b)
		if err != nil {
			compileCode(f.Name(), b)
		}
	}
}

func ck(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func usage() {
	fmt.Fprintln(os.Stderr, "usage: tc [options] file")
	flag.PrintDefaults()
	os.Exit(2)
}

func parseFlags() {
	flags.OutputFile = "a.vm"
	flags.MaxErrors = 10

	flag.Usage = usage
	flag.BoolVar(&flags.Disasm, "d", flags.Disasm, "disassemble executable")
	flag.BoolVar(&flags.DumpCode, "c", flags.DumpCode, "dump code portion of executable")
	flag.BoolVar(&flags.DumpLexemes, "l", flags.DumpLexemes, "dump lexemes")
	flag.BoolVar(&flags.DumpAST, "T", flags.DumpAST, "dump ast")
	flag.BoolVar(&flags.RunProgram, "r", flags.RunProgram, "run program after compiling")
	flag.IntVar(&flags.Verbose, "v", flags.Verbose, "verbosity level")
	flag.IntVar(&flags.MaxErrors, "b", flags.MaxErrors, "max errors before bailing out")
	flag.StringVar(&flags.OutputFile, "o", flags.OutputFile, "output file")
	flag.Parse()
	if flag.NArg() != 1 {
		usage()
	}
}

func dumpExec(f *os.File) {
	vm := tc.NewVM(0, 0)
	vm.Verbose = flags.Verbose
	err := vm.Load(f)
	ck(err)

	if flags.Disasm {
		vm.Objdump(os.Stdout)
	} else {
		fmt.Printf(".byte ")
		for i := uint32(0); i < vm.MR; i++ {
			if i > 0 && i%10 == 0 {
				fmt.Printf("\n.byte ")
			}
			fmt.Printf("%#02x", vm.M[i])
			if (i+1)%10 != 0 && i+1 < vm.MR {
				fmt.Printf(",")
			}
		}
		fmt.Println()
	}
}

func runVM(buf []byte) error {
	vm := tc.NewVM(0, 0)
	vm.Verbose = flags.Verbose
	err := vm.Load(bytes.NewBuffer(buf))
	if err != nil {
		return err
	}

	err = vm.Run()
	if err != nil {
		return err
	}
	return nil
}

func dumpLexemes(f *os.File) {
	errh := func(pos scanner.Position, err error) {
		fmt.Printf("%v: %v\n", pos, err)
	}

	var lx lex.Lexer
	lx.Init(f.Name(), f, errh)
	for {
		pos, tok, lit := lx.Next()
		if tok == lex.EOF {
			break
		}
		fmt.Printf("%v: %v %q\n", pos, tok, lit)
	}
}

func dumpAST(f *os.File) {
	var lx lex.Lexer
	var pr parse.Parser
	var nerr int

	defer func() {
		if e := recover(); e != nil {
			fmt.Println(e)
			return
		}
	}()

	errh := func(pos scanner.Position, err error) {
		if nerr++; nerr > flags.MaxErrors {
			panic("too many errors, aborting!")
		}
		fmt.Printf("%v: %v\n", pos, err)
	}

	mode := parse.Mode(0)
	if flags.Verbose > 1 {
		mode |= parse.DeclarationErrors
	}

	lx.Init(f.Name(), f, errh)
	pr.Init(&lx, errh, mode)
	prog := pr.Parse()
	if nerr == 0 {
		ast.Print(prog)
	}
}

type bailout struct{}

func compileCode(name string, buf []byte) {
	var lx lex.Lexer
	var pr parse.Parser
	var nerr int

	defer func() {
		if e := recover(); e != nil {
			if _, ok := e.(bailout); !ok {
				panic(e)
			}
			fmt.Println("too many errors, aborting!")
		}
	}()

	errh := func(pos scanner.Position, err error) {
		if nerr++; nerr > flags.MaxErrors {
			panic(bailout{})
		}
		fmt.Printf("%v: %v\n", pos, err)
	}

	lx.Init(name, bytes.NewBuffer(buf), errh)
	pr.Init(&lx, errh, parse.DeclarationErrors)
	prog := pr.Parse()
	if prog == nil || nerr > 0 {
		return
	}

	cg := tc.NewCG()
	if flags.RunProgram {
		b := new(bytes.Buffer)
		cg.Compile(b, prog)
		runVM(b.Bytes())
	} else {
		f, err := os.Create(flags.OutputFile)
		ck(err)

		cg.Compile(f, prog)
		ck(f.Close())
	}
}
