var a;
var b::3;
var	Codetbl;

struct	CG =	CG_PUSH, CG_CLEAR,
		CG_LDVAL, CG_LDADDR, CG_LDLREF, CG_LDGLOB,
		CG_LDLOCL,
		CG_STGLOB, CG_STLOCL, CG_STINDR, CG_STINDB,
		CG_INCGLOB, CG_INCLOCL,
		CG_ALLOC, CG_DEALLOC, CG_LOCLVEC, CG_GLOBVEC,
		CG_INDEX, CG_DEREF, CG_INDXB, CG_DREFB,
		CG_MARK, CG_RESOLV,
		CG_CALL, CG_JUMPFWD, CG_JUMPBACK, CG_JMPFALSE,
		CG_JMPTRUE, CG_FOR, CG_FORDOWN,
		CG_ENTER, CG_EXIT, CG_HALT,
		CG_NEG, CG_INV, CG_LOGNOT, CG_ADD, CG_SUB,
		CG_MUL, CG_DIV, CG_MOD, CG_AND, CG_OR, CG_XOR,
		CG_SHL, CG_SHR, CG_EQ, CG_NEQ, CG_LT, CG_GT,
		CG_LE, CG_GE,
		CG_WORD;

do
	Codetbl := [
		[ CG_PUSH,	"50"			],
		[ CG_CLEAR,	"31c0"			],
		[ CG_LDVAL,	"b8,w"			],
		[ CG_LDADDR,	"b8,a"			],
		[ CG_LDLREF,	"8d85,w"		],
		[ CG_LDGLOB,	"a1,a"			],
		[ CG_LDLOCL,	"8b85,w"		],
		[ CG_STGLOB,	"a3,a"			],
		[ CG_STLOCL,	"8985,w"		],
		[ CG_STINDR,	"5b8903"		],
		[ CG_STINDB,	"5b8803"		],
		[ CG_INCGLOB,	"8105,a"		],
		[ CG_INCLOCL,	"8185,w"		],
		[ CG_ALLOC,	"81ec,w"		],
		[ CG_DEALLOC,	"81c4,w"		],
		[ CG_LOCLVEC,	"89e050"		],
		[ CG_GLOBVEC,	"8925,a"		],
		[ CG_INDEX,	"c1e0025b01d8"		],
		[ CG_DEREF,	"8b00"			],
		[ CG_INDXB,	"5b01d8"		],
		[ CG_DREFB,	"89c331c08a03"		],
		[ CG_MARK,	",m"			],
		[ CG_RESOLV,	",r"			],
		[ CG_CALL,	"e8,w"			],
		[ CG_JUMPFWD,	"e9,>"			],
		[ CG_JUMPBACK,	"e9,<"			],
		[ CG_JMPFALSE,	"09c00f84,>"		],
		[ CG_JMPTRUE,	"09c00f85,>"		],
		[ CG_FOR,	"5b39c30f8d,>"		],
		[ CG_FORDOWN,	"5b39c30f8e,>"		],
		[ CG_ENTER,	"5589e5"		],
		[ CG_EXIT,	"5dc3"			],
		[ CG_HALT,	"68,w5031c040cd80"	],
		[ CG_NEG,	"f7d8"			],
		[ CG_INV,	"f7d0"			],
		[ CG_LOGNOT,	"f7d819c0f7d0"		],
		[ CG_ADD,	"5b01d8"		],
		[ CG_SUB,	"89c35829d8"		],
		[ CG_MUL,	"5bf7eb"		],
		[ CG_DIV,	"89c35899f7fb"		],
		[ CG_MOD,	"89c35899f7fb89d0"	],
		[ CG_AND,	"5b21d8"		],
		[ CG_OR,	"5b09d8"		],
		[ CG_XOR,	"5b31d8"		],
		[ CG_SHL,	"89c158d3e0"		],
		[ CG_SHR,	"89c158d3e8"		],
		[ CG_EQ,	"5b39c30f95c20fb6c248"	],
		[ CG_NEQ,	"5b39c30f94c20fb6c248"	],
		[ CG_LT,	"5b39c30f9dc20fb6c248"	],
		[ CG_GT,	"5b39c30f9ec20fb6c248"	],
		[ CG_LE,	"5b39c30f9fc20fb6c248"	],
		[ CG_GE,	"5b39c30f9cc20fb6c248"	],
		[ CG_WORD,	",w"			],
		[ %1,		""			] ];
	
	a[3][4][5][a[4] * (6*46) mod 2]::234 := 2;
	b::134 := a[3][5][6][4]::1;
end
