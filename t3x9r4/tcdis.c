/*
 * Tcode9 disassembler
 * Nils M Holm, 2017, 0BSD license
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#define MEMSIZE	131072

#define byte	unsigned char
#define sbyte	signed char
#define cell	int

byte	*M;
sbyte	*S;
cell	Red;
cell	Data;

void writes(char *s) {
	write(1, s, strlen(s));
}

void wlog(char *s) {
	write(1, s, strlen(s));
}

void fail(char *s) {
	wlog("tcdis: ");
	wlog(s);
	wlog("\n");
	_exit(1);
}

#define BSIZE	1024

char	B[BSIZE];
int	Fd, C, K;

int rdch(void) {
	if (C >= K) {
		K = read(Fd, B, BSIZE);
		C = 0;
		if (K < 1) return -1;
	}
	C = C+1;
	return B[C-1] & 255;
}

cell rdwd(void) {
	cell	v;

	v = rdch();
	v = v | (rdch() << 8);
	v = v | (rdch() << 16);
	v = v | (rdch() << 24);
	return v;
}

void readblk(byte *v, int k) {
	int	i, c;

	for (i=0; i<k; i++) {
		c = rdch();
		if (-1 == c) fail("file too short");
		v[i] = c & 255;
	}
}

void load(char *s) {
	int	c;
	cell	tp, dp;

	Fd = open(s, O_RDONLY);
	if (Fd < 0) fail("could not open program");
	C = 0;
	K = 0;
	c = rdch();
	while (c != '\n' && c != -1)
		c = rdch();
	if (rdwd() != 0x39583354) fail("not a tcvm program");
	tp = rdwd();
	dp = rdwd();
	Data = tp;
	Red = tp + dp;
	M = malloc(MEMSIZE);
	S = (sbyte *) M;
	if (NULL == M) fail("not enough memory");
	readblk(M, tp+dp);
	if (rdch() != -1) fail("trailing garbage");
	close(Fd);
}

cell	A, F, I, P;

cell w(cell a) {
	return  M[a+0]
	     | (M[a+1] << 8)
	     | (M[a+2] << 16)
	     | (M[a+3] << 24);
}

#define a()	w(I+1)
#define a2()	w(I+5)
#define s()	S[I+1]
#define s2()	w(I+2)

void decode(void) {
	cell	j;

	for (I=0; I < Data; I++) {
	printf("%07x ", I);
	switch (M[I]) {
	case 0x80:
	case 0x00: printf("push"); break;
	case 0x81:
	case 0x01: printf("clear"); break;
	case 0x82: printf("ldval\t%x", s()); I++; break;
	case 0x02: printf("ldval\t%x", a()); I += 4; break;
	case 0x83:
	case 0x03: printf("ldaddr\t%x", a()); I += 4; break;
	case 0x84: printf("ldlref\t%x", s()); I++; break;
	case 0x04: printf("ldlref\t%x", a()); I += 4; break;
	case 0x85:
	case 0x05: printf("ldglob\t%x", a()); I += 4; break;
	case 0x86: printf("ldlocl\t%x", s()); I++; break;
	case 0x06: printf("ldlocl\t%x", a()); I += 4; break;
	case 0x87:
	case 0x07: printf("stglob\t%x", a()); I += 4; break;
	case 0x08: printf("stlocl\t%x", a()); I += 4; break;
	case 0x88: printf("stlocl\t%x", s()); I++; break;
	case 0x89:
	case 0x09: printf("stindr"); break;
	case 0x8a:
	case 0x0a: printf("stindb"); break;
	case 0x8b:
	case 0x0b: printf("incglob\t%x %x", a(), a2()); I += 8; break;
	case 0x8c: printf("inclocl\t%x %x", s(), s2()); I += 5; break;
	case 0x0c: printf("inclocl\t%x %x", a(), a2()); I += 8; break;
	case 0x8d: printf("alloc\t%x", s()); I++; break;
	case 0x0d: printf("alloc\t%x", a()); I += 4; break;
	case 0x8e: printf("dealloc\t%x", s()); I++; break;
	case 0x0e: printf("dealloc\t%x", a()); I += 4; break;
	case 0x8f:
	case 0x0f: printf("loclvec"); break;
	case 0x90:
	case 0x10: printf("globvec\t%x", a()); I += 4; break;
	case 0x91:
	case 0x11: printf("index"); break;
	case 0x92:
	case 0x12: printf("deref"); break;
	case 0x93:
	case 0x13: printf("indxb"); break;
	case 0x94:
	case 0x14: printf("drefb"); break;
	/* case 0x95: MARK */
	/* case 0x15: MARK */
	/* case 0x96: RESOLV */
	/* case 0x16: RESOLV */
	case 0x97:
	case 0x17: printf("call\t%x", a()+I+5); I += 4; break;
	case 0x98:
	case 0x18: printf("jumpfwd\t%x", a()+I+5); I += 4; break;
	case 0x99:
	case 0x19: printf("jumpback\t%x", a()+I+5); I += 4; break;
	case 0x9a:
	case 0x1a: printf("jmpfalse\t%x", a()+I+5); I += 4; break;
	case 0x9b:
	case 0x1b: printf("jmptrue\t%x", a()+I+5); I += 4; break;
	case 0x9c:
	case 0x1c: printf("for\t%x", a()+I+5); I += 4; break;
	case 0x9d:
	case 0x1d: printf("fordown\t%x", a()+I+5); I += 4; break;
	case 0x9e:
	case 0x1e: printf("enter"); break;
	case 0x9f:
	case 0x1f: printf("exit"); break;
	case 0xa0: printf("halt\t%x", s()); I++; break;
	case 0x20: printf("halt\t%x", a()); I += 4; break;
	case 0xa1:
	case 0x21: printf("neg"); break;
	case 0xa2:
	case 0x22: printf("inv"); break;
	case 0xa3:
	case 0x23: printf("lognot"); break;
	case 0xa4:
	case 0x24: printf("add"); break;
	case 0xa5:
	case 0x25: printf("sub"); break;
	case 0xa6:
	case 0x26: printf("mul"); break;
	case 0xa7:
	case 0x27: printf("div"); break;
	case 0xa8:
	case 0x28: printf("mod"); break;
	case 0xa9:
	case 0x29: printf("and"); break;
	case 0xaa:
	case 0x2a: printf("or"); break;
	case 0xab:
	case 0x2b: printf("xor"); break;
	case 0xac:
	case 0x2c: printf("shl"); break;
	case 0xad:
	case 0x2d: printf("shr"); break;
	case 0xae:
	case 0x2e: printf("eq"); break;
	case 0xaf:
	case 0x2f: printf("neq"); break;
	case 0xb0:
	case 0x30: printf("lt"); break;
	case 0xb1:
	case 0x31: printf("gt"); break;
	case 0xb2:
	case 0x32: printf("le"); break;
	case 0xb3:
	case 0x33: printf("ge"); break;
	/* case 0xb4: WORD */
	/* case 0x34: WORD */
	case 0xb5:
	case 0x35: printf("syscall\t%x", M[I+1]); I++; break;
	default:   fail("invalId opcode"); break;
	}
	putchar('\n');
	fflush(stdout);
	}
	putchar('\n');
	for (; I < Red; I += 16) {
		printf("%07x ", I);
		for (j=0; j<16; j++) {
			if (j && j%4 == 0) putchar(' ');
			printf(" %02x", M[I+j]);
		}
		printf("  ");
		for (j=0; j<16; j++)
			printf("%c",
			  ' ' <= M[I+j] && M[I+j] <= '~'? M[I+j]: '.');
		putchar('\n');
	}
}

cell main(cell argc, char **argv) {
	if (argc != 2) fail("usage: tcdis program");
	load(argv[1]);
	decode();
	return 0;
}
