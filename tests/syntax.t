! basic syntax checking
! this is a comment
! more comments

! and another
! and some more

struct Point = PX, PY, PZ;

const A = 30;
const b = 40;
const x = 1, y = 2, z = 1+b;

var Q[40], R::2, S::A, T[b];

decl foo(10), gen(20), thirty(10);

foo(q1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
do
end

another_one(unit)
do
	var uz, uy, uq, northern.lights12;
	do
		const u0 = 1;
	end
end

do
	var p, l1, l2, l3;
	p := (1 /\ 4 \/ 34) / (45*24 mod 5) /\ (1 \/ (1 /\ 4 mod 56 * 45));
	p := 1 /\ 2 /\ 3 /\ 4 /\ 5 /\ 6 /\ 7 /\ 8 /\ 9 \/ 44 /\ 5 /\ 6 \/ 5;
	do
		do
			const G1 = 4, G2 = 5, G3 = 6;
			do
			end
		end
	end
	! comment inside of compound

	! location of the place
	l1 := 2 + 3 + 5 * 2 / 4 mod 3;
	another_one(1);
end

