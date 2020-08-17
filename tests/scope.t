! This language does not have block scope, it has global, local and level scope
! The compiler C implementation has different behavior than the T3X9 implementation

! The C behavior is to allow one to redeclare a global variable name inside a block
! but not the T3X9 implementation, we adopt the C behavior here

! global scope, every block below and function can see it, but they can
! override the definition in their own block
var au;

! function scope, the block scope in do..end is shared with the function
! scope also
f(x)
do
	! this will error out
	!var x;
	var uzi; 
end

g(x)
do
	const y = 5, z = 1;
end

l(p)
do
	do
		var x;
		do var u; end
		do var u; end
		do var u; end
		do
			do
				const b = 450;
			end
		end
	end
end

var XYZ;
var QUT[3452];
var OXL::3563;

! this is a block, inside a block, outer declarations are also matched
! against new declarations for redefine errors, however if they are at
! same level scope, then it is valid
do
	var x;
	do
		var a;
	end
	do
		var a;
	end
end
