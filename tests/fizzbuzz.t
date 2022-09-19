var ntoa_buf::100;

ntoa(x) do var i, k;
	if (x = 0) return "0";
	i := 0;
	k := x<0-> -x: x;
	while (k > 0) do
		i := i+1;
		k := k/10;
	end
	i := i+1;
	if (x < 0) i := i+1;
	ntoa_buf::i := 0;
	k := x<0-> -x: x;
	while (k > 0) do
		i := i-1;
		ntoa_buf::i := '0' + k mod 10;
		k := k/10;
	end
	if (x < 0) do
		i := i-1;
		ntoa_buf::i := '-';
	end
	return @ntoa_buf::i;
end

str.length(s) return t.memscan(s, 0, 32767);

writes(s) t.write(1, s, str.length(s));

fib(n) do var r1, r2, i, t;
	r1 := 0;
	r2 := 1;
	for (i=1, n) do
		t := r2;
		r2 := r2 + r1;
		r1 := t;
	end
	return r2;
end 

do var i;
	for (i=1, 101) do
		if (i mod 3 \= 0 /\ i mod 5 \= 0)
			writes(ntoa(i));
		if (i mod 3 = 0)
			writes("Fizz ");
		if (i mod 5 = 0)
			writes("Buzz ");
		writes("\n");
	end
end
