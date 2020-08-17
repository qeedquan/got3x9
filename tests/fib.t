str.length(s) return t.memscan(s, 0, 32767);

str.copy(sd, ss) t.memcopy(ss, sd, str.length(ss)+1);

str.append(sd, ss)
    t.memcopy(ss, @sd::str.length(sd), str.length(ss)+1);

str.equal(s1, s2)
    return t.memcomp(s1, s2, str.length(s1)+1) = 0;

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

writes(s) t.write(1, s, str.length(s));

fib(n) do var x1, x2, i, t;
    x1 := 0;
    x2 := 1;
    for (i=1, n) do
        t := x2;
        x2 := x2 + x1;
        x1 := t;
    end
    return x2;
end 

do var i;
    for (i=1, 11) do
        writes(ntoa(fib(i)));
        writes("\n");
    end
end
