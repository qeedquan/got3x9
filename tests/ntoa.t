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

do
	ntoa(100);
end
