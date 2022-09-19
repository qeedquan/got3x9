! length limited to 32766 bytes
str.length(s) return t.memscan(s, 0, 32767);

str.copy(sd, ss) t.memcopy(ss, sd, str.length(ss)+1);

str.append(sd, ss)
    t.memcopy(ss, @sd::str.length(sd), str.length(ss)+1);

str.equal(s1, s2)
    return t.memcomp(s1, s2, str.length(s1)+1) = 0;

writes(s) t.write(1, s, str.length(s));

do
	writes("strings\n");
end
