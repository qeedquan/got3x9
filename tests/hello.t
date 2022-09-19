str.length(s)
	return t.memscan(s, 0, 32767);

log(s)
do
	t.write(1, s, str.length(s));
end

do
	var i, text;
	text := "Hello, World!\n";
	for (i = 0, 10, 1)
	do
		log(text);
	end
	do
		for (i = 0, 1000, 1)
		do
			log("Goodbye, World!\n");
			leave;
		end
	end
	while (1)
	do
		log("Program Finished\n");
		leave;
	end
end

