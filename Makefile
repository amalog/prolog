all: bin/ama

bin/ama: bin/ama.pl prolog/*.pl prolog/amalog/*.pl
	swipl -f dev.pl -q -t main -o bin/ama -c bin/ama.pl
