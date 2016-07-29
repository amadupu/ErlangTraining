.SUFFIXES: .erl .beam

EBIN 	= ebin
SRC  	= src
INCLUDE = include
PRIV    = priv

EFLAGS	= -I include

$(EBIN)/%.beam:$(SRC)/%.erl
	erlc -W -b beam -o $(EBIN) $(EFLAGS) $<

ERL = erl -boot start_clean

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)
OBJS = $(SOURCE:%.erl=$(EBIN)/%.beam)

SOURCE = client.erl \
	controller.erl \
	dbservice.erl
       
all: $(TARGETS)

install: $(TARGETS)
	$(ERL) -pa $(EBIN) -mnesia dir $(PRIV) -s dbservice install [[node()]]

app: $(TARGETS)
	$(ERL) -pa $(EBIN) -mnesia dir $(PRIV)

node1: $(TARGETS)
	$(ERL) -pa $(EBIN) -sname masternode1 
node2: $(TARGETS)
	$(ERL) -pa $(EBIN) -sname masternode2 

dnode1: $(TARGETS)
	$(ERL) -pa $(EBIN) -name masterdnode1@erlang.examples.com -setcookie  shoretel
dnode2: $(TARGETS)
	$(ERL) -pa $(EBIN) -name masterdnode2@erlang.examples.com -setcookie  shoretel
clean:
	rm -f $(OBJS) erl_crash.dump
