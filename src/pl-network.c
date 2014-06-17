/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-fli.h"
#include "pl-atom.h"

#include <netdb.h>
#include <netinet/in.h>

static int PL_unify_ip(union cell *Ip, const struct in_addr *addr)
{
	unsigned long ip;
	unsigned char a[4];
	int i;

	ip = ntohl(addr->s_addr);
	for (i = 0; i < 4; i++) {
		a[i] = (ip >> 8 * i) & 0xff;
	}

	Ip = deref(Ip);
	if (is_var(Ip)) {
		union cell *f = new_struct(FUN(_ip, 4), 4);
		for (i = 0; i < 4; i++) {
			PL_put_integer(f + 1 + i, a[3 - i]);
		}
		mkrefp(Ip, f);
		trail(Ip);
		succeed;
	} else if (isfun(FUN(_ip, 4), Ip)) {
		for (i = 0; i < 4; i++) {
			if (!PL_unify_intg(Ip + 1 + i, a[i]))
				fail;
		}
		succeed;
	} else
		fail;
}

static int PL_get_ip(union cell *Ip, struct in_addr *addr)
{
	unsigned long ip = 0;

	Ip = deref(Ip);
	if (isfun(FUN(_ip, 4), Ip)) {
		int i;

		for (i = 0; i < 4; i++) {
			int n;
			if (!PL_get_intg(Ip + 1 + i, &n))
				fail;
			ip = (ip << 8) | n;
		}

		addr->s_addr = htonl(ip);
		succeed;
	}

	fail;
}

int pl_host_to_addr(union cell *Host, union cell *Addr)
{
	const char *name;
	struct in_addr addr;
	struct hostent *host;

	if (PL_get_atom_chars(Host, &name)) {
		struct in_addr *addr;

		host = gethostbyname(name);
		if (!host || host->h_length != sizeof(struct in_addr)) {	// FIXME : errmsg
			fprintf(stderr, "gethostbyname() -> %d\n", h_errno);
			fail;
		}
		addr = (struct in_addr *)host->h_addr;
		return (PL_unify_ip(Addr, addr));
	}

	if (PL_get_ip(Addr, &addr)) {
		host = gethostbyaddr((char *)&addr, sizeof(addr), AF_INET);
		if (!host) {		// FIXME : errmsg
			fprintf(stderr, "gethostbyaddr() -> %d\n", h_errno);
			fail;
		}

		return (PL_unify_atom_chars(Host, host->h_name));
	}

	fail;
}

/********************************************************/
