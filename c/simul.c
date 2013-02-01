#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include "msgconf.h"
#include "calcs.h"
#include "inits.h"
#include "sync.h"
#include "ioconf.h"

int main(int argc,char *argv[])
{
	srand(time(NULL));
	int gen_no,tra_no;
	vote_t *votes=parse_input(&gen_no,&tra_no);
	int loy_no=gen_no-tra_no;
	print_input(gen_no,loy_no,tra_no,votes);
	mb_t *mbs=create_mailboxes(gen_no+1);
	mbi_t *mbis=mailbox_addresses(mbs,gen_no+1);
	mbo_t mbo=mbs[0].mbo;
	role_t *roles=init_roles(gen_no,tra_no);
	gen_t *gens=create_generals(roles,mbs,mbis,gen_no,tra_no,votes);
	sig_t sig=getpid();
	int rounds=max_traitors(gen_no)+1;
	for(int round=1;round<=rounds;round++)
	{
		sync_phase_wait(mbo,gen_no);
		print_round(round,rounds);
		print_phase(1,3);
		sync_phase_signal(mbis,sig,gen_no);
		sync_phase_wait(mbo,gen_no);
		print_phase(2,3);
		sync_phase_signal(mbis,sig,gen_no);
		sync_phase_wait(mbo,gen_no);
		print_phase(3,3);
		sync_phase_signal(mbis,sig,gen_no);
	}
	votes=get_final_votes(mbo,loy_no);
	print_output(loy_no,votes);
	free(votes);
	destroy_mailboxes(mbs,gen_no+1);
	free(mbs); free(mbis);
	free(gens); free(roles);
	return 0;
}
