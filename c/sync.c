#include <stdlib.h>

#include "msgconf.h"

// suspends until all generals are ready
void sync_phase_wait(mbo_t mbo,int gen_no)
{
	msg_t *msgs=receive_messages(mbo,gen_no);
	free(msgs);
	return;
}

// alerts generals to start the next phase
void sync_phase_signal(mbi_t *mbis,sig_t sig,int gen_no)
{
	msg_t msg;
	msg.sig=sig;
	msg.data='K';
	send_messages(mbis+1,gen_no,msg);
	return;
}

// suspends a general until alerted
void sync_general(mbi_t mbi,mbo_t mbo,sig_t sig)
{
	msg_t msg;
	msg.sig=sig;
	msg.data='R';
	send_message(mbi,msg);
	msg=receive_message(mbo);
	return;
}
