#include <stdlib.h>
#include <unistd.h>

#include "msgconf.h"
#include "calcs.h"
#include "sync.h"
#include "loyal.h"

// plays a loyal general
void play_loyal(int gen_id,mbi_t *mbis,mbo_t mbo,int gen_no,vote_t vote)
{
	sig_t sig=getpid();
	int rounds=max_traitors(gen_no)+1;
	for(int round=1;round<=rounds;round++)
	{
		sync_general(mbis[0],mbo,sig);
		play_loyal_ph1(mbis,mbo,gen_no,sig,&vote);
		sync_general(mbis[0],mbo,sig);
		cnt_t cnt=play_loyal_ph2(mbis,mbo,gen_no,sig,&vote);
		sync_general(mbis[0],mbo,sig);
		play_loyal_ph3(gen_id,mbis,mbo,gen_no,sig,&vote,cnt,round);
	}
	msg_t msg;
	msg.sig=sig;
	msg.data=vote;
	send_message(mbis[0],msg);
	exit(0);
}

// plays phase #1 of generals' protocol
void play_loyal_ph1(mbi_t *mbis,mbo_t mbo,int gen_no,sig_t sig,vote_t *vote)
{
	msg_t msg;
	msg.sig=sig;
	msg.data=*vote;
	send_messages(mbis+1,gen_no,msg);
	msg_t *msgs=receive_messages(mbo,gen_no);
	cnt_t cnt=count_votes(msgs,gen_no);
	*vote=decide_vote(cnt,super_majority(gen_no));
	free(msgs);
	return;
}

// plays phase #2 of generals' protocol
cnt_t play_loyal_ph2(mbi_t *mbis,mbo_t mbo,int gen_no,sig_t sig,vote_t *vote)
{
	msg_t msg;
	msg.sig=sig;
	msg.data=*vote;
	send_messages(mbis+1,gen_no,msg);
	msg_t *msgs=receive_messages(mbo,gen_no);
	cnt_t cnt=count_votes(msgs,gen_no);
	*vote=decide_vote(cnt,simple_majority(gen_no));
	free(msgs);
	return cnt;
}

// plays phase #3 of generals' protocol
void play_loyal_ph3(int gen_id,mbi_t *mbis,mbo_t mbo,int gen_no,sig_t sig,vote_t *vote,cnt_t cnt,int round)
{
	if(gen_id==round)
	{
		msg_t msg;
		msg.sig=sig;
		msg.data=*vote;
		send_messages(mbis+1,gen_no,msg);
	}
	msg_t msg=receive_message(mbo);
	int sm=super_majority(gen_no);
	if(*vote==2 || (*vote==0&&cnt.v0<sm) || (*vote==1&&cnt.v1<sm))
		*vote=(msg.data<1)?0:1;
	return;
}
