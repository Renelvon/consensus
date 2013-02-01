#include <stdlib.h>
#include <unistd.h>

#include "msgconf.h"
#include "calcs.h"
#include "inits.h"
#include "sync.h"
#include "traitor.h"

// plays a traitor general
void play_traitor(int gen_id,mbi_t *mbis,mbo_t mbo,int gen_no,role_t *roles,int tra_no)
{
	sig_t sig=getpid();
	int loy_no=gen_no-tra_no;
	mbi_t *loys=(mbi_t*)malloc(loy_no*sizeof(mbi_t));
	for(int x=0,i=0;i<gen_no;i++)
		if(roles[i]=='L') loys[x++]=mbis[i+1];
	int tra_idx=0;
	for(int i=0;i<gen_id;i++)
		if(roles[i]=='T') tra_idx++;
	int rounds=max_traitors(gen_no)+1;
	for(int round=1;round<=rounds;round++)
	{
		if(roles[round-1]=='T')
		{
			sync_general(mbis[0],mbo,sig);
			play_traitor_Tph1(loys,mbo,loy_no,sig,tra_idx);
			sync_general(mbis[0],mbo,sig);
			play_traitor_Tph2(loys,mbo,loy_no,sig);
			sync_general(mbis[0],mbo,sig);
			play_traitor_Tph3(gen_id,loys,loy_no,gen_no,sig,round);
		}
		else
		{
			int k_id=-1;
			for(int i=0;i<round;i++)
				if(roles[i]=='L') k_id++;
			sync_general(mbis[0],mbo,sig);
			cnt_t cnt=play_traitor_Lph1(loys,mbo,loy_no,gen_no,sig,tra_idx,k_id);
			sync_general(mbis[0],mbo,sig);
			play_traitor_Lph2(loys,mbo,loy_no,gen_no,sig,cnt,k_id);
			sync_general(mbis[0],mbo,sig);
			play_traitor_Lph3(mbo);
		}
	}
	free(loys);
	exit(0);
}

// plays phase #1 of traitors' protocol (Traitor King)
void play_traitor_Tph1(mbi_t *loys,mbo_t mbo,int loy_no,sig_t sig,int tra_idx)
{
	msg_t *msgs=receive_messages(mbo,loy_no);
	cnt_t cnt=count_votes(msgs,loy_no);
	vote_t vote=vote_minority(cnt,tra_idx);
	msg_t msg;
	msg.sig=sig;
	msg.data=vote;
	send_messages(loys,loy_no,msg);
	free(msgs);
	return;
}

// plays phase #2 of traitors' protocol (Traitor King)
void play_traitor_Tph2(mbi_t *loys,mbo_t mbo,int loy_no,sig_t sig)
{
	msg_t *msgs=receive_messages(mbo,loy_no);
	vote_t vote=2;
	msg_t msg;
	msg.sig=sig;
	msg.data=vote;
	send_messages(loys,loy_no,msg);
	free(msgs);
	return;
}

// plays phase #3 of traitors' protocol (Traitor King)
void play_traitor_Tph3(int gen_id,mbi_t *loys,int loy_no,int gen_no,sig_t sig,int round)
{
	if(gen_id==round)
	{
		if(round<=max_traitors(gen_no))
		{
			vote_t vote=1;
			msg_t msg;
			msg.sig=sig;
			msg.data=vote;
			send_messages(loys,loy_no,msg);
		}
		else
		{
			vote_t vote=1;
			msg_t msg;
			msg.sig=sig;
			msg.data=vote;
			send_messages(loys+1,loy_no-1,msg);
			vote=0;
			msg.data=vote;
			send_message(loys[0],msg);
		}
	}
	return;
}

// plays phase #1 of traitors' protocol (Loyal King)
cnt_t play_traitor_Lph1(mbi_t *loys,mbo_t mbo,int loy_no,int gen_no,sig_t sig,int tra_idx,int k_id)
{
	msg_t *msgs=receive_messages(mbo,loy_no);
	cnt_t cnt=count_votes(msgs,loy_no);
	if(cnt.v1>=simple_majority(gen_no))
	{
		int needed_bot_no=super_majority(gen_no)-gen_no+loy_no;
		int bot_no=(needed_bot_no<=loy_no-1)?needed_bot_no:loy_no-1;
		int rest_no=loy_no-bot_no;
		mbi_t *bots=(mbi_t*)malloc(bot_no*sizeof(mbi_t));
		mbi_t *rest=(mbi_t*)malloc(rest_no*sizeof(mbi_t));
		for(int b=0,r=0,i=0;i<loy_no;i++)
			if(b<bot_no && i!=k_id) bots[b++]=loys[i];
			else rest[r++]=loys[i];
		vote_t vote=1;
		msg_t msg;
		msg.sig=sig;
		msg.data=vote;
		send_messages(bots,bot_no,msg);
		vote=vote_minority(cnt,tra_idx);
		msg.data=vote;
		send_messages(rest,rest_no,msg);		
		free(bots); free(rest);
	}
	else
	{
		vote_t vote=vote_minority(cnt,tra_idx);
		msg_t msg;
		msg.sig=sig;
		msg.data=vote;
		send_messages(loys,loy_no,msg);
	}
	free(msgs);
	return cnt;
}

// plays phase #2 of traitors' protocol (Loyal King)
void play_traitor_Lph2(mbi_t *loys,mbo_t mbo,int loy_no,int gen_no,sig_t sig,cnt_t cnt,int k_id)
{
	msg_t *msgs=receive_messages(mbo,loy_no);
	if(cnt.v1>=simple_majority(gen_no))
	{
		int needed_bot_no=super_majority(gen_no)-gen_no+loy_no;
		int bot_no=(needed_bot_no<=loy_no-1)?needed_bot_no:loy_no-1;
		int rest_no=loy_no-bot_no;
		mbi_t *bots=(mbi_t*)malloc(bot_no*sizeof(mbi_t));
		mbi_t *rest=(mbi_t*)malloc(rest_no*sizeof(mbi_t));
		for(int b=0,r=0,i=0;i<loy_no;i++)
			if(b<bot_no && i!=k_id) bots[b++]=loys[i];
			else rest[r++]=loys[i];
		vote_t vote=1;
		msg_t msg;
		msg.sig=sig;
		msg.data=vote;
		send_messages(bots,bot_no,msg);
		vote=0;
		msg.data=vote;
		send_messages(rest,rest_no,msg);
		free(bots); free(rest);
	}
	else
	{
		vote_t vote=2;
		msg_t msg;
		msg.sig=sig;
		msg.data=vote;
		send_messages(loys,loy_no,msg);
	}
	free(msgs);
	return;
}

// plays phase #3 of traitors' protocol (Loyal King)
void play_traitor_Lph3(mbo_t mbo)
{
	receive_message(mbo);
	return;
}
