#include <stdlib.h>

#include "msgconf.h"
#include "calcs.h"

// calcs maximum traitors unable to break the protocol
int max_traitors(int gen_no)
{
	return (gen_no-1)/3;
}

// calcs minimum votes needed for super majority
int super_majority(int gen_no)
{
	return gen_no-max_traitors(gen_no);
}

// calcs minimum votes needed for simple majority
int simple_majority(int gen_no)
{
	return max_traitors(gen_no)+1;
}

// counts the votes
cnt_t count_votes(msg_t *msgs,int no)
{
	cnt_t cnt;
	cnt.v0=cnt.v1=cnt.v2=0;
	for(int i=0;i<no;i++)
		switch(msgs[i].data)
		{
			case 0: cnt.v0++; break;
			case 1: cnt.v1++; break;
			case 2: cnt.v2++; break;
			default: break;
		}
	return cnt;
}

// decides the new vote based on counted votes and a vote bound
vote_t decide_vote(cnt_t cnt,int bound)
{
	if(cnt.v0>=bound) return 0;
	if(cnt.v1>=bound) return 1;
	return 2;
}

// decides a vote that doesn't create a super majority
vote_t vote_minority(cnt_t cnt,int tra_idx)
{
	if(cnt.v0>cnt.v1 && cnt.v1+tra_idx<=cnt.v0) return 1;
	if(cnt.v1>cnt.v0 && cnt.v0+tra_idx<=cnt.v1) return 0;
	return (tra_idx%2)?1:0;
}

// gets final votes from loyal generals
vote_t* get_final_votes(mbo_t mbo,int loy_no)
{
	vote_t *votes=(vote_t*)malloc(loy_no*sizeof(vote_t));
	msg_t *msgs=receive_messages(mbo,loy_no);
	for(int i=0;i<loy_no;i++) votes[i]=msgs[i].data;
	free(msgs);
	return votes;
}

// returns a random number in specified range
int random(int l,int h)
{
	return l+rand()%(h-l+1);
}
