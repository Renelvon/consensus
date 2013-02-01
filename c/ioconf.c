#include <stdio.h>
#include <stdlib.h>

#include "msgconf.h"
#include "calcs.h"

// parses the input
vote_t* parse_input(int *gen_no,int *tra_no)
{
	scanf("%d%d",gen_no,tra_no);
	vote_t *votes=(vote_t*)malloc((*gen_no-*tra_no)*sizeof(vote_t));
	for(int i=0;i<*gen_no-*tra_no;i++)
	{
		int x;
		scanf("%d",&x);
		votes[i]=x;
	}
	return votes;
}

// prints the input
void print_input(int gen_no,int loy_no,int tra_no,vote_t *votes)
{
	printf("Number of generals: %d\n",gen_no);
	printf("Number of loyal generals: %d\n",loy_no);
	printf("Number of traitor generals: %d\n\n",tra_no);
	printf("Votes needed for super majority: %d\n",super_majority(gen_no));
	printf("Votes needed for simple majority: %d\n\n",simple_majority(gen_no));
	printf("Loyal generals' initial votes:");
	for(int i=0;i<loy_no;i++) printf(" %d",votes[i]);
	printf("\n\n");
	return;
}

// prints the output
void print_output(int loy_no,vote_t *votes)
{
	printf("\n");
	printf("Loyal generals' final votes:");
	for(int i=0;i<loy_no;i++) printf(" %d",votes[i]);
	printf("\n\n");
	return;
}

// prints round info
void print_round(int round,int rounds_no)
{
	printf("Round %d of %d started.\n\n",round,rounds_no);
	return;
}

// prints phase info
void print_phase(int phase,int phases_no)
{
	printf("Phase %d of %d started.\n",phase,phases_no);
	if(phase==phases_no) printf("\n");
	return;
}
