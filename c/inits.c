#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "msgconf.h"
#include "calcs.h"
#include "inits.h"
#include "loyal.h"
#include "traitor.h"

// initializes the generals' roles
role_t* init_roles(int gen_no,int tra_no)
{
	role_t *roles=(role_t*)malloc(gen_no*sizeof(role_t));
	for(int i=0;i<gen_no;i++) roles[i]='L';
	for(int i=0;i<tra_no;i++)
	{
		int x;
		while(roles[x=random(0,gen_no-1)]!='L');
		roles[x]='T';
	}
	return roles;
}

// creates a loyal general
gen_t create_loyal(int gen_id,mbi_t *mbis,mbo_t mbo,int gen_no,vote_t vote)
{
	gen_t gen=fork();
	if(gen<0)
	{
		perror("create_loyal: fork");
		exit(1);
	}
	if(!gen)
	{
		play_loyal(gen_id,mbis,mbo,gen_no,vote);
		exit(1);
	}
	return gen;
}

// creates a traitor general
gen_t create_traitor(int gen_id,mbi_t *mbis,mbo_t mbo,int gen_no,role_t *roles,int tra_no)
{
	gen_t gen=fork();
	if(gen<0)
	{
		perror("create_traitor: fork");
		exit(1);
	}
	if(!gen)
	{
		play_traitor(gen_id,mbis,mbo,gen_no,roles,tra_no);
		exit(1);
	}
	return gen;
}

// creates the generals
gen_t* create_generals(role_t *roles,mb_t *mbs,mbi_t *mbis,int gen_no,int tra_no,vote_t *votes)
{
	gen_t *gens=(gen_t*)malloc(gen_no*sizeof(gen_t));
	for(int x=0,i=0;i<gen_no;i++)
		if(roles[i]=='L') gens[i]=create_loyal(i+1,mbis,mbs[i+1].mbo,gen_no,votes[x++]);
		else gens[i]=create_traitor(i+1,mbis,mbs[i+1].mbo,gen_no,roles,tra_no);
	return gens;
}
