#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "msgconf.h"

// creates a mailbox
mb_t create_mailbox(void)
{
	mb_t mb;
	int pfd[2];
	if(pipe(pfd)<0)
	{
		perror("create_mailbox: pipe");
		exit(1);
	}
	mb.mbi=pfd[1];
	mb.mbo=pfd[0];
	return mb;
}

// creates a number of mailboxes
mb_t* create_mailboxes(int no)
{
	mb_t *mbs=(mb_t*)malloc(no*sizeof(mb_t));
	for(int i=0;i<no;i++) mbs[i]=create_mailbox();
	return mbs;
}

// destroys a mailbox
void destroy_mailbox(mb_t mb)
{
	if(close(mb.mbi)<0)
	{
		perror("destroy_mailbox: close");
		exit(1);
	}
	if(close(mb.mbo)<0)
	{
		perror("destroy_mailbox: close");
		exit(1);
	}
	return;
}

// destroys a number of mailboxes
void destroy_mailboxes(mb_t *mbs,int no)
{
	for(int i=0;i<no;i++) destroy_mailbox(mbs[i]);
	return;
}

// sends a message to a mailbox
void send_message(mbi_t mbi,msg_t msg)
{
	if(write(mbi,&msg,sizeof(msg_t))<0)
	{
		perror("send_message: write");
		exit(1);
	}
	return;
}

// sends a message to a number of mailboxes
void send_messages(mbi_t *mbis,int no,msg_t msg)
{
	for(int i=0;i<no;i++) send_message(mbis[i],msg);
	return;
}

// receives a message from a mailbox
msg_t receive_message(mbo_t mbo)
{
	msg_t msg;
	if(read(mbo,&msg,sizeof(msg_t))<0)
	{
		perror("receive_message: read");
		exit(1);
	}
	return msg;
}

// receives a number of messages from a mailbox
msg_t* receive_messages(mbo_t mbo,int no)
{
	msg_t *msgs=(msg_t*)malloc(no*sizeof(msg_t));
	for(int i=0;i<no;i++) msgs[i]=receive_message(mbo);
	return msgs;
}

// keeps only sender's (in) info for a number of mailboxes
mbi_t* mailbox_addresses(mb_t *mbs,int no)
{
	mbi_t *mbis=(mbi_t*)malloc(no*sizeof(mbi_t));
	for(int i=0;i<no;i++) mbis[i]=mbs[i].mbi;
	return mbis;
}
