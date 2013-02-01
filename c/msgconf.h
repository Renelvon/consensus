#include <sys/types.h>

// mailbox type for sender (in)
typedef int mbi_t;
// mailbox type for receiver (out)
typedef int mbo_t;
// whole mailbox type (in-out)
typedef struct {mbi_t mbi; mbo_t mbo;} mb_t;
// signature type
typedef pid_t sig_t;
// data type
typedef char data_t;
// signed message type
typedef struct {sig_t sig; data_t data;} msg_t;

// creates a mailbox
mb_t create_mailbox(void);
// creates a number of mailboxes
mb_t* create_mailboxes(int);
// destroys a mailbox
void destroy_mailbox(mb_t);
// destroys a number of mailboxes
void destroy_mailboxes(mb_t*,int);
// sends a message to a mailbox
void send_message(mbi_t,msg_t);
// sends a message to a number of mailboxes
void send_messages(mbi_t*,int,msg_t);
// receives a message from a mailbox
msg_t receive_message(mbo_t);
// receives a number of messages from a mailbox
msg_t* receive_messages(mbo_t,int);
// keeps only sender's (in) info for a number of mailboxes
mbi_t* mailbox_addresses(mb_t*,int);
