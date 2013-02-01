// vote type
typedef char vote_t;
// counted votes type
typedef struct{int v0,v1,v2;} cnt_t;

// calcs maximum traitors unable to break the protocol
int max_traitors(int);
// calcs minimum votes needed for super majority
int super_majority(int);
// calcs minimum votes needed for simple majority
int simple_majority(int);
// counts the votes
cnt_t count_votes(msg_t*,int);
// decides the new vote based on counted votes and a vote bound
vote_t decide_vote(cnt_t,int);
// decides a vote that doesn't create a super majority
vote_t vote_minority(cnt_t,int);
// gets the final votes from loyal generals
vote_t* get_final_votes(mbo_t,int);
// returns a random number in specified range
int random(int,int);
