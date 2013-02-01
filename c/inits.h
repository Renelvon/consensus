// general type
typedef pid_t gen_t;
// role type
typedef char role_t;

// initializes the generals' roles
role_t* init_roles(int,int);
// creates a loyal general
gen_t create_loyal(int,mbi_t*,mbo_t,int,vote_t);
// creates a traitor general
gen_t create_traitor(int,mbi_t*,mbo_t,int,role_t*,int);
// creates the generals
gen_t* create_generals(role_t*,mb_t*,mbi_t*,int,int,vote_t*);
