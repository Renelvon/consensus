// plays a traitor general
void play_traitor(int,mbi_t*,mbo_t,int,role_t*,int);
// plays phase #1 of traitors' protocol (Traitor King)
void play_traitor_Tph1(mbi_t*,mbo_t,int,sig_t,int);
// plays phase #2 of traitors' protocol (Traitor King)
void play_traitor_Tph2(mbi_t*,mbo_t,int,sig_t);
// plays phase #3 of traitors' protocol (Traitor King)
void play_traitor_Tph3(int,mbi_t*,int,int,sig_t,int);
// plays phase #1 of traitors' protocol (Loyal King)
cnt_t play_traitor_Lph1(mbi_t*,mbo_t,int,int,sig_t,int,int);
// plays phase #2 of traitors' protocol (Loyal King)
void play_traitor_Lph2(mbi_t*,mbo_t,int,int,sig_t,cnt_t,int);
// plays phase #3 of traitors' protocol (Loyal King)
void play_traitor_Lph3(mbo_t);
