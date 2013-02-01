// suspends until all generals are ready
void sync_phase_wait(mbo_t,int);
// alerts generals to start the next phase
void sync_phase_signal(mbi_t*,sig_t,int);
// suspends a general until alerted
void sync_general(mbi_t,mbo_t,sig_t);
