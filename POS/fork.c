
#define _XOPEN_SOURCE
#define _XOPEN_SOURCE_EXTENDED 1 /* XPG 4.2 - needed for WCOREDUMP() */
#define GRANDPARENT "grandparent"
#define PARENT "parent"
#define CHILD "child"
#define FORK_ERROR "Failed to fork new process!"
#define EXECV_ERROR "Failed to call execv!"
#define CORE_DUMP "with core dump"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <string.h>

void printIdentification(char name[]) {
	printf("%s identification:\n", name);
	printf("\tpid = %d,\tppid = %d,\tpgrp = %d\n", getpid(), getppid(), getpgrp());
	printf("\tuid = %d,\tgid = %d,\n", getuid(), getgid());
	printf("\teuid = %d,\tegid = %d\n", geteuid(), getegid());
}

void printTermination(int status, int core) {
	if(WIFEXITED(status)) {
		printf("\tnormal termination (exit code = %d)\n", WEXITSTATUS(status));
	} else if(WIFSIGNALED(status)) {
		if(core)
			printf("\tsignal termination %s(signal = %d)\n", CORE_DUMP, WTERMSIG(status));
		else
			printf("\tsignal termination (signal = %d)\n", WTERMSIG(status));
	} else {
		printf("\tunknown type of termination\n");
	}
}

/* ARGSUSED */
int main(int argc, char *argv[]) {
	int i,j,pStatus,core;

	/* PRINT OUT GRANDPARENT */
	printIdentification(GRANDPARENT);
	pid_t pId = fork();

	switch(pId) {
		case -1:
			fprintf(stderr, "%s\n", FORK_ERROR);
			exit(-1);
		case 0:
			/* PRINT OUT PARENT */
			printIdentification(PARENT);
			int cStatus;
			pid_t cId = fork();

			switch(cId) {
				case -1:
					fprintf(stderr, "%s\n", FORK_ERROR);
					exit(-2);
				case 0:
					/* PRINT OUT CHILD */
					printIdentification(CHILD);
					if(argc >= 2) {
						char** execv_argv = malloc(sizeof(char*) * (argc+1));
						for ( i = 1, j = 0; i < argc; i++, j++ ) {
							execv_argv[j] = malloc(sizeof(char) * (strlen(argv[i]) + 1));
							execv_argv[j] = argv[i];
						}
						execv_argv[argc] = "\0";
						i = execv(argv[1], execv_argv);
						if (i == -1) {
							fprintf(stderr, "%s\n", EXECV_ERROR);
						}
						free(execv_argv);
					}
					break;
				default:
					core = 0;
					waitpid(cId, &cStatus, 0);
					printf("%s exit (pid = %d):", CHILD, (int)cId);
					#ifdef WCOREDUMP
						if (WCOREDUMP(cStatus)){
							core = 1;
						}
					#endif

					printTermination(cStatus, core);
					break;
			}
			break;
		default:
			core = 0;
			waitpid(pId, &pStatus, 0);
			printf("%s exit (pid = %d):", PARENT, (int)pId);
			#ifdef WCOREDUMP
				if (WCOREDUMP(pStatus)){
					core = 1;
				}
			#endif

			printTermination(pStatus, core);
			break;
	}
	
	return 0;
}