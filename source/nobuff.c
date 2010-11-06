#include <stdio.h>
#include <unistd.h>
void nobuff(){setbuf(stdout, (char*)0);}
int cd(char *path){return chdir(path);}
