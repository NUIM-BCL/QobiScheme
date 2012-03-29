#include <stdio.h>
#include <unistd.h>
void nobuff(){setbuf(stdout, (char*)0);}
int cd(char *path){return chdir(path);}
int c_file_exists(char *pathname){return access(pathname, F_OK)==0;}
