#include <stdio.h>
int main()  {
	testing:  {
		printf("Hellow world");
	}
	goto testing;
back:
	printf("done testing");
	return 0;
}
