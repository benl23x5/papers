
#include <stdio.h>

struct ListInt { int x; struct ListInt* xs; };

int lengthListInt (struct ListInt* list)
{
	int len	= 0;
	for (struct ListInt* node = list; node != 0; node = node->xs)
		len ++;
	return len;
}


int main (int argc, char* argv)
{
	struct ListInt l1;
	struct ListInt l2;
	struct ListInt l3;

	l1.x	= 1;
	l1.xs	= &l2;

	l2.x	= 2;
	l2.xs	= 0;

	l3.x	= 3;
	l3.xs	= 0;


	printf("len = %d\n", lengthListInt(&l1));	
}

