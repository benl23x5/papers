
struct IntVector {
	int const * x;
	int const *const y;
};

int main (int argc, char** argv)
{
	IntVector *iv	= new IntVector();

	int a	= 3;
	int b	= 4;	

	iv->x	= &a;
	*(iv->x)++;

	iv->y	= &b;

	return 1;
}
