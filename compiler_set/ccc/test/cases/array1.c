int x[10];

int f(int t) {
	return (t + 10) * 4;
}

int main(int argc)
{
	x[2] = x[0] + x[1];
	x[3] = f(x[5]);

	print_int(x[2]);
	print_int(x[3]);

	return 0;
}
