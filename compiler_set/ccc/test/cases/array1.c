int x[10];

int f(int t);
void g(int * t);

int main(int argc)
{
  x[2] = x[0] + x[1];
  x[3] = f(x[5]);
  g(x);
  return x[6];
}
