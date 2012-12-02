/*
f:
	addi	$r3, $r3, 10
	slli	$r3, $r3, 2
	return
g:
	addi	$r4, $r0, 10
	addi	$r5, $r0, 0
g_loop:
	addi	$r5, $r5, 1
	ldr		$r6, $r3, $r5
	outputb	$r6
	blt		$r5, $r4, g_loop
	nop
	nop
	return
 */
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
