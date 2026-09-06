/* 차등 테스팅 드라이버 — zexdoc.cim 을 superzazu 코어로 돌려 매 스텝
   (pc af bc de hl ix iy sp) 를 stdout 에 찍는다. OCaml 쪽 덤프와 diff. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "z80.h"

static uint8_t mem[0x10000];

static uint8_t rb(void* _, uint16_t a) { (void)_; return mem[a]; }
static void wb(void* _, uint16_t a, uint8_t v) { (void)_; mem[a] = v; }
static uint8_t fin(z80* z, uint8_t port) {
  (void)port;
  /* BDOS: c=2 문자, c=9 문자열 — 드라이버는 상태만 찍으니 무시 */
  (void)z;
  return 0xff;
}
static int done = 0;
static void fout(z80* z, uint8_t port, uint8_t v) { (void)z; (void)port; (void)v; done = 1; }

int main(int argc, char** argv) {
  const char* path = argc > 1 ? argv[1] : "zexdoc.cim";
  long max_steps = argc > 2 ? atol(argv[2]) : 6000;
  long skip = argc > 3 ? atol(argv[3]) : 0;
  memset(mem, 0, sizeof mem);
  mem[0x00] = 0xd3; mem[0x01] = 0x00;        /* out (0),a */
  mem[0x05] = 0xdb; mem[0x06] = 0x00; mem[0x07] = 0xc9;  /* in a,(0); ret */
  FILE* f = fopen(path, "rb");
  if (!f) { perror("open"); return 2; }
  fread(mem + 0x100, 1, sizeof mem - 0x100, f);
  fclose(f);

  z80 cpu;
  z80_init(&cpu);
  cpu.read_byte = rb; cpu.write_byte = wb;
  cpu.port_in = fin; cpu.port_out = fout;
  cpu.pc = 0x0100;

  for (long n = 0; n < max_steps && !done; n++) {
    if (n % 100000000 == 0) fprintf(stderr, "n=%ld pc=%04x\n", n, cpu.pc);
    if (n < skip || (skip == 0 && n % 1024 != 0)) { z80_step(&cpu); continue; }
    if (n == 1359755) { printf("mem@1359755:"); for (int i=0x1d80;i<0x1da0;i++) printf(" %02x", mem[i]); printf("\n"); }
    if (n == 7) { printf("stack@7:"); for (int i=0xc8e0;i<0xc910;i++) printf(" %02x", mem[i]); printf("\n"); }
    unsigned mchk = 2166136261u;
    for (int i = 0x100; i < 0x10000; i++) { mchk ^= mem[i]; mchk *= 16777619u; }
    printf("m %ld %08x\n", n, mchk);
    uint8_t fl = (cpu.sf << 7) | (cpu.zf << 6) | (cpu.yf << 5) | (cpu.hf << 4)
               | (cpu.xf << 3) | (cpu.pf << 2) | (cpu.nf << 1) | cpu.cf;
    printf("%ld op=%02x pc=%04x af=%04x bc=%04x de=%04x hl=%04x ix=%04x iy=%04x sp=%04x\n",
           n, rb(0, cpu.pc), cpu.pc, (cpu.a << 8) | fl, (cpu.b << 8) | cpu.c,
           (cpu.d << 8) | cpu.e, (cpu.h << 8) | cpu.l, cpu.ix, cpu.iy, cpu.sp);
    z80_step(&cpu);
  }
  fprintf(stderr, "steps=%ld done=%d\n", max_steps < 0 ? 0 : 0, done);
  return 0;
}
