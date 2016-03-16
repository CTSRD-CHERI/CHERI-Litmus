#include "io.h"
#include "log.h"
#include "platform.h"
#include "arch.h"
#include "rand.h"
#include "test.h"

int main()
{

  // Get my global process id
  int me = arch_get_process_id();

  // Processes not being used spin here
  if (me >= NUM_PROCESSES) while (1);

  if (me == 0) {
    log_init();
  }

  // Random seed
  uint32_t seed = 0;

  for (int i = 0; i < NUM_ITERATIONS; i++) {
    if (me == 0) test_init(&seed);
    test_body(me);
    if (me == 0) log_update();
  }

  if (me == 0) {
    put_string("Observed outcomes:\n");
    log_display();
    flush();
  }

  return 0;
}
