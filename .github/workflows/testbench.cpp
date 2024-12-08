#include "Vtop.h"  // Include the header for the top module
#include "verilated.h"

int main(int argc, char **argv) {
    VerilatedContext *m_contextp = new VerilatedContext; // Context
    Vtop *m_duvp = new Vtop;                             // Design

    int i = 0;

    while (!m_contextp->gotFinish() && i < 1000)
    {
        // Refresh circuit state
        m_duvp->eval();

        // Increase simulation time
        m_contextp->timeInc(1);

        ++i;
    }

    // Free memory
    delete m_duvp;
    delete m_contextp;

    return 0;
}
