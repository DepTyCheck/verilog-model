-- Seed: 9674380199337811449,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity ulubtyf is
  port (mmueobfctp : buffer std_logic; rmrfeqzzep : out time; meuy : out time; xo : out time);
end ulubtyf;

architecture aqn of ulubtyf is
  
begin
  -- Single-driven assignments
  xo <= rmrfeqzzep;
  meuy <= xo;
  rmrfeqzzep <= 2_0_1_0_4.1 ps;
  
  -- Multi-driven assignments
  mmueobfctp <= '1';
  mmueobfctp <= mmueobfctp;
  mmueobfctp <= mmueobfctp;
  mmueobfctp <= 'X';
end aqn;



-- Seed after: 6156709577274418910,16159265764638711791
