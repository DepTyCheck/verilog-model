-- Seed: 3893214623111981502,3181554006726329157

entity ri is
  port (a : buffer time_vector(1 to 2));
end ri;

architecture i of ri is
  
begin
  -- Single-driven assignments
  a <= (4_1_3.11313 ns, 8#23453# ps);
end i;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity pquxgdo is
  port (lqdjvfm : buffer std_logic; xqjmvlcso : inout array_value_mirror);
end pquxgdo;

architecture dn of pquxgdo is
  signal u : time_vector(1 to 2);
  signal x : time_vector(1 to 2);
  signal zfwemoesg : time_vector(1 to 2);
  signal suhbpgcuqd : time_vector(1 to 2);
begin
  mvfn : entity work.ri
    port map (a => suhbpgcuqd);
  lyfiunstmb : entity work.ri
    port map (a => zfwemoesg);
  gxarrgqprs : entity work.ri
    port map (a => x);
  dezfuh : entity work.ri
    port map (a => u);
end dn;



-- Seed after: 6056095606551756880,3181554006726329157
