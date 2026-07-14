-- Seed: 4877250678489846569,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;

entity rilxmqik is
  port (hj : buffer std_logic_vector(4 to 3); m : buffer std_logic_vector(3 to 1));
end rilxmqik;

architecture qkmvn of rilxmqik is
  
begin
  
end qkmvn;

use std.reflection.all;

entity bk is
  port (qgmshsyw : buffer boolean; rcbn : inout time; itvw : inout array_subtype_mirror; taljyjxii : inout file_subtype_mirror);
end bk;

library ieee;
use ieee.std_logic_1164.all;

architecture tymw of bk is
  signal zg : std_logic_vector(3 to 1);
  signal eyrzlyxj : std_logic_vector(3 to 1);
begin
  aujuh : entity work.rilxmqik
    port map (hj => eyrzlyxj, m => zg);
  ex : entity work.rilxmqik
    port map (hj => eyrzlyxj, m => eyrzlyxj);
  
  -- Single-driven assignments
  rcbn <= 4 sec;
  qgmshsyw <= FALSE;
  
  -- Multi-driven assignments
  eyrzlyxj <= (others => '0');
  eyrzlyxj <= (others => '0');
end tymw;



-- Seed after: 8246807499593192206,7726014785203345639
