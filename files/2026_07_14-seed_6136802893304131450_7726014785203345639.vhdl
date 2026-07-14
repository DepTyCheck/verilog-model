-- Seed: 6136802893304131450,7726014785203345639

use std.reflection.all;

entity vc is
  port (q : linkage real_vector(1 downto 4); kvswiqlsf : inout physical_value_mirror);
end vc;

architecture bekqoak of vc is
  
begin
  
end bekqoak;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity pgcb is
  port (nncudzgda : inout record_subtype_mirror; etwhuiq : linkage std_logic_vector(3 to 4); rgmyowgubt : inout floating_subtype_mirror);
end pgcb;

use std.reflection.all;

architecture vrezoaqqdq of pgcb is
  shared variable ifbczuzglv : physical_value_mirror;
  signal kclxyjetul : real_vector(1 downto 4);
begin
  p : entity work.vc
    port map (q => kclxyjetul, kvswiqlsf => ifbczuzglv);
end vrezoaqqdq;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity hijabi is
  port (zuapbii : buffer integer; qg : inout integer_value_mirror; aichopb : out std_logic);
end hijabi;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture urnqobxx of hijabi is
  shared variable iabr : physical_value_mirror;
  signal kuedi : real_vector(1 downto 4);
  shared variable vsqs : floating_subtype_mirror;
  shared variable dbgoh : record_subtype_mirror;
  shared variable scgnjaapdd : floating_subtype_mirror;
  signal xvbofcpp : std_logic_vector(3 to 4);
  shared variable veztan : record_subtype_mirror;
  shared variable nbgm : floating_subtype_mirror;
  signal lk : std_logic_vector(3 to 4);
  shared variable satu : record_subtype_mirror;
begin
  fgpsq : entity work.pgcb
    port map (nncudzgda => satu, etwhuiq => lk, rgmyowgubt => nbgm);
  k : entity work.pgcb
    port map (nncudzgda => veztan, etwhuiq => xvbofcpp, rgmyowgubt => scgnjaapdd);
  torxjahy : entity work.pgcb
    port map (nncudzgda => dbgoh, etwhuiq => xvbofcpp, rgmyowgubt => vsqs);
  zyvoxfud : entity work.vc
    port map (q => kuedi, kvswiqlsf => iabr);
  
  -- Single-driven assignments
  zuapbii <= zuapbii;
  
  -- Multi-driven assignments
  aichopb <= 'U';
  aichopb <= aichopb;
  lk <= lk;
  xvbofcpp <= lk;
end urnqobxx;



-- Seed after: 12362596149815029832,7726014785203345639
