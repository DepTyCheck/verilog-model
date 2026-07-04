-- Seed: 11990024919754723940,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity cspg is
  port (vfjuwnin : in std_logic_vector(2 to 3); cajksrdmt : inout array_value_mirror; phkaizxv : buffer std_logic);
end cspg;

architecture ksyujxcym of cspg is
  
begin
  -- Multi-driven assignments
  phkaizxv <= phkaizxv;
end ksyujxcym;

use std.reflection.all;

entity chmwjlp is
  port (e : inout record_value_mirror; xis : inout integer_value_mirror; aity : inout physical_subtype_mirror);
end chmwjlp;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture pn of chmwjlp is
  shared variable wvg : array_value_mirror;
  signal vfcrbt : std_logic;
  shared variable owncowoyj : array_value_mirror;
  signal db : std_logic_vector(2 to 3);
begin
  veayk : entity work.cspg
    port map (vfjuwnin => db, cajksrdmt => owncowoyj, phkaizxv => vfcrbt);
  prlrrs : entity work.cspg
    port map (vfjuwnin => db, cajksrdmt => wvg, phkaizxv => vfcrbt);
  
  -- Multi-driven assignments
  db <= ('0', 'Z');
  db <= ('L', 'U');
  vfcrbt <= 'H';
  db <= "H-";
end pn;



-- Seed after: 18102278481064884326,6290177331721581829
