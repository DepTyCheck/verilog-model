-- Seed: 14103507538063288028,6290177331721581829

use std.reflection.all;

entity kok is
  port (kbwnjh : buffer time; h : inout access_value_mirror);
end kok;

architecture urk of kok is
  
begin
  -- Single-driven assignments
  kbwnjh <= 4 min;
end urk;

use std.reflection.all;

entity unae is
  port (yicupvdld : inout physical_subtype_mirror);
end unae;

use std.reflection.all;

architecture rbsbenuvu of unae is
  shared variable pdvsxolfv : access_value_mirror;
  signal bys : time;
begin
  tuxjkgu : entity work.kok
    port map (kbwnjh => bys, h => pdvsxolfv);
end rbsbenuvu;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ulwrqo is
  port (pjndgcb : inout physical_subtype_mirror; qdqqdzsk : out std_logic_vector(2 to 1); t : buffer integer);
end ulwrqo;

use std.reflection.all;

architecture gwk of ulwrqo is
  shared variable degqodahr : physical_subtype_mirror;
  shared variable r : access_value_mirror;
  signal cur : time;
begin
  afbs : entity work.kok
    port map (kbwnjh => cur, h => r);
  igwof : entity work.unae
    port map (yicupvdld => degqodahr);
  
  -- Multi-driven assignments
  qdqqdzsk <= (others => '0');
  qdqqdzsk <= qdqqdzsk;
  qdqqdzsk <= (others => '0');
  qdqqdzsk <= "";
end gwk;



-- Seed after: 10263538983239627544,6290177331721581829
