-- Seed: 2746529625784022615,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity was is
  port (nosrdgdv : inout std_logic_vector(1 to 0); dahxoi : inout floating_subtype_mirror; kkpydvlkj : in std_logic_vector(3 downto 4));
end was;

architecture hhl of was is
  
begin
  -- Multi-driven assignments
  nosrdgdv <= kkpydvlkj;
  nosrdgdv <= "";
  nosrdgdv <= kkpydvlkj;
  nosrdgdv <= kkpydvlkj;
end hhl;

use std.reflection.all;

entity w is
  port (l : inout protected_value_mirror; pzntgbs : buffer time);
end w;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture yyzrzv of w is
  shared variable haitpso : floating_subtype_mirror;
  shared variable ktoexziegi : floating_subtype_mirror;
  signal ukbkj : std_logic_vector(1 to 0);
  signal pqn : std_logic_vector(3 downto 4);
  shared variable yotld : floating_subtype_mirror;
  signal qbez : std_logic_vector(1 to 0);
  signal opl : std_logic_vector(3 downto 4);
  shared variable dcwnfld : floating_subtype_mirror;
  signal odoxkruojm : std_logic_vector(3 downto 4);
begin
  rdeagug : entity work.was
    port map (nosrdgdv => odoxkruojm, dahxoi => dcwnfld, kkpydvlkj => opl);
  unrtf : entity work.was
    port map (nosrdgdv => qbez, dahxoi => yotld, kkpydvlkj => pqn);
  itnxkvfxqz : entity work.was
    port map (nosrdgdv => ukbkj, dahxoi => ktoexziegi, kkpydvlkj => qbez);
  eid : entity work.was
    port map (nosrdgdv => qbez, dahxoi => haitpso, kkpydvlkj => odoxkruojm);
  
  -- Single-driven assignments
  pzntgbs <= pzntgbs;
  
  -- Multi-driven assignments
  ukbkj <= "";
  odoxkruojm <= "";
end yyzrzv;



-- Seed after: 5245890393533870681,14426950258250697445
