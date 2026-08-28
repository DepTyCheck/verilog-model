-- Seed: 8987199974577212395,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity materdopzc is
  port (u : out integer; jh : buffer time; eieswsj : buffer std_logic_vector(0 to 3));
end materdopzc;

architecture cknlxrsvzh of materdopzc is
  
begin
  -- Multi-driven assignments
  eieswsj <= eieswsj;
end cknlxrsvzh;

library ieee;
use ieee.std_logic_1164.all;

entity zopbch is
  port (pjnqupx : linkage std_logic; si : inout real; qzoarkla : buffer std_logic_vector(0 to 2));
end zopbch;

library ieee;
use ieee.std_logic_1164.all;

architecture rumzft of zopbch is
  signal weujilt : time;
  signal mfxjqsriib : integer;
  signal ytavmtaswn : time;
  signal nkbvmnea : integer;
  signal anyctjjo : std_logic_vector(0 to 3);
  signal qnlwygnexe : time;
  signal wyuxmnvuc : integer;
begin
  tsxeug : entity work.materdopzc
    port map (u => wyuxmnvuc, jh => qnlwygnexe, eieswsj => anyctjjo);
  ukgkxr : entity work.materdopzc
    port map (u => nkbvmnea, jh => ytavmtaswn, eieswsj => anyctjjo);
  k : entity work.materdopzc
    port map (u => mfxjqsriib, jh => weujilt, eieswsj => anyctjjo);
  
  -- Multi-driven assignments
  anyctjjo <= ('X', 'U', 'H', '-');
end rumzft;

library ieee;
use ieee.std_logic_1164.all;

entity orpxh is
  port (q : linkage std_logic_vector(0 downto 4));
end orpxh;

architecture kt of orpxh is
  
begin
  
end kt;

entity ago is
  port (npzwrcsvi : linkage severity_level);
end ago;

library ieee;
use ieee.std_logic_1164.all;

architecture wzl of ago is
  signal koshz : std_logic_vector(0 to 3);
  signal c : time;
  signal pripvyk : integer;
  signal dhhvufm : std_logic_vector(0 to 3);
  signal s : time;
  signal bwhpvgr : integer;
  signal qaoa : std_logic_vector(0 downto 4);
begin
  wf : entity work.orpxh
    port map (q => qaoa);
  upikxi : entity work.materdopzc
    port map (u => bwhpvgr, jh => s, eieswsj => dhhvufm);
  qfmpezen : entity work.materdopzc
    port map (u => pripvyk, jh => c, eieswsj => koshz);
end wzl;



-- Seed after: 17841798654346939668,7198033922882419595
