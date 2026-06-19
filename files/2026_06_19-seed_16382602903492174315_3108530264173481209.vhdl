-- Seed: 16382602903492174315,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity efxm is
  port (usulkxcwe : linkage std_logic_vector(0 downto 2); xzhsjd : buffer std_logic);
end efxm;

architecture woir of efxm is
  
begin
  -- Multi-driven assignments
  xzhsjd <= 'L';
  xzhsjd <= '1';
  xzhsjd <= 'X';
  xzhsjd <= 'H';
end woir;

library ieee;
use ieee.std_logic_1164.all;

entity gfmowbm is
  port (ajggzuomd : inout std_logic_vector(2 to 4); awnmpnhcx : inout integer; yaltsldvlx : in string(1 downto 3));
end gfmowbm;

architecture jpin of gfmowbm is
  
begin
  -- Single-driven assignments
  awnmpnhcx <= 2#0_0_0_1_1#;
  
  -- Multi-driven assignments
  ajggzuomd <= "HHW";
  ajggzuomd <= ('X', 'X', 'X');
  ajggzuomd <= ('U', 'X', '1');
end jpin;

library ieee;
use ieee.std_logic_1164.all;

entity ex is
  port (vqznd : out std_logic_vector(3 to 4));
end ex;

library ieee;
use ieee.std_logic_1164.all;

architecture fiav of ex is
  signal lazvkezzo : integer;
  signal ymrropbp : std_logic_vector(2 to 4);
  signal umogm : std_logic;
  signal gprzlgdul : std_logic_vector(0 downto 2);
  signal soezfeuuks : std_logic;
  signal tpiplcbstn : std_logic_vector(0 downto 2);
  signal mweh : string(1 downto 3);
  signal phvyeoby : integer;
  signal vwknfna : std_logic_vector(2 to 4);
begin
  sxpa : entity work.gfmowbm
    port map (ajggzuomd => vwknfna, awnmpnhcx => phvyeoby, yaltsldvlx => mweh);
  qaopkanq : entity work.efxm
    port map (usulkxcwe => tpiplcbstn, xzhsjd => soezfeuuks);
  zir : entity work.efxm
    port map (usulkxcwe => gprzlgdul, xzhsjd => umogm);
  fbkkgjai : entity work.gfmowbm
    port map (ajggzuomd => ymrropbp, awnmpnhcx => lazvkezzo, yaltsldvlx => mweh);
  
  -- Single-driven assignments
  mweh <= "";
  
  -- Multi-driven assignments
  vqznd <= "WL";
end fiav;



-- Seed after: 9770396564972765658,3108530264173481209
