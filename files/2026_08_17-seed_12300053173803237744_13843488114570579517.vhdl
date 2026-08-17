-- Seed: 12300053173803237744,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity tx is
  port (yi : linkage integer_vector(0 downto 4); mhie : in string(4 to 4); bbtjvkzcpx : in std_logic);
end tx;

architecture w of tx is
  
begin
  
end w;

library ieee;
use ieee.std_logic_1164.all;

entity rgxopytevy is
  port (nqmid : out std_logic_vector(4 to 3); ydswfaqya : inout bit; hmf : in integer);
end rgxopytevy;

library ieee;
use ieee.std_logic_1164.all;

architecture ov of rgxopytevy is
  signal fd : integer_vector(0 downto 4);
  signal ukquvkmhxt : std_logic;
  signal bfpmzaycy : string(4 to 4);
  signal kolfvtuk : integer_vector(0 downto 4);
begin
  mk : entity work.tx
    port map (yi => kolfvtuk, mhie => bfpmzaycy, bbtjvkzcpx => ukquvkmhxt);
  velrjcthll : entity work.tx
    port map (yi => fd, mhie => bfpmzaycy, bbtjvkzcpx => ukquvkmhxt);
  
  -- Single-driven assignments
  ydswfaqya <= '1';
  bfpmzaycy <= "n";
  
  -- Multi-driven assignments
  nqmid <= "";
end ov;

entity dimry is
  port (mmoyqmn : in integer; fmbgmbiky : in real; prut : inout real; flkghs : out time);
end dimry;

library ieee;
use ieee.std_logic_1164.all;

architecture whpglvz of dimry is
  signal kfsfbjy : bit;
  signal igjlhavxv : std_logic_vector(4 to 3);
  signal sqgvtkjqa : bit;
  signal zaujiiuag : integer;
  signal joixemi : bit;
  signal aa : std_logic_vector(4 to 3);
  signal wwp : std_logic;
  signal no : string(4 to 4);
  signal kdusyawy : integer_vector(0 downto 4);
begin
  c : entity work.tx
    port map (yi => kdusyawy, mhie => no, bbtjvkzcpx => wwp);
  fki : entity work.rgxopytevy
    port map (nqmid => aa, ydswfaqya => joixemi, hmf => zaujiiuag);
  hceuq : entity work.rgxopytevy
    port map (nqmid => aa, ydswfaqya => sqgvtkjqa, hmf => zaujiiuag);
  rxsual : entity work.rgxopytevy
    port map (nqmid => igjlhavxv, ydswfaqya => kfsfbjy, hmf => mmoyqmn);
  
  -- Single-driven assignments
  prut <= prut;
  
  -- Multi-driven assignments
  aa <= (others => '0');
  igjlhavxv <= "";
  wwp <= 'L';
end whpglvz;

library ieee;
use ieee.std_logic_1164.all;

entity yc is
  port (heopuxalpp : in std_logic_vector(2 downto 3); kjlxeb : in real_vector(3 downto 0); fm : linkage std_logic);
end yc;

architecture aus of yc is
  signal vzhex : time;
  signal q : real;
  signal kqhokln : integer;
begin
  tzijijgmmy : entity work.dimry
    port map (mmoyqmn => kqhokln, fmbgmbiky => q, prut => q, flkghs => vzhex);
  
  -- Single-driven assignments
  kqhokln <= 1;
end aus;



-- Seed after: 15428937826470836100,13843488114570579517
