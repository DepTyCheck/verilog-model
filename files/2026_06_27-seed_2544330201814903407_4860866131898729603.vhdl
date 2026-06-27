-- Seed: 2544330201814903407,4860866131898729603

entity zrswft is
  port (vu : out bit; dezmma : buffer boolean);
end zrswft;

architecture ytjcg of zrswft is
  
begin
  -- Single-driven assignments
  vu <= '1';
  dezmma <= FALSE;
end ytjcg;

library ieee;
use ieee.std_logic_1164.all;

entity kiair is
  port (hm : linkage time; dda : out std_logic; lvobbihbv : inout std_logic);
end kiair;

architecture kshg of kiair is
  signal jin : boolean;
  signal rup : bit;
  signal bqm : boolean;
  signal wo : bit;
  signal nuz : boolean;
  signal h : bit;
begin
  uontu : entity work.zrswft
    port map (vu => h, dezmma => nuz);
  x : entity work.zrswft
    port map (vu => wo, dezmma => bqm);
  nwxr : entity work.zrswft
    port map (vu => rup, dezmma => jin);
  
  -- Multi-driven assignments
  lvobbihbv <= 'W';
  lvobbihbv <= '-';
  lvobbihbv <= '1';
end kshg;

library ieee;
use ieee.std_logic_1164.all;

entity jq is
  port (dtrxcamzwv : linkage std_logic; adshbtmuj : out std_logic; mmmkfoo : out integer; gelacu : in std_logic_vector(1 downto 2));
end jq;

library ieee;
use ieee.std_logic_1164.all;

architecture ts of jq is
  signal v : time;
  signal otqgq : time;
  signal qwxmxjgh : std_logic;
  signal dqlqhtok : std_logic;
  signal qfb : time;
  signal coluvn : boolean;
  signal hmlfaxmk : bit;
begin
  shp : entity work.zrswft
    port map (vu => hmlfaxmk, dezmma => coluvn);
  aabti : entity work.kiair
    port map (hm => qfb, dda => dqlqhtok, lvobbihbv => qwxmxjgh);
  ot : entity work.kiair
    port map (hm => otqgq, dda => qwxmxjgh, lvobbihbv => adshbtmuj);
  rpj : entity work.kiair
    port map (hm => v, dda => adshbtmuj, lvobbihbv => adshbtmuj);
  
  -- Single-driven assignments
  mmmkfoo <= 3_4_3_2;
  
  -- Multi-driven assignments
  dqlqhtok <= '-';
  dqlqhtok <= '1';
  adshbtmuj <= '0';
end ts;



-- Seed after: 15936334996424699679,4860866131898729603
