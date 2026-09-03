-- Seed: 1084582854959875709,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity vua is
  port (qzl : buffer time; hzqvnba : inout std_logic; ohirkdkdrp : out real);
end vua;

architecture bvwjh of vua is
  
begin
  -- Single-driven assignments
  ohirkdkdrp <= 0_4_1_2_1.21213;
  qzl <= 3_0 ps;
  
  -- Multi-driven assignments
  hzqvnba <= 'L';
end bvwjh;

library ieee;
use ieee.std_logic_1164.all;

entity igdwxdrp is
  port (ry : in std_logic; mvgmi : out integer; brqe : in std_logic);
end igdwxdrp;

library ieee;
use ieee.std_logic_1164.all;

architecture igtsfkp of igdwxdrp is
  signal jfrjcllt : real;
  signal ft : std_logic;
  signal rwhrzfkjqk : time;
begin
  eitqmfptwe : entity work.vua
    port map (qzl => rwhrzfkjqk, hzqvnba => ft, ohirkdkdrp => jfrjcllt);
  
  -- Single-driven assignments
  mvgmi <= 8#2#;
  
  -- Multi-driven assignments
  ft <= 'L';
  ft <= brqe;
end igtsfkp;

library ieee;
use ieee.std_logic_1164.all;

entity pum is
  port (ivomn : out std_logic_vector(3 downto 0); cn : out integer_vector(2 to 4));
end pum;

library ieee;
use ieee.std_logic_1164.all;

architecture wvzcnqevx of pum is
  signal qivtiu : real;
  signal chsdxjyl : time;
  signal lydjaj : std_logic;
  signal majzdert : integer;
  signal ofimceory : real;
  signal dtxiism : std_logic;
  signal ls : time;
begin
  wh : entity work.vua
    port map (qzl => ls, hzqvnba => dtxiism, ohirkdkdrp => ofimceory);
  em : entity work.igdwxdrp
    port map (ry => dtxiism, mvgmi => majzdert, brqe => lydjaj);
  se : entity work.vua
    port map (qzl => chsdxjyl, hzqvnba => lydjaj, ohirkdkdrp => qivtiu);
  
  -- Single-driven assignments
  cn <= cn;
end wvzcnqevx;



-- Seed after: 8289697279325999792,11127274767545411571
