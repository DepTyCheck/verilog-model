-- Seed: 412422059223126697,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (hufgt : inout std_logic; yg : in boolean_vector(3 downto 3));
end z;

architecture ugd of z is
  
begin
  -- Multi-driven assignments
  hufgt <= hufgt;
  hufgt <= 'X';
  hufgt <= '0';
  hufgt <= 'W';
end ugd;

library ieee;
use ieee.std_logic_1164.all;

entity zwb is
  port (tfbycszz : linkage std_logic_vector(3 downto 4); nqtp : linkage string(4 to 5));
end zwb;

library ieee;
use ieee.std_logic_1164.all;

architecture umxzl of zwb is
  signal sxy : boolean_vector(3 downto 3);
  signal bhbnkj : std_logic;
  signal iunyo : boolean_vector(3 downto 3);
  signal lb : std_logic;
  signal upxdpho : boolean_vector(3 downto 3);
  signal frpch : std_logic;
begin
  v : entity work.z
    port map (hufgt => frpch, yg => upxdpho);
  vwqakjuno : entity work.z
    port map (hufgt => lb, yg => iunyo);
  buv : entity work.z
    port map (hufgt => bhbnkj, yg => iunyo);
  phtlckiag : entity work.z
    port map (hufgt => frpch, yg => sxy);
  
  -- Single-driven assignments
  upxdpho <= (others => TRUE);
  iunyo <= (others => TRUE);
  
  -- Multi-driven assignments
  bhbnkj <= 'Z';
end umxzl;

entity cww is
  port (vcjrwar : buffer real);
end cww;

architecture lv of cww is
  
begin
  
end lv;

library ieee;
use ieee.std_logic_1164.all;

entity hzuaxh is
  port (qctgapv : linkage time; zgjmu : out std_logic; hmweryucf : in bit);
end hzuaxh;

library ieee;
use ieee.std_logic_1164.all;

architecture azxwtgdswl of hzuaxh is
  signal swnyxare : boolean_vector(3 downto 3);
  signal udfmtwrqv : std_logic;
begin
  jfzitlwrtb : entity work.z
    port map (hufgt => udfmtwrqv, yg => swnyxare);
  
  -- Single-driven assignments
  swnyxare <= swnyxare;
  
  -- Multi-driven assignments
  udfmtwrqv <= zgjmu;
  udfmtwrqv <= 'Z';
end azxwtgdswl;



-- Seed after: 6690811828465763820,17234720251424330329
