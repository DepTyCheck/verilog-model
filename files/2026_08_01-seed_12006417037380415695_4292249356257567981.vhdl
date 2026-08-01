-- Seed: 12006417037380415695,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity wdvh is
  port (idlska : linkage std_logic; bolszc : out integer; clfj : inout std_logic_vector(0 downto 4));
end wdvh;

architecture u of wdvh is
  
begin
  -- Single-driven assignments
  bolszc <= bolszc;
  
  -- Multi-driven assignments
  clfj <= "";
  clfj <= clfj;
  clfj <= clfj;
end u;

library ieee;
use ieee.std_logic_1164.all;

entity hvldk is
  port (vj : linkage time_vector(1 downto 0); eczerpgjhg : out std_logic_vector(0 downto 1); csa : buffer severity_level; mequ : linkage integer);
end hvldk;

library ieee;
use ieee.std_logic_1164.all;

architecture b of hvldk is
  signal nygldtbdmr : std_logic_vector(0 downto 4);
  signal d : integer;
  signal wruzvpqi : std_logic;
begin
  mro : entity work.wdvh
    port map (idlska => wruzvpqi, bolszc => d, clfj => nygldtbdmr);
  
  -- Single-driven assignments
  csa <= csa;
  
  -- Multi-driven assignments
  eczerpgjhg <= eczerpgjhg;
  nygldtbdmr <= eczerpgjhg;
end b;

entity uavkabo is
  port (exiqmfoefl : out integer_vector(0 downto 2));
end uavkabo;

library ieee;
use ieee.std_logic_1164.all;

architecture yyhsa of uavkabo is
  signal gwzs : integer;
  signal yl : std_logic;
  signal ulim : std_logic_vector(0 downto 4);
  signal tuqhgjovyu : integer;
  signal smzudlbw : integer;
  signal qlqvxno : std_logic;
  signal cfirby : integer;
  signal wv : severity_level;
  signal sdsbelyvh : std_logic_vector(0 downto 4);
  signal dzynlaqei : time_vector(1 downto 0);
begin
  q : entity work.hvldk
    port map (vj => dzynlaqei, eczerpgjhg => sdsbelyvh, csa => wv, mequ => cfirby);
  eua : entity work.wdvh
    port map (idlska => qlqvxno, bolszc => smzudlbw, clfj => sdsbelyvh);
  dukptoefi : entity work.wdvh
    port map (idlska => qlqvxno, bolszc => tuqhgjovyu, clfj => ulim);
  inhhycn : entity work.wdvh
    port map (idlska => yl, bolszc => gwzs, clfj => sdsbelyvh);
  
  -- Single-driven assignments
  exiqmfoefl <= (others => 0);
  
  -- Multi-driven assignments
  qlqvxno <= qlqvxno;
  sdsbelyvh <= "";
  ulim <= sdsbelyvh;
end yyhsa;



-- Seed after: 18327069538910844790,4292249356257567981
