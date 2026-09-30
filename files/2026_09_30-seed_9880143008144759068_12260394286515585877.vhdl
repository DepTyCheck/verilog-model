-- Seed: 9880143008144759068,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity ce is
  port (ads : inout real; lvrwlgp : out std_logic_vector(2 downto 0));
end ce;

architecture da of ce is
  
begin
  -- Single-driven assignments
  ads <= ads;
  
  -- Multi-driven assignments
  lvrwlgp <= ('U', '0', 'Z');
end da;

entity howi is
  port (ynxvokxub : inout integer; dtfmxmwtc : in severity_level; ypmrjkrdor : out real; h : out time);
end howi;

library ieee;
use ieee.std_logic_1164.all;

architecture paegq of howi is
  signal outxee : std_logic_vector(2 downto 0);
  signal sjsqyug : real;
begin
  edslaydvg : entity work.ce
    port map (ads => sjsqyug, lvrwlgp => outxee);
  vxwahoxs : entity work.ce
    port map (ads => ypmrjkrdor, lvrwlgp => outxee);
  
  -- Multi-driven assignments
  outxee <= "L-0";
  outxee <= outxee;
end paegq;

library ieee;
use ieee.std_logic_1164.all;

entity tx is
  port (waspmcf : out std_logic);
end tx;

library ieee;
use ieee.std_logic_1164.all;

architecture q of tx is
  signal cebapovbt : std_logic_vector(2 downto 0);
  signal k : real;
begin
  zixnd : entity work.ce
    port map (ads => k, lvrwlgp => cebapovbt);
  
  -- Multi-driven assignments
  cebapovbt <= cebapovbt;
  waspmcf <= 'H';
end q;

entity jxynjzo is
  port (ohlufr : inout time; hegdqbweo : buffer integer);
end jxynjzo;

library ieee;
use ieee.std_logic_1164.all;

architecture gtz of jxynjzo is
  signal tmrnzjcc : std_logic;
begin
  jaoflltse : entity work.tx
    port map (waspmcf => tmrnzjcc);
  
  -- Single-driven assignments
  hegdqbweo <= hegdqbweo;
  ohlufr <= ohlufr;
end gtz;



-- Seed after: 17168075299538747586,12260394286515585877
