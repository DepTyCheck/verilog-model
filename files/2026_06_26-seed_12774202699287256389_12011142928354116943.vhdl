-- Seed: 12774202699287256389,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity twshbvu is
  port (xo : out time; upxiz : out std_logic_vector(1 to 4); ukznkvc : inout std_logic; ht : out severity_level);
end twshbvu;

architecture bjxpmb of twshbvu is
  
begin
  -- Multi-driven assignments
  ukznkvc <= 'W';
  ukznkvc <= 'H';
  ukznkvc <= 'H';
  ukznkvc <= 'Z';
end bjxpmb;

library ieee;
use ieee.std_logic_1164.all;

entity qdbtmd is
  port (c : linkage character; ujzfsl : linkage std_logic; vdxmawjliw : in time; lrv : out std_logic_vector(1 to 4));
end qdbtmd;

library ieee;
use ieee.std_logic_1164.all;

architecture qaxbguvm of qdbtmd is
  signal cybhtavo : severity_level;
  signal ixritq : std_logic_vector(1 to 4);
  signal gwpgjbkgz : time;
  signal jnhhz : severity_level;
  signal eusjyduh : std_logic;
  signal ij : time;
begin
  suhbvktfzv : entity work.twshbvu
    port map (xo => ij, upxiz => lrv, ukznkvc => eusjyduh, ht => jnhhz);
  p : entity work.twshbvu
    port map (xo => gwpgjbkgz, upxiz => ixritq, ukznkvc => eusjyduh, ht => cybhtavo);
  
  -- Multi-driven assignments
  ixritq <= ('Z', '0', '1', 'W');
  eusjyduh <= '-';
  eusjyduh <= 'Z';
  lrv <= ('L', 'U', '-', '0');
end qaxbguvm;

library ieee;
use ieee.std_logic_1164.all;

entity zhh is
  port (qnaye : inout boolean; qnrlg : in std_logic; cvqbft : inout std_logic_vector(2 downto 1); zghvvsb : out time_vector(2 to 2));
end zhh;

library ieee;
use ieee.std_logic_1164.all;

architecture ffkbvxyt of zhh is
  signal y : severity_level;
  signal dpunooap : std_logic;
  signal jsasb : std_logic_vector(1 to 4);
  signal t : time;
begin
  xing : entity work.twshbvu
    port map (xo => t, upxiz => jsasb, ukznkvc => dpunooap, ht => y);
  
  -- Single-driven assignments
  qnaye <= FALSE;
  zghvvsb <= (others => 16#A91.5936E# ps);
  
  -- Multi-driven assignments
  jsasb <= ('L', 'Z', 'H', 'X');
  jsasb <= "UZ0-";
  cvqbft <= "WZ";
end ffkbvxyt;



-- Seed after: 1292065381168648820,12011142928354116943
