-- Seed: 16021711758669112168,11181851762153539145

library ieee;
use ieee.std_logic_1164.all;

entity dhnrzuwo is
  port (omprjc : out boolean_vector(4 downto 2); pixx : in std_logic; ejbowl : linkage integer);
end dhnrzuwo;



architecture aymmx of dhnrzuwo is
  
begin
  
end aymmx;

library ieee;
use ieee.std_logic_1164.all;

entity hgwjvny is
  port (tyz : buffer severity_level; icpfcmi : inout std_logic_vector(3 downto 4));
end hgwjvny;

library ieee;
use ieee.std_logic_1164.all;

architecture tksxonfrsa of hgwjvny is
  signal crjudaf : integer;
  signal whwildfbm : std_logic;
  signal ynml : boolean_vector(4 downto 2);
begin
  hdwpsvrfey : entity work.dhnrzuwo
    port map (omprjc => ynml, pixx => whwildfbm, ejbowl => crjudaf);
end tksxonfrsa;

library ieee;
use ieee.std_logic_1164.all;

entity xgptwkkmi is
  port (mihfoe : linkage std_logic; iznu : buffer bit_vector(3 downto 4));
end xgptwkkmi;

library ieee;
use ieee.std_logic_1164.all;

architecture jmbwfbs of xgptwkkmi is
  signal rmatnzqezy : std_logic_vector(3 downto 4);
  signal ycupm : severity_level;
  signal qskta : integer;
  signal juxt : std_logic;
  signal jts : boolean_vector(4 downto 2);
begin
  jkjytyu : entity work.dhnrzuwo
    port map (omprjc => jts, pixx => juxt, ejbowl => qskta);
  rb : entity work.hgwjvny
    port map (tyz => ycupm, icpfcmi => rmatnzqezy);
end jmbwfbs;

library ieee;
use ieee.std_logic_1164.all;

entity pdq is
  port (bonwbrqfuo : linkage std_logic_vector(3 downto 2));
end pdq;

library ieee;
use ieee.std_logic_1164.all;

architecture qk of pdq is
  signal yyuweoa : severity_level;
  signal fbqc : integer;
  signal zyvel : std_logic;
  signal kewflk : boolean_vector(4 downto 2);
  signal kh : severity_level;
  signal murxohefe : std_logic_vector(3 downto 4);
  signal jppx : severity_level;
begin
  w : entity work.hgwjvny
    port map (tyz => jppx, icpfcmi => murxohefe);
  pqx : entity work.hgwjvny
    port map (tyz => kh, icpfcmi => murxohefe);
  cnroz : entity work.dhnrzuwo
    port map (omprjc => kewflk, pixx => zyvel, ejbowl => fbqc);
  jeoyvuxtv : entity work.hgwjvny
    port map (tyz => yyuweoa, icpfcmi => murxohefe);
end qk;



-- Seed after: 18443342258289057656,11181851762153539145
