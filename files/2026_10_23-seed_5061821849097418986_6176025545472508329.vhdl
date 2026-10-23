-- Seed: 5061821849097418986,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity cipk is
  port (hljqu : in string(1 downto 5); jzxhwublw : linkage std_logic_vector(0 to 0));
end cipk;

architecture wqpoziyn of cipk is
  
begin
  
end wqpoziyn;

library ieee;
use ieee.std_logic_1164.all;

entity jgamzmly is
  port (wmyvlbzhwc : linkage std_logic; yaqnsdyjg : in integer_vector(0 to 1); ag : out std_logic_vector(0 downto 4));
end jgamzmly;

library ieee;
use ieee.std_logic_1164.all;

architecture sk of jgamzmly is
  signal cck : string(1 downto 5);
  signal ikg : std_logic_vector(0 to 0);
  signal mkpn : string(1 downto 5);
begin
  kbsewhipx : entity work.cipk
    port map (hljqu => mkpn, jzxhwublw => ikg);
  nwqwdn : entity work.cipk
    port map (hljqu => cck, jzxhwublw => ikg);
  rivpur : entity work.cipk
    port map (hljqu => mkpn, jzxhwublw => ikg);
  
  -- Single-driven assignments
  cck <= "";
  mkpn <= mkpn;
  
  -- Multi-driven assignments
  ag <= ag;
end sk;

library ieee;
use ieee.std_logic_1164.all;

entity aopuzso is
  port (kohnrsyt : out character; dkr : inout std_logic);
end aopuzso;

architecture oddv of aopuzso is
  
begin
  -- Single-driven assignments
  kohnrsyt <= 'y';
end oddv;

library ieee;
use ieee.std_logic_1164.all;

entity yqvnw is
  port (uvgyn : inout integer; bffdpvu : out std_logic_vector(3 downto 1); aqd : out std_logic);
end yqvnw;

library ieee;
use ieee.std_logic_1164.all;

architecture upocauur of yqvnw is
  signal op : std_logic_vector(0 downto 4);
  signal okxdmx : std_logic_vector(0 downto 4);
  signal vmctbddfby : integer_vector(0 to 1);
  signal lwddwaa : std_logic;
  signal mmcecfswj : std_logic_vector(0 downto 4);
  signal gfqb : integer_vector(0 to 1);
  signal zlxzwidvt : std_logic;
  signal xjkmps : character;
begin
  teavec : entity work.aopuzso
    port map (kohnrsyt => xjkmps, dkr => aqd);
  gefregtpwe : entity work.jgamzmly
    port map (wmyvlbzhwc => zlxzwidvt, yaqnsdyjg => gfqb, ag => mmcecfswj);
  xoatyg : entity work.jgamzmly
    port map (wmyvlbzhwc => lwddwaa, yaqnsdyjg => vmctbddfby, ag => okxdmx);
  vjsst : entity work.jgamzmly
    port map (wmyvlbzhwc => zlxzwidvt, yaqnsdyjg => gfqb, ag => op);
  
  -- Single-driven assignments
  vmctbddfby <= gfqb;
  gfqb <= (2#1001#, 16#F_7#);
  uvgyn <= 2#0#;
end upocauur;



-- Seed after: 5652207374308661186,6176025545472508329
