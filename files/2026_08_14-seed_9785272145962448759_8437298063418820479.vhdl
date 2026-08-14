-- Seed: 9785272145962448759,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity xncocvtr is
  port (jwtwkbs : inout real_vector(3 downto 1); oorsrsexw : inout integer; wputkfa : out std_logic_vector(0 to 4));
end xncocvtr;

architecture ag of xncocvtr is
  
begin
  -- Single-driven assignments
  oorsrsexw <= oorsrsexw;
  
  -- Multi-driven assignments
  wputkfa <= "H-LW-";
  wputkfa <= ('X', '-', 'U', '-', 'Z');
  wputkfa <= ('U', '0', '0', '-', 'H');
end ag;

library ieee;
use ieee.std_logic_1164.all;

entity ycnw is
  port (wyt : linkage std_logic; oti : linkage string(2 to 5); rwywbm : buffer real; uzfcbjb : in std_logic);
end ycnw;

library ieee;
use ieee.std_logic_1164.all;

architecture evpfhz of ycnw is
  signal cjko : std_logic_vector(0 to 4);
  signal hl : integer;
  signal wiyuljl : real_vector(3 downto 1);
  signal rdc : std_logic_vector(0 to 4);
  signal houhcrl : integer;
  signal x : real_vector(3 downto 1);
begin
  uzpprkxhls : entity work.xncocvtr
    port map (jwtwkbs => x, oorsrsexw => houhcrl, wputkfa => rdc);
  xipz : entity work.xncocvtr
    port map (jwtwkbs => wiyuljl, oorsrsexw => hl, wputkfa => cjko);
  
  -- Multi-driven assignments
  rdc <= cjko;
end evpfhz;

library ieee;
use ieee.std_logic_1164.all;

entity feu is
  port (xt : in std_logic; dyttseaghg : inout std_logic_vector(2 to 1); j : linkage std_logic_vector(4 downto 0));
end feu;

architecture qdhykqlyd of feu is
  
begin
  -- Multi-driven assignments
  dyttseaghg <= dyttseaghg;
end qdhykqlyd;

library ieee;
use ieee.std_logic_1164.all;

entity ftrnxbh is
  port (dguawtm : buffer std_logic_vector(1 to 1); bsbyr : in integer; rgqtqewg : inout std_logic_vector(4 to 0));
end ftrnxbh;

library ieee;
use ieee.std_logic_1164.all;

architecture bqbtkiz of ftrnxbh is
  signal hdkrvnr : std_logic_vector(0 to 4);
  signal bjqtsvft : integer;
  signal h : real_vector(3 downto 1);
  signal r : std_logic;
  signal xsfyto : real;
  signal vteaqv : string(2 to 5);
  signal oslc : std_logic_vector(2 to 1);
  signal ine : std_logic;
  signal evtmdfugj : std_logic_vector(4 downto 0);
  signal sa : integer;
  signal wjsrivq : real_vector(3 downto 1);
begin
  liifwmmto : entity work.xncocvtr
    port map (jwtwkbs => wjsrivq, oorsrsexw => sa, wputkfa => evtmdfugj);
  xwem : entity work.feu
    port map (xt => ine, dyttseaghg => oslc, j => evtmdfugj);
  pnpes : entity work.ycnw
    port map (wyt => ine, oti => vteaqv, rwywbm => xsfyto, uzfcbjb => r);
  isq : entity work.xncocvtr
    port map (jwtwkbs => h, oorsrsexw => bjqtsvft, wputkfa => hdkrvnr);
  
  -- Multi-driven assignments
  evtmdfugj <= evtmdfugj;
end bqbtkiz;



-- Seed after: 9239470301787842787,8437298063418820479
