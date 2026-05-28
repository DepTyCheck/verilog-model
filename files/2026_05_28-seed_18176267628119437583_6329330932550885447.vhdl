-- Seed: 18176267628119437583,6329330932550885447

library ieee;
use ieee.std_logic_1164.all;

entity czm is
  port (nxo : in integer; nqenkl : linkage std_logic);
end czm;



architecture dect of czm is
  
begin
  
end dect;

library ieee;
use ieee.std_logic_1164.all;

entity nltw is
  port (zdtisjdmxu : linkage boolean; nv : buffer std_logic_vector(2 downto 4); khrxtwipfw : linkage time; qbocejfxml : buffer std_logic);
end nltw;

library ieee;
use ieee.std_logic_1164.all;

architecture nnhm of nltw is
  signal z : std_logic;
  signal cybwd : integer;
begin
  hchd : entity work.czm
    port map (nxo => cybwd, nqenkl => z);
  ldfcq : entity work.czm
    port map (nxo => cybwd, nqenkl => qbocejfxml);
end nnhm;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (wvospfbubq : buffer std_logic_vector(2 downto 2));
end p;



architecture zvukntz of p is
  
begin
  
end zvukntz;

library ieee;
use ieee.std_logic_1164.all;

entity dhoj is
  port (ehzic : inout integer; xlfcvinm : inout real; mnmtetcka : buffer std_logic; qseab : buffer time);
end dhoj;

library ieee;
use ieee.std_logic_1164.all;

architecture qhxyx of dhoj is
  signal v : std_logic_vector(2 downto 2);
  signal sirzobzfw : std_logic_vector(2 downto 2);
begin
  vomtlpxwu : entity work.p
    port map (wvospfbubq => sirzobzfw);
  gvaqdlmwgi : entity work.p
    port map (wvospfbubq => sirzobzfw);
  metprekxdb : entity work.czm
    port map (nxo => ehzic, nqenkl => mnmtetcka);
  arbyebck : entity work.p
    port map (wvospfbubq => v);
end qhxyx;



-- Seed after: 14321561763497844063,6329330932550885447
