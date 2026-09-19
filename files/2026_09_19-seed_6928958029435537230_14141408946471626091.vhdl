-- Seed: 6928958029435537230,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (koznw : in std_logic_vector(1 to 3));
end d;

architecture w of d is
  
begin
  
end w;

entity nzxni is
  port (xhipdio : linkage time; iwzcwhpoc : in integer);
end nzxni;

library ieee;
use ieee.std_logic_1164.all;

architecture qnbazjydg of nzxni is
  signal ekhppze : std_logic_vector(1 to 3);
  signal nruc : std_logic_vector(1 to 3);
begin
  uwdkxf : entity work.d
    port map (koznw => nruc);
  jzv : entity work.d
    port map (koznw => nruc);
  eepjlujfy : entity work.d
    port map (koznw => nruc);
  uo : entity work.d
    port map (koznw => ekhppze);
  
  -- Multi-driven assignments
  nruc <= nruc;
  nruc <= nruc;
  ekhppze <= ('0', 'X', 'U');
  nruc <= "LLZ";
end qnbazjydg;

library ieee;
use ieee.std_logic_1164.all;

entity gnfpgx is
  port (aoqzminig : buffer time; uthufpn : buffer integer; cjdk : inout std_logic);
end gnfpgx;

architecture uthjcciu of gnfpgx is
  
begin
  jgmobbpdpc : entity work.nzxni
    port map (xhipdio => aoqzminig, iwzcwhpoc => uthufpn);
  
  -- Single-driven assignments
  uthufpn <= 42;
end uthjcciu;

library ieee;
use ieee.std_logic_1164.all;

entity eqilue is
  port (zirges : inout std_logic_vector(4 to 1); srjqf : out severity_level; hpmnw : in time);
end eqilue;

library ieee;
use ieee.std_logic_1164.all;

architecture dfzid of eqilue is
  signal mo : std_logic;
  signal gkidpy : integer;
  signal vkbvsufie : time;
  signal pcuueu : std_logic_vector(1 to 3);
  signal yhcrge : std_logic_vector(1 to 3);
begin
  aepmrhe : entity work.d
    port map (koznw => yhcrge);
  mvknp : entity work.d
    port map (koznw => pcuueu);
  pfq : entity work.d
    port map (koznw => yhcrge);
  yijr : entity work.gnfpgx
    port map (aoqzminig => vkbvsufie, uthufpn => gkidpy, cjdk => mo);
  
  -- Single-driven assignments
  srjqf <= srjqf;
  
  -- Multi-driven assignments
  pcuueu <= ('X', 'W', 'X');
end dfzid;



-- Seed after: 6825496052187326900,14141408946471626091
