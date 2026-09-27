-- Seed: 14223114676647879971,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity lcikqy is
  port (cbfrovsyqi : buffer std_logic; plfkwlxiiu : inout string(1 to 2));
end lcikqy;

architecture dmrddw of lcikqy is
  
begin
  -- Single-driven assignments
  plfkwlxiiu <= plfkwlxiiu;
end dmrddw;

entity xwapd is
  port (zraq : buffer integer_vector(0 downto 3); gfjihrgrh : buffer real);
end xwapd;

architecture lwkkzguaca of xwapd is
  
begin
  -- Single-driven assignments
  gfjihrgrh <= gfjihrgrh;
  zraq <= (others => 0);
end lwkkzguaca;

library ieee;
use ieee.std_logic_1164.all;

entity emovjysmdm is
  port (hvpvu : buffer time; iwxz : linkage time; oc : buffer std_logic_vector(2 downto 4));
end emovjysmdm;

architecture syefmdlu of emovjysmdm is
  
begin
  -- Single-driven assignments
  hvpvu <= hvpvu;
  
  -- Multi-driven assignments
  oc <= "";
  oc <= oc;
  oc <= (others => '0');
end syefmdlu;

entity yqgodca is
  port (jmk : buffer time);
end yqgodca;

library ieee;
use ieee.std_logic_1164.all;

architecture hum of yqgodca is
  signal brppt : string(1 to 2);
  signal hp : std_logic;
  signal cyqrvhfamg : string(1 to 2);
  signal xgwq : std_logic;
  signal hxxtplv : real;
  signal puaylkxug : integer_vector(0 downto 3);
begin
  ft : entity work.xwapd
    port map (zraq => puaylkxug, gfjihrgrh => hxxtplv);
  wn : entity work.lcikqy
    port map (cbfrovsyqi => xgwq, plfkwlxiiu => cyqrvhfamg);
  hw : entity work.lcikqy
    port map (cbfrovsyqi => hp, plfkwlxiiu => brppt);
  
  -- Single-driven assignments
  jmk <= jmk;
  
  -- Multi-driven assignments
  hp <= xgwq;
end hum;



-- Seed after: 7644776728000551458,6379010654866854599
