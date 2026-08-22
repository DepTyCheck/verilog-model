-- Seed: 13469888031087017262,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity kxfnx is
  port (llxmclu : inout real; lbgbxdctud : in integer_vector(0 to 3); nbnxqf : buffer std_logic);
end kxfnx;

architecture qgl of kxfnx is
  
begin
  -- Single-driven assignments
  llxmclu <= 1_4.0_3_2;
  
  -- Multi-driven assignments
  nbnxqf <= '1';
  nbnxqf <= 'U';
end qgl;

library ieee;
use ieee.std_logic_1164.all;

entity ch is
  port (rt : in boolean; myztq : buffer bit; av : out std_logic_vector(3 to 0));
end ch;

library ieee;
use ieee.std_logic_1164.all;

architecture r of ch is
  signal k : integer_vector(0 to 3);
  signal rdzhmshgpt : real;
  signal imbvzh : real;
  signal dw : real;
  signal vlzmmicg : std_logic;
  signal nfxrlzn : integer_vector(0 to 3);
  signal vabnmlqjvi : real;
begin
  podyooqebj : entity work.kxfnx
    port map (llxmclu => vabnmlqjvi, lbgbxdctud => nfxrlzn, nbnxqf => vlzmmicg);
  dqxiv : entity work.kxfnx
    port map (llxmclu => dw, lbgbxdctud => nfxrlzn, nbnxqf => vlzmmicg);
  yoqobzu : entity work.kxfnx
    port map (llxmclu => imbvzh, lbgbxdctud => nfxrlzn, nbnxqf => vlzmmicg);
  seyyw : entity work.kxfnx
    port map (llxmclu => rdzhmshgpt, lbgbxdctud => k, nbnxqf => vlzmmicg);
  
  -- Single-driven assignments
  myztq <= '1';
end r;

library ieee;
use ieee.std_logic_1164.all;

entity wdypcb is
  port (yuliwo : inout std_logic_vector(3 to 4); osisdwelk : inout time);
end wdypcb;

library ieee;
use ieee.std_logic_1164.all;

architecture jgafjcicm of wdypcb is
  signal zrdtwoolfu : std_logic;
  signal lht : integer_vector(0 to 3);
  signal pkpjb : real;
  signal vtmfsrc : std_logic;
  signal hjnnwnh : integer_vector(0 to 3);
  signal pbonfzcqn : real;
begin
  eaz : entity work.kxfnx
    port map (llxmclu => pbonfzcqn, lbgbxdctud => hjnnwnh, nbnxqf => vtmfsrc);
  ec : entity work.kxfnx
    port map (llxmclu => pkpjb, lbgbxdctud => lht, nbnxqf => zrdtwoolfu);
  
  -- Single-driven assignments
  lht <= (0_2_4_4, 16#5_E_C#, 221, 8#1#);
  hjnnwnh <= hjnnwnh;
  osisdwelk <= 434 fs;
  
  -- Multi-driven assignments
  zrdtwoolfu <= vtmfsrc;
  vtmfsrc <= 'Z';
end jgafjcicm;



-- Seed after: 16548962768520002734,5805648483995786113
