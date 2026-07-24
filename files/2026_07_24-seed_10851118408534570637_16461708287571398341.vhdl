-- Seed: 10851118408534570637,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity oirqouao is
  port (uhoealwtn : out integer; wywf : in std_logic);
end oirqouao;

architecture pgdibg of oirqouao is
  
begin
  -- Single-driven assignments
  uhoealwtn <= 2#0011#;
end pgdibg;

entity wsj is
  port (evjhea : in integer);
end wsj;

library ieee;
use ieee.std_logic_1164.all;

architecture hfxzfng of wsj is
  signal m : integer;
  signal j : std_logic;
  signal yswmxqq : integer;
  signal vvl : std_logic;
  signal sus : integer;
  signal ppwo : std_logic;
  signal qkspu : integer;
begin
  tkhz : entity work.oirqouao
    port map (uhoealwtn => qkspu, wywf => ppwo);
  klhga : entity work.oirqouao
    port map (uhoealwtn => sus, wywf => vvl);
  xnjrpsd : entity work.oirqouao
    port map (uhoealwtn => yswmxqq, wywf => j);
  qkgfzxzf : entity work.oirqouao
    port map (uhoealwtn => m, wywf => vvl);
  
  -- Multi-driven assignments
  ppwo <= vvl;
  ppwo <= '0';
  ppwo <= vvl;
  ppwo <= 'X';
end hfxzfng;



-- Seed after: 1486321676888364339,16461708287571398341
