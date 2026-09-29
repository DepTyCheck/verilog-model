-- Seed: 13051459117186109341,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity ayoctbu is
  port (heve : in integer; c : in std_logic; xemrqnx : in time_vector(1 downto 3));
end ayoctbu;

architecture honfr of ayoctbu is
  
begin
  
end honfr;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (hbmre : in std_logic; zamafsoml : linkage std_logic; b : buffer std_logic; fjvlexc : inout severity_level);
end j;

architecture mnepsoqwmn of j is
  
begin
  -- Single-driven assignments
  fjvlexc <= WARNING;
  
  -- Multi-driven assignments
  b <= b;
  b <= 'Z';
end mnepsoqwmn;

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (wov : out std_logic; ll : inout time; raejfwwhij : out std_logic_vector(0 to 0); dulfqu : buffer std_logic_vector(3 to 2));
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture sbqjsa of i is
  signal b : integer;
  signal rkffkro : time_vector(1 downto 3);
  signal deyu : std_logic;
  signal qky : time_vector(1 downto 3);
  signal ulftqlkrs : std_logic;
  signal pkpcxyj : integer;
  signal ywcga : severity_level;
  signal kefx : std_logic;
begin
  sxaevfwzu : entity work.j
    port map (hbmre => wov, zamafsoml => kefx, b => wov, fjvlexc => ywcga);
  jglh : entity work.ayoctbu
    port map (heve => pkpcxyj, c => ulftqlkrs, xemrqnx => qky);
  wqpl : entity work.ayoctbu
    port map (heve => pkpcxyj, c => deyu, xemrqnx => rkffkro);
  hgkxebhzs : entity work.ayoctbu
    port map (heve => b, c => wov, xemrqnx => qky);
  
  -- Single-driven assignments
  rkffkro <= qky;
  pkpcxyj <= pkpcxyj;
  ll <= ll;
  
  -- Multi-driven assignments
  deyu <= '0';
  dulfqu <= dulfqu;
end sbqjsa;



-- Seed after: 6851249898523510405,10940991575366938685
