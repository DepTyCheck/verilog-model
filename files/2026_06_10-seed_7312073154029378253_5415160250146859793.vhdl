-- Seed: 7312073154029378253,5415160250146859793

library ieee;
use ieee.std_logic_1164.all;

entity cjieli is
  port (wdgj : in integer; ynhrr : buffer std_logic_vector(0 to 3); cafwxicwrs : out std_logic; rccqx : buffer std_logic);
end cjieli;



architecture ozx of cjieli is
  
begin
  
end ozx;

library ieee;
use ieee.std_logic_1164.all;

entity ayyjlnyb is
  port (xuha : in std_logic);
end ayyjlnyb;

library ieee;
use ieee.std_logic_1164.all;

architecture ordmdmw of ayyjlnyb is
  signal uxc : std_logic;
  signal vthryuihxs : std_logic_vector(0 to 3);
  signal rlefbu : integer;
  signal pprk : std_logic;
  signal tnzstuqdlu : std_logic_vector(0 to 3);
  signal xnkemalyo : integer;
  signal gudfxumiuk : std_logic;
  signal n : std_logic_vector(0 to 3);
  signal jzhlnz : integer;
begin
  yeinjuroh : entity work.cjieli
    port map (wdgj => jzhlnz, ynhrr => n, cafwxicwrs => gudfxumiuk, rccqx => gudfxumiuk);
  anezhw : entity work.cjieli
    port map (wdgj => xnkemalyo, ynhrr => tnzstuqdlu, cafwxicwrs => pprk, rccqx => pprk);
  etpbiap : entity work.cjieli
    port map (wdgj => rlefbu, ynhrr => vthryuihxs, cafwxicwrs => gudfxumiuk, rccqx => uxc);
end ordmdmw;



entity xuza is
  port (mx : in character);
end xuza;

library ieee;
use ieee.std_logic_1164.all;

architecture c of xuza is
  signal tmpe : std_logic;
  signal zyvwgkv : std_logic_vector(0 to 3);
  signal kloj : integer;
  signal fnhificgt : std_logic;
begin
  uhxzdwjwgy : entity work.ayyjlnyb
    port map (xuha => fnhificgt);
  uaelkawmdt : entity work.cjieli
    port map (wdgj => kloj, ynhrr => zyvwgkv, cafwxicwrs => fnhificgt, rccqx => tmpe);
end c;



-- Seed after: 1535686267454559474,5415160250146859793
