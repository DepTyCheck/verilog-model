-- Seed: 5371336594784229131,7332793847894666635

library ieee;
use ieee.std_logic_1164.all;

entity ujhppxmvck is
  port (lqmduqicw : buffer std_logic_vector(4 to 1));
end ujhppxmvck;



architecture lzjdgjer of ujhppxmvck is
  
begin
  
end lzjdgjer;

library ieee;
use ieee.std_logic_1164.all;

entity tvkf is
  port (zxzao : in integer; mwggubjv : inout std_logic_vector(2 to 1); msbteyhry : in std_logic);
end tvkf;

library ieee;
use ieee.std_logic_1164.all;

architecture syvw of tvkf is
  signal eljcr : std_logic_vector(4 to 1);
  signal yksowppf : std_logic_vector(4 to 1);
begin
  ghnwroigod : entity work.ujhppxmvck
    port map (lqmduqicw => yksowppf);
  o : entity work.ujhppxmvck
    port map (lqmduqicw => eljcr);
end syvw;



entity icmwj is
  port (fqfb : in time_vector(1 to 0); awu : in real; whaqr : in real);
end icmwj;

library ieee;
use ieee.std_logic_1164.all;

architecture xvxojqkcm of icmwj is
  signal uwliepqfnn : std_logic;
  signal axvldrnjv : integer;
  signal rolflkenm : std_logic_vector(2 to 1);
begin
  kqxnj : entity work.ujhppxmvck
    port map (lqmduqicw => rolflkenm);
  ybsbaaawh : entity work.tvkf
    port map (zxzao => axvldrnjv, mwggubjv => rolflkenm, msbteyhry => uwliepqfnn);
  bx : entity work.tvkf
    port map (zxzao => axvldrnjv, mwggubjv => rolflkenm, msbteyhry => uwliepqfnn);
end xvxojqkcm;



-- Seed after: 6779731435573162050,7332793847894666635
