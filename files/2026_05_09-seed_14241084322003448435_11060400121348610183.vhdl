-- Seed: 14241084322003448435,11060400121348610183

library ieee;
use ieee.std_logic_1164.all;

entity snrllm is
  port (yzodrttzkw : inout std_logic);
end snrllm;



architecture mrhqyrtfaf of snrllm is
  
begin
  
end mrhqyrtfaf;

library ieee;
use ieee.std_logic_1164.all;

entity icccifju is
  port (rnisusj : buffer std_logic);
end icccifju;



architecture tmctwyte of icccifju is
  
begin
  cjbrbonbs : entity work.snrllm
    port map (yzodrttzkw => rnisusj);
end tmctwyte;



entity sduqe is
  port (hdyz : out integer; vorplmh : inout real);
end sduqe;

library ieee;
use ieee.std_logic_1164.all;

architecture szzm of sduqe is
  signal bhikavpec : std_logic;
  signal yq : std_logic;
begin
  ow : entity work.icccifju
    port map (rnisusj => yq);
  onsehjfrn : entity work.snrllm
    port map (yzodrttzkw => bhikavpec);
end szzm;

library ieee;
use ieee.std_logic_1164.all;

entity xle is
  port (zofuizp : out std_logic; glrc : buffer std_logic);
end xle;



architecture fm of xle is
  signal qmzklitk : real;
  signal ez : integer;
  signal k : real;
  signal zlyakorz : integer;
begin
  dhvh : entity work.sduqe
    port map (hdyz => zlyakorz, vorplmh => k);
  xvmymcv : entity work.sduqe
    port map (hdyz => ez, vorplmh => qmzklitk);
end fm;



-- Seed after: 17467804947584251422,11060400121348610183
