-- Seed: 4677318020857019319,12011142928354116943

entity s is
  port (sn : inout string(4 downto 4));
end s;

architecture bz of s is
  
begin
  -- Single-driven assignments
  sn <= "r";
end bz;

library ieee;
use ieee.std_logic_1164.all;

entity pgu is
  port (jxanemlcqs : linkage std_logic_vector(3 to 3); danwnen : linkage time);
end pgu;

architecture qth of pgu is
  
begin
  
end qth;

library ieee;
use ieee.std_logic_1164.all;

entity yrw is
  port (qobut : inout integer; pngihtzf : in std_logic_vector(2 to 1));
end yrw;

library ieee;
use ieee.std_logic_1164.all;

architecture zocysee of yrw is
  signal m : time;
  signal bzkqpcwjs : std_logic_vector(3 to 3);
  signal mojw : string(4 downto 4);
begin
  wiiluxqd : entity work.s
    port map (sn => mojw);
  kns : entity work.pgu
    port map (jxanemlcqs => bzkqpcwjs, danwnen => m);
  
  -- Single-driven assignments
  qobut <= 8#13344#;
  
  -- Multi-driven assignments
  bzkqpcwjs <= (others => '1');
  bzkqpcwjs <= "W";
end zocysee;



-- Seed after: 11877187853032967288,12011142928354116943
