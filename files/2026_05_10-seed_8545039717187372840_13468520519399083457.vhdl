-- Seed: 8545039717187372840,13468520519399083457



entity hefob is
  port (q : buffer time; jhmsogxw : buffer bit; lgcqbmusx : out time; nmcxnquiuq : linkage time);
end hefob;



architecture ttkcttggh of hefob is
  
begin
  
end ttkcttggh;



entity xufknku is
  port (ygojg : in bit; bmm : out severity_level);
end xufknku;



architecture niyvt of xufknku is
  
begin
  
end niyvt;

library ieee;
use ieee.std_logic_1164.all;

entity xeloy is
  port (fim : inout std_logic; fczkpvug : buffer std_logic; gnl : out integer);
end xeloy;



architecture hngabwi of xeloy is
  signal teqcwcvbk : time;
  signal gbydrgbke : severity_level;
  signal vlp : time;
  signal eje : time;
  signal du : bit;
  signal x : time;
  signal sgafe : severity_level;
  signal yhvgyx : bit;
begin
  tsstpqn : entity work.xufknku
    port map (ygojg => yhvgyx, bmm => sgafe);
  srnyri : entity work.hefob
    port map (q => x, jhmsogxw => du, lgcqbmusx => eje, nmcxnquiuq => vlp);
  vvawwh : entity work.xufknku
    port map (ygojg => du, bmm => gbydrgbke);
  junhrtsxc : entity work.hefob
    port map (q => vlp, jhmsogxw => yhvgyx, lgcqbmusx => teqcwcvbk, nmcxnquiuq => eje);
end hngabwi;



-- Seed after: 5630531678306971508,13468520519399083457
