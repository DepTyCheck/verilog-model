-- Seed: 15824109669010715956,1852660963590380551



entity cyzyn is
  port (hvtaxplu : inout time_vector(3 downto 2));
end cyzyn;



architecture r of cyzyn is
  
begin
  
end r;

library ieee;
use ieee.std_logic_1164.all;

entity sc is
  port (obevy : linkage std_logic_vector(0 downto 3));
end sc;



architecture utcmobbg of sc is
  signal flp : time_vector(3 downto 2);
begin
  dydwyb : entity work.cyzyn
    port map (hvtaxplu => flp);
end utcmobbg;



-- Seed after: 752974223490113622,1852660963590380551
