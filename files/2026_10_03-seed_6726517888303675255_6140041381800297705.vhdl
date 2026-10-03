-- Seed: 6726517888303675255,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity tozdtdnwzz is
  port (ct : in real; kwm : out std_logic_vector(3 to 0); qrfreqq : in real);
end tozdtdnwzz;

architecture ilsklanttm of tozdtdnwzz is
  
begin
  
end ilsklanttm;

entity r is
  port (zlodipmhz : out real);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture vslhzsqfz of r is
  signal g : real;
  signal djyen : std_logic_vector(3 to 0);
  signal nqutlbu : real;
  signal rmmz : real;
  signal aujgm : real;
  signal qesc : std_logic_vector(3 to 0);
  signal aeptonbbn : real;
begin
  omtua : entity work.tozdtdnwzz
    port map (ct => aeptonbbn, kwm => qesc, qrfreqq => aujgm);
  zyzzfh : entity work.tozdtdnwzz
    port map (ct => zlodipmhz, kwm => qesc, qrfreqq => rmmz);
  p : entity work.tozdtdnwzz
    port map (ct => nqutlbu, kwm => djyen, qrfreqq => g);
  
  -- Single-driven assignments
  zlodipmhz <= 0_3.4_0_1_2;
  aujgm <= aujgm;
  
  -- Multi-driven assignments
  qesc <= (others => '0');
  qesc <= (others => '0');
  qesc <= qesc;
end vslhzsqfz;

library ieee;
use ieee.std_logic_1164.all;

entity epdokoghs is
  port (tlysu : linkage bit_vector(1 to 3); yrdq : out std_logic_vector(3 to 2));
end epdokoghs;

library ieee;
use ieee.std_logic_1164.all;

architecture grlz of epdokoghs is
  signal frrjayyir : std_logic_vector(3 to 0);
  signal pnadkg : real;
  signal npyuiy : std_logic_vector(3 to 0);
  signal ddd : real;
begin
  pk : entity work.tozdtdnwzz
    port map (ct => ddd, kwm => npyuiy, qrfreqq => ddd);
  mametgqoox : entity work.tozdtdnwzz
    port map (ct => pnadkg, kwm => frrjayyir, qrfreqq => ddd);
  
  -- Single-driven assignments
  ddd <= ddd;
  pnadkg <= 16#A85.E#;
  
  -- Multi-driven assignments
  npyuiy <= "";
  yrdq <= "";
  frrjayyir <= "";
  frrjayyir <= "";
end grlz;

entity ysiff is
  port (pabwjlvw : in time; ypww : buffer time; e : in real);
end ysiff;

library ieee;
use ieee.std_logic_1164.all;

architecture limltnva of ysiff is
  signal kfpwzm : std_logic_vector(3 to 2);
  signal ke : bit_vector(1 to 3);
begin
  vqburt : entity work.epdokoghs
    port map (tlysu => ke, yrdq => kfpwzm);
  
  -- Single-driven assignments
  ypww <= 8#2_1_5# ps;
end limltnva;



-- Seed after: 12845636422531972177,6140041381800297705
