-- Seed: 13524074566627672769,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity njktbkw is
  port (hzswfdx : linkage std_logic_vector(0 downto 3); tobxch : in integer_vector(3 to 1); jcybgr : in time);
end njktbkw;

architecture sivtkxti of njktbkw is
  
begin
  
end sivtkxti;

library ieee;
use ieee.std_logic_1164.all;

entity xuktwz is
  port (kxajfya : inout boolean; iroezu : out integer; uodrc : inout std_logic; dypim : inout std_logic_vector(2 downto 3));
end xuktwz;

library ieee;
use ieee.std_logic_1164.all;

architecture pdtzau of xuktwz is
  signal wrfktgx : integer_vector(3 to 1);
  signal byhulqjmdu : time;
  signal sutb : integer_vector(3 to 1);
  signal kntr : time;
  signal lwhfnh : integer_vector(3 to 1);
  signal ci : time;
  signal i : integer_vector(3 to 1);
  signal iofotjb : std_logic_vector(0 downto 3);
begin
  evrgim : entity work.njktbkw
    port map (hzswfdx => iofotjb, tobxch => i, jcybgr => ci);
  isngyful : entity work.njktbkw
    port map (hzswfdx => dypim, tobxch => lwhfnh, jcybgr => kntr);
  vadu : entity work.njktbkw
    port map (hzswfdx => dypim, tobxch => sutb, jcybgr => byhulqjmdu);
  riszsueuoj : entity work.njktbkw
    port map (hzswfdx => dypim, tobxch => wrfktgx, jcybgr => ci);
  
  -- Single-driven assignments
  ci <= 3 min;
  kxajfya <= TRUE;
  iroezu <= 2_4_0;
  
  -- Multi-driven assignments
  iofotjb <= (others => '0');
  dypim <= (others => '0');
  iofotjb <= "";
  dypim <= (others => '0');
end pdtzau;



-- Seed after: 16214121313848278991,6697892553037813751
