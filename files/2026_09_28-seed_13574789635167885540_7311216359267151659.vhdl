-- Seed: 13574789635167885540,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity uusbnlgvuo is
  port (nbkicjxulo : in std_logic_vector(2 downto 4));
end uusbnlgvuo;

architecture yydconaai of uusbnlgvuo is
  
begin
  
end yydconaai;

entity xxavxeelks is
  port (nf : out integer_vector(0 to 4); vkgo : linkage time_vector(3 downto 2); tabggmzxh : out real);
end xxavxeelks;

library ieee;
use ieee.std_logic_1164.all;

architecture jdvsmuq of xxavxeelks is
  signal ywcu : std_logic_vector(2 downto 4);
begin
  ifji : entity work.uusbnlgvuo
    port map (nbkicjxulo => ywcu);
end jdvsmuq;

entity exgazzb is
  port (martkdg : linkage real);
end exgazzb;

library ieee;
use ieee.std_logic_1164.all;

architecture sds of exgazzb is
  signal ioafq : real;
  signal sebjxgqcq : time_vector(3 downto 2);
  signal oqbodjxu : integer_vector(0 to 4);
  signal r : real;
  signal yfeybx : time_vector(3 downto 2);
  signal sfyqlwe : integer_vector(0 to 4);
  signal zpef : std_logic_vector(2 downto 4);
begin
  zh : entity work.uusbnlgvuo
    port map (nbkicjxulo => zpef);
  mwixn : entity work.xxavxeelks
    port map (nf => sfyqlwe, vkgo => yfeybx, tabggmzxh => r);
  x : entity work.xxavxeelks
    port map (nf => oqbodjxu, vkgo => sebjxgqcq, tabggmzxh => ioafq);
  
  -- Multi-driven assignments
  zpef <= zpef;
  zpef <= zpef;
  zpef <= zpef;
end sds;



-- Seed after: 8320437114469297547,7311216359267151659
