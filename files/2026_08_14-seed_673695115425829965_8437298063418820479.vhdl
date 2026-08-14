-- Seed: 673695115425829965,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity xuvsg is
  port (zv : in std_logic_vector(4 downto 4));
end xuvsg;

architecture akbddzu of xuvsg is
  
begin
  
end akbddzu;

entity xmq is
  port (y : in time);
end xmq;

library ieee;
use ieee.std_logic_1164.all;

architecture xckiajghtk of xmq is
  signal axirqz : std_logic_vector(4 downto 4);
  signal ptvwbld : std_logic_vector(4 downto 4);
begin
  wlqrhcvhd : entity work.xuvsg
    port map (zv => ptvwbld);
  ynijycrj : entity work.xuvsg
    port map (zv => ptvwbld);
  guomnchvcd : entity work.xuvsg
    port map (zv => axirqz);
  sbcsluvda : entity work.xuvsg
    port map (zv => ptvwbld);
  
  -- Multi-driven assignments
  ptvwbld <= (others => '1');
end xckiajghtk;



-- Seed after: 2110173215300838738,8437298063418820479
