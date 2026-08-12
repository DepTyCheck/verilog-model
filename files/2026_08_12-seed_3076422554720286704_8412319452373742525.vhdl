-- Seed: 3076422554720286704,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity yek is
  port (utacikymna : out std_logic);
end yek;

architecture k of yek is
  
begin
  
end k;

entity entwpr is
  port (e : out boolean; ywlci : buffer integer; xyampolda : inout boolean);
end entwpr;

library ieee;
use ieee.std_logic_1164.all;

architecture lmzpffpaa of entwpr is
  signal gcwbn : std_logic;
begin
  jcffkankf : entity work.yek
    port map (utacikymna => gcwbn);
  
  -- Single-driven assignments
  xyampolda <= FALSE;
  ywlci <= 16#B6#;
  e <= FALSE;
  
  -- Multi-driven assignments
  gcwbn <= gcwbn;
  gcwbn <= 'L';
  gcwbn <= 'W';
  gcwbn <= '1';
end lmzpffpaa;



-- Seed after: 8019392501000197343,8412319452373742525
