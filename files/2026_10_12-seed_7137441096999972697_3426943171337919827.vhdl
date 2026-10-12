-- Seed: 7137441096999972697,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity ml is
  port (ug : out std_logic; gw : buffer std_logic);
end ml;

architecture unrz of ml is
  
begin
  -- Multi-driven assignments
  ug <= 'H';
  gw <= gw;
  gw <= ug;
  gw <= gw;
end unrz;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (tzwmtc : out integer; assvw : in std_logic_vector(0 to 1); p : inout integer; pdu : linkage integer);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture ibtzzz of h is
  signal bt : std_logic;
  signal lycwdzabpj : std_logic;
begin
  npb : entity work.ml
    port map (ug => lycwdzabpj, gw => bt);
  
  -- Single-driven assignments
  p <= p;
  tzwmtc <= p;
  
  -- Multi-driven assignments
  lycwdzabpj <= lycwdzabpj;
  lycwdzabpj <= lycwdzabpj;
  lycwdzabpj <= 'Z';
  lycwdzabpj <= bt;
end ibtzzz;



-- Seed after: 7079506449161378796,3426943171337919827
