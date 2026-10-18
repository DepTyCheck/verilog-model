-- Seed: 14844170206660507466,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity jmhhsthph is
  port (b : out real; qnqby : in std_logic_vector(4 to 1); liqyhai : inout bit; ycv : buffer time_vector(4 to 2));
end jmhhsthph;

architecture zezq of jmhhsthph is
  
begin
  -- Single-driven assignments
  b <= 1.3_1_0;
  liqyhai <= '1';
  ycv <= ycv;
end zezq;

library ieee;
use ieee.std_logic_1164.all;

entity ghykp is
  port (paktld : inout std_logic_vector(1 downto 1); djjexb : buffer time);
end ghykp;

library ieee;
use ieee.std_logic_1164.all;

architecture flv of ghykp is
  signal vxna : time_vector(4 to 2);
  signal elmresca : bit;
  signal bq : std_logic_vector(4 to 1);
  signal bg : real;
begin
  ceuldw : entity work.jmhhsthph
    port map (b => bg, qnqby => bq, liqyhai => elmresca, ycv => vxna);
  
  -- Single-driven assignments
  djjexb <= 8#7_2# us;
  
  -- Multi-driven assignments
  paktld <= paktld;
end flv;



-- Seed after: 11577163383917257325,14108379317686958507
