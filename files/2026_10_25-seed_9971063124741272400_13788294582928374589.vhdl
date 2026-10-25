-- Seed: 9971063124741272400,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity fuhkhrnt is
  port (cttm : linkage std_logic; zcty : buffer time_vector(3 downto 1));
end fuhkhrnt;

architecture dflnxcl of fuhkhrnt is
  
begin
  -- Single-driven assignments
  zcty <= (4.123 ns, 2_4_0 us, 2_0 ps);
end dflnxcl;

entity ao is
  port (nkqt : inout integer);
end ao;

library ieee;
use ieee.std_logic_1164.all;

architecture m of ao is
  signal mrfcz : time_vector(3 downto 1);
  signal pthixmqxi : std_logic;
begin
  fz : entity work.fuhkhrnt
    port map (cttm => pthixmqxi, zcty => mrfcz);
  
  -- Single-driven assignments
  nkqt <= 2#0_1_1#;
end m;



-- Seed after: 6041186589300858319,13788294582928374589
