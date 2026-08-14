-- Seed: 2008565536358525971,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity cnxevwog is
  port (imotiypvt : buffer std_logic_vector(2 to 3));
end cnxevwog;

architecture lfwyr of cnxevwog is
  
begin
  -- Multi-driven assignments
  imotiypvt <= "UU";
  imotiypvt <= imotiypvt;
  imotiypvt <= ('X', '0');
  imotiypvt <= ('0', '1');
end lfwyr;

library ieee;
use ieee.std_logic_1164.all;

entity vwv is
  port (ssjzov : inout real; lqzdimo : inout integer; p : buffer real; sfndhec : out std_logic);
end vwv;

library ieee;
use ieee.std_logic_1164.all;

architecture reivudxgh of vwv is
  signal wqmczd : std_logic_vector(2 to 3);
  signal d : std_logic_vector(2 to 3);
  signal vlv : std_logic_vector(2 to 3);
begin
  pbdkqvb : entity work.cnxevwog
    port map (imotiypvt => vlv);
  uywbr : entity work.cnxevwog
    port map (imotiypvt => d);
  yizfxaout : entity work.cnxevwog
    port map (imotiypvt => vlv);
  mytusfqhr : entity work.cnxevwog
    port map (imotiypvt => wqmczd);
  
  -- Single-driven assignments
  p <= 2.0_4_4_3;
  lqzdimo <= 8#42467#;
  ssjzov <= 4_1_3.4;
  
  -- Multi-driven assignments
  vlv <= "H1";
end reivudxgh;



-- Seed after: 3400861172630233593,8437298063418820479
