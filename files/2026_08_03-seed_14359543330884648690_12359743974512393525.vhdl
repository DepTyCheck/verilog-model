-- Seed: 14359543330884648690,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity ic is
  port (ogsz : inout real; twikhmu : buffer std_logic; mau : out std_logic; i : buffer integer);
end ic;

architecture ifd of ic is
  
begin
  -- Single-driven assignments
  i <= i;
  ogsz <= 16#8_7.061F#;
  
  -- Multi-driven assignments
  twikhmu <= mau;
  twikhmu <= 'Z';
  mau <= 'Z';
end ifd;

library ieee;
use ieee.std_logic_1164.all;

entity gwjskbj is
  port (jq : buffer real; wmfywr : in std_logic_vector(1 to 1); tj : inout integer);
end gwjskbj;

library ieee;
use ieee.std_logic_1164.all;

architecture oa of gwjskbj is
  signal zege : std_logic;
  signal dzr : integer;
  signal rdiuuerv : std_logic;
  signal uxzhn : real;
begin
  pq : entity work.ic
    port map (ogsz => uxzhn, twikhmu => rdiuuerv, mau => rdiuuerv, i => dzr);
  clzo : entity work.ic
    port map (ogsz => jq, twikhmu => zege, mau => rdiuuerv, i => tj);
  
  -- Multi-driven assignments
  rdiuuerv <= rdiuuerv;
  rdiuuerv <= '1';
  zege <= rdiuuerv;
end oa;



-- Seed after: 2560138420260666430,12359743974512393525
