-- Seed: 5197907840541657120,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity erpa is
  port (xjerz : inout std_logic; jrwgvqqbqi : in time);
end erpa;

architecture jzi of erpa is
  
begin
  -- Multi-driven assignments
  xjerz <= '1';
  xjerz <= 'X';
  xjerz <= 'Z';
  xjerz <= '0';
end jzi;

entity gxrz is
  port (g : out boolean; tjklu : out boolean; raqmhqb : out real);
end gxrz;

library ieee;
use ieee.std_logic_1164.all;

architecture gobxmpdjg of gxrz is
  signal auslkgjgsv : time;
  signal y : std_logic;
begin
  tsp : entity work.erpa
    port map (xjerz => y, jrwgvqqbqi => auslkgjgsv);
  
  -- Single-driven assignments
  tjklu <= FALSE;
  auslkgjgsv <= 2#110.00110# us;
  raqmhqb <= 8#1365.20707#;
  g <= FALSE;
end gobxmpdjg;



-- Seed after: 13768155837197815380,1834764876137802293
