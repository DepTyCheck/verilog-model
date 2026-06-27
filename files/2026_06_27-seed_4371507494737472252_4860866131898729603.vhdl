-- Seed: 4371507494737472252,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity favtukjm is
  port (yelymgja : inout time; btbyztohr : in boolean; dlasphmp : out std_logic_vector(1 to 4));
end favtukjm;

architecture gozsxlja of favtukjm is
  
begin
  -- Single-driven assignments
  yelymgja <= 2_3 ms;
  
  -- Multi-driven assignments
  dlasphmp <= "H1ZW";
end gozsxlja;

entity qanz is
  port (jlxgermg : in time; qs : in real; bslqkizv : buffer time; teaxulx : out real);
end qanz;

library ieee;
use ieee.std_logic_1164.all;

architecture qurva of qanz is
  signal kzvhqxarfa : std_logic_vector(1 to 4);
  signal tuhewkeehq : boolean;
  signal hwmevx : time;
begin
  sgsungg : entity work.favtukjm
    port map (yelymgja => hwmevx, btbyztohr => tuhewkeehq, dlasphmp => kzvhqxarfa);
  loysfu : entity work.favtukjm
    port map (yelymgja => bslqkizv, btbyztohr => tuhewkeehq, dlasphmp => kzvhqxarfa);
  
  -- Single-driven assignments
  teaxulx <= 16#6.A4E#;
  tuhewkeehq <= TRUE;
end qurva;



-- Seed after: 1073973014454054621,4860866131898729603
