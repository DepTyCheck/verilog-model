-- Seed: 13672118395275602191,13479070923501788437

entity rznx is
  port (auybd : inout time; oovfeg : buffer integer; jzsjt : in real);
end rznx;

architecture yh of rznx is
  
begin
  
end yh;

library ieee;
use ieee.std_logic_1164.all;

entity ce is
  port (b : out std_logic_vector(2 to 1); eqpurhz : in time; iojbdgs : buffer std_logic);
end ce;

architecture na of ce is
  signal jlfrcmvuez : integer;
  signal jixddxzkk : time;
  signal efppu : real;
  signal msklpd : integer;
  signal jhsewndia : time;
begin
  uxeoi : entity work.rznx
    port map (auybd => jhsewndia, oovfeg => msklpd, jzsjt => efppu);
  srjdpk : entity work.rznx
    port map (auybd => jixddxzkk, oovfeg => jlfrcmvuez, jzsjt => efppu);
  
  -- Single-driven assignments
  efppu <= 1412.020;
  
  -- Multi-driven assignments
  iojbdgs <= 'W';
  iojbdgs <= '1';
end na;



-- Seed after: 105285479415835884,13479070923501788437
