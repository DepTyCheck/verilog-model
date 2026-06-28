-- Seed: 1407483710358882438,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity cy is
  port (joyd : buffer std_logic_vector(2 downto 4); zleuk : linkage real; mucchj : out std_logic; pvuxhp : inout std_logic_vector(0 downto 3));
end cy;

architecture nudfi of cy is
  
begin
  -- Multi-driven assignments
  pvuxhp <= "";
  joyd <= "";
end nudfi;

entity g is
  port (wjxk : out time_vector(2 to 2));
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture hrn of g is
  signal oh : std_logic_vector(0 downto 3);
  signal pfopl : std_logic;
  signal trwusphg : real;
  signal vcc : std_logic_vector(2 downto 4);
begin
  iop : entity work.cy
    port map (joyd => vcc, zleuk => trwusphg, mucchj => pfopl, pvuxhp => oh);
  
  -- Single-driven assignments
  wjxk <= (others => 1 min);
  
  -- Multi-driven assignments
  vcc <= "";
  vcc <= (others => '0');
  pfopl <= 'H';
end hrn;



-- Seed after: 12392033829828259333,6697892553037813751
