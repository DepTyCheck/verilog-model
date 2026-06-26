-- Seed: 8865435847901001129,12011142928354116943

entity sqerwm is
  port (reqcak : out integer; jcfdznoq : buffer real);
end sqerwm;

architecture ramvmpx of sqerwm is
  
begin
  
end ramvmpx;

entity jzeeh is
  port (uzu : in time; g : buffer bit; d : out integer);
end jzeeh;

architecture ecii of jzeeh is
  signal hlwr : real;
begin
  embkfgrjpt : entity work.sqerwm
    port map (reqcak => d, jcfdznoq => hlwr);
  
  -- Single-driven assignments
  g <= '0';
end ecii;

library ieee;
use ieee.std_logic_1164.all;

entity tr is
  port (c : out std_logic_vector(1 to 3));
end tr;

architecture qdztp of tr is
  signal cohqvoz : real;
  signal hu : integer;
begin
  eqfhxvbchc : entity work.sqerwm
    port map (reqcak => hu, jcfdznoq => cohqvoz);
  
  -- Multi-driven assignments
  c <= ('H', '0', 'Z');
  c <= "11Z";
  c <= ('0', '0', 'Z');
  c <= ('Z', '1', 'U');
end qdztp;



-- Seed after: 9040874219097505719,12011142928354116943
