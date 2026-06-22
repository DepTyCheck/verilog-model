-- Seed: 12893848070797808954,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity wltli is
  port (qtrus : out real; idtkpyb : linkage std_logic; rnvn : buffer std_logic_vector(4 downto 2));
end wltli;

architecture ekfidv of wltli is
  
begin
  -- Single-driven assignments
  qtrus <= 2#1_0.0#;
  
  -- Multi-driven assignments
  rnvn <= ('W', 'X', '1');
  rnvn <= "W-1";
  rnvn <= ('L', 'W', 'W');
  rnvn <= "XZ0";
end ekfidv;



-- Seed after: 10113577187407091490,13479070923501788437
