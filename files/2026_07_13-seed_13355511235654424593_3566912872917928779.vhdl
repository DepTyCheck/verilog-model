-- Seed: 13355511235654424593,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;

entity qlrd is
  port (stgjzodw : inout std_logic; b : in boolean_vector(2 downto 1));
end qlrd;

architecture pjnywnkg of qlrd is
  
begin
  -- Multi-driven assignments
  stgjzodw <= stgjzodw;
  stgjzodw <= 'W';
  stgjzodw <= '-';
  stgjzodw <= 'X';
end pjnywnkg;

use std.reflection.all;

entity ltq is
  port (quzrnqmr : inout physical_value_mirror);
end ltq;

library ieee;
use ieee.std_logic_1164.all;

architecture uupg of ltq is
  signal rhflj : boolean_vector(2 downto 1);
  signal tanl : std_logic;
  signal sb : boolean_vector(2 downto 1);
  signal dhelllpfys : std_logic;
begin
  fyyvpzgvou : entity work.qlrd
    port map (stgjzodw => dhelllpfys, b => sb);
  fjbeo : entity work.qlrd
    port map (stgjzodw => tanl, b => rhflj);
  hnmomzus : entity work.qlrd
    port map (stgjzodw => dhelllpfys, b => sb);
  
  -- Single-driven assignments
  sb <= sb;
  rhflj <= sb;
end uupg;



-- Seed after: 1964087746158122200,3566912872917928779
