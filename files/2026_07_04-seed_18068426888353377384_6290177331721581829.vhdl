-- Seed: 18068426888353377384,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity fla is
  port (iat : inout access_value_mirror; ptyrep : inout std_logic_vector(3 to 2));
end fla;

architecture qiq of fla is
  
begin
  -- Multi-driven assignments
  ptyrep <= ptyrep;
  ptyrep <= (others => '0');
  ptyrep <= ptyrep;
  ptyrep <= ptyrep;
end qiq;

use std.reflection.all;

entity o is
  port (qn : linkage boolean_vector(1 to 1); jwqhvdz : out string(5 downto 4); gnakyzbq : inout access_subtype_mirror);
end o;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture ge of o is
  signal zzxqzdq : std_logic_vector(3 to 2);
  shared variable vowg : access_value_mirror;
  signal y : std_logic_vector(3 to 2);
  shared variable tylyzh : access_value_mirror;
  signal mstl : std_logic_vector(3 to 2);
  shared variable clmpbs : access_value_mirror;
  signal b : std_logic_vector(3 to 2);
  shared variable dlcm : access_value_mirror;
begin
  vkubfv : entity work.fla
    port map (iat => dlcm, ptyrep => b);
  mhhxa : entity work.fla
    port map (iat => clmpbs, ptyrep => mstl);
  ay : entity work.fla
    port map (iat => tylyzh, ptyrep => y);
  psxobqwu : entity work.fla
    port map (iat => vowg, ptyrep => zzxqzdq);
  
  -- Single-driven assignments
  jwqhvdz <= ('t', 'n');
  
  -- Multi-driven assignments
  mstl <= "";
  y <= "";
  mstl <= b;
end ge;



-- Seed after: 15446768683915442474,6290177331721581829
