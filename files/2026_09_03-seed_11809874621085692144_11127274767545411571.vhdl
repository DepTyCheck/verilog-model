-- Seed: 11809874621085692144,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity rcispt is
  port (ywz : buffer std_logic_vector(2 downto 4));
end rcispt;

architecture ri of rcispt is
  
begin
  -- Multi-driven assignments
  ywz <= ywz;
  ywz <= ywz;
end ri;

library ieee;
use ieee.std_logic_1164.all;

entity bmpllwz is
  port (a : out boolean_vector(4 downto 1); mwkc : out boolean; up : buffer std_logic_vector(0 downto 3));
end bmpllwz;

architecture bt of bmpllwz is
  
begin
  m : entity work.rcispt
    port map (ywz => up);
  mbqwyxv : entity work.rcispt
    port map (ywz => up);
  rfrmfezjzw : entity work.rcispt
    port map (ywz => up);
  e : entity work.rcispt
    port map (ywz => up);
  
  -- Single-driven assignments
  mwkc <= mwkc;
  a <= a;
  
  -- Multi-driven assignments
  up <= up;
  up <= "";
end bt;



-- Seed after: 14191945434829122196,11127274767545411571
