-- Seed: 4316759144824525467,12359743974512393525

entity ac is
  port (rbfnd : inout severity_level);
end ac;

architecture a of ac is
  
begin
  -- Single-driven assignments
  rbfnd <= FAILURE;
end a;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (nudx : out std_logic; q : in integer_vector(2 downto 0));
end y;

architecture onph of y is
  signal k : severity_level;
  signal wepzwl : severity_level;
begin
  boamceg : entity work.ac
    port map (rbfnd => wepzwl);
  uzbhgzv : entity work.ac
    port map (rbfnd => k);
  
  -- Multi-driven assignments
  nudx <= '0';
  nudx <= '0';
  nudx <= '-';
end onph;



-- Seed after: 15509100767948990918,12359743974512393525
