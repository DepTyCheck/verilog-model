-- Seed: 372022605636048922,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity lmctdfacft is
  port (lamrgmixb : buffer severity_level; atkkbz : buffer std_logic_vector(2 downto 4); h : in std_logic);
end lmctdfacft;

architecture gr of lmctdfacft is
  
begin
  -- Single-driven assignments
  lamrgmixb <= WARNING;
  
  -- Multi-driven assignments
  atkkbz <= (others => '0');
  atkkbz <= "";
  atkkbz <= "";
end gr;



-- Seed after: 13600556016365105595,17924494779688682807
