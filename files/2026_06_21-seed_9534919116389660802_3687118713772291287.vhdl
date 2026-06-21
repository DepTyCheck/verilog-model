-- Seed: 9534919116389660802,3687118713772291287

entity ofjr is
  port (ga : in bit_vector(4 downto 4); umkalxwtm : out boolean_vector(0 to 3); siswn : buffer real);
end ofjr;

architecture pnwupvqnc of ofjr is
  
begin
  -- Single-driven assignments
  umkalxwtm <= (TRUE, FALSE, TRUE, FALSE);
  siswn <= 16#4_B.63#;
end pnwupvqnc;

library ieee;
use ieee.std_logic_1164.all;

entity zsyd is
  port (fd : inout bit_vector(2 downto 4); otm : buffer real_vector(3 downto 3); kwmegntq : inout std_logic_vector(1 to 3));
end zsyd;

architecture j of zsyd is
  signal ktuokj : real;
  signal jg : boolean_vector(0 to 3);
  signal bg : bit_vector(4 downto 4);
begin
  yq : entity work.ofjr
    port map (ga => bg, umkalxwtm => jg, siswn => ktuokj);
  
  -- Single-driven assignments
  bg <= (others => '0');
  otm <= (others => 4.213);
  fd <= (others => '0');
  
  -- Multi-driven assignments
  kwmegntq <= ('L', '-', '0');
  kwmegntq <= ('L', 'X', '-');
end j;



-- Seed after: 2420897023304935324,3687118713772291287
