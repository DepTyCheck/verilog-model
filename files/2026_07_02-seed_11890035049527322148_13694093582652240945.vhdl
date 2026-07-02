-- Seed: 11890035049527322148,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity jklwv is
  port (thkdqrwv : in std_logic_vector(2 downto 3); njmjvabrei : buffer time; kxwsmepw : in character);
end jklwv;

architecture fzmucc of jklwv is
  
begin
  -- Single-driven assignments
  njmjvabrei <= 0_2_4 ns;
end fzmucc;

entity p is
  port (dvzhh : buffer bit);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture scd of p is
  signal xsqju : character;
  signal xuzbby : time;
  signal om : std_logic_vector(2 downto 3);
begin
  ycvgzwtdaq : entity work.jklwv
    port map (thkdqrwv => om, njmjvabrei => xuzbby, kxwsmepw => xsqju);
  
  -- Single-driven assignments
  dvzhh <= '1';
  xsqju <= 's';
  
  -- Multi-driven assignments
  om <= (others => '0');
  om <= "";
  om <= "";
end scd;



-- Seed after: 12142139280560522810,13694093582652240945
