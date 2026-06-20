-- Seed: 4956119581402544416,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity slkgueba is
  port (cftnwjcvvy : in real_vector(2 downto 0); izlidqlumd : out std_logic_vector(3 to 2));
end slkgueba;

architecture kzhlqjgpo of slkgueba is
  
begin
  -- Multi-driven assignments
  izlidqlumd <= "";
  izlidqlumd <= (others => '0');
end kzhlqjgpo;

entity hwq is
  port (f : in real; kdyfxsaqru : inout time);
end hwq;

library ieee;
use ieee.std_logic_1164.all;

architecture p of hwq is
  signal hmv : std_logic_vector(3 to 2);
  signal fg : real_vector(2 downto 0);
begin
  samtyackhr : entity work.slkgueba
    port map (cftnwjcvvy => fg, izlidqlumd => hmv);
  
  -- Single-driven assignments
  kdyfxsaqru <= 8#4655.2# us;
  
  -- Multi-driven assignments
  hmv <= (others => '0');
end p;



-- Seed after: 17339212963837025119,17924494779688682807
