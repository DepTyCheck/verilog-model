-- Seed: 4787693605078476775,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity wpfkdiao is
  port (stltwn : inout std_logic; d : inout real; cepzkoqtn : buffer real_vector(3 to 1); ykrsfn : buffer time_vector(3 downto 3));
end wpfkdiao;

architecture i of wpfkdiao is
  
begin
  -- Single-driven assignments
  ykrsfn <= (others => 8#235# ns);
  cepzkoqtn <= (others => 0.0);
  
  -- Multi-driven assignments
  stltwn <= 'X';
  stltwn <= 'Z';
  stltwn <= 'U';
end i;



-- Seed after: 9163833002666386390,17047277710231705797
