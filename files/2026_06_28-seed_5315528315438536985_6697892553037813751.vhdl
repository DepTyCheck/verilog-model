-- Seed: 5315528315438536985,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity jl is
  port (lz : inout std_logic);
end jl;

architecture geuum of jl is
  
begin
  
end geuum;

library ieee;
use ieee.std_logic_1164.all;

entity ify is
  port (b : out real; jxdispt : buffer real; puqoadscr : in std_logic_vector(1 downto 3); qgeas : inout std_logic);
end ify;

architecture ml of ify is
  
begin
  zdsakclsev : entity work.jl
    port map (lz => qgeas);
  vco : entity work.jl
    port map (lz => qgeas);
  rc : entity work.jl
    port map (lz => qgeas);
  
  -- Single-driven assignments
  jxdispt <= 16#77D7.68E#;
end ml;



-- Seed after: 2821333047059723215,6697892553037813751
