-- Seed: 12853764198813983496,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity uzj is
  port (lwmvnpyra : buffer std_logic_vector(3 downto 3));
end uzj;

architecture qdzggqfc of uzj is
  
begin
  -- Multi-driven assignments
  lwmvnpyra <= lwmvnpyra;
  lwmvnpyra <= "0";
  lwmvnpyra <= lwmvnpyra;
  lwmvnpyra <= (others => 'W');
end qdzggqfc;

entity kgfu is
  port (rgngqmyv : inout bit; xtmflapd : in integer);
end kgfu;

library ieee;
use ieee.std_logic_1164.all;

architecture jzpbwe of kgfu is
  signal vps : std_logic_vector(3 downto 3);
  signal ipedwal : std_logic_vector(3 downto 3);
begin
  uapku : entity work.uzj
    port map (lwmvnpyra => ipedwal);
  mbktqhixrt : entity work.uzj
    port map (lwmvnpyra => vps);
  
  -- Single-driven assignments
  rgngqmyv <= '0';
end jzpbwe;



-- Seed after: 8788257683477852641,13843488114570579517
