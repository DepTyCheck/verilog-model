-- Seed: 17752324861568607471,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity zpvfsaxaau is
  port (smutwby : buffer std_logic_vector(2 downto 4); vbl : linkage std_logic_vector(1 to 0));
end zpvfsaxaau;

architecture qffy of zpvfsaxaau is
  
begin
  -- Multi-driven assignments
  smutwby <= (others => '0');
end qffy;

library ieee;
use ieee.std_logic_1164.all;

entity mvmqmb is
  port (blrswyd : linkage time; czizdhcd : buffer std_logic_vector(0 downto 4); r : buffer integer);
end mvmqmb;

library ieee;
use ieee.std_logic_1164.all;

architecture asxy of mvmqmb is
  signal szaqynidxi : std_logic_vector(1 to 0);
  signal elgvpoiw : std_logic_vector(2 downto 4);
begin
  dj : entity work.zpvfsaxaau
    port map (smutwby => czizdhcd, vbl => elgvpoiw);
  o : entity work.zpvfsaxaau
    port map (smutwby => elgvpoiw, vbl => szaqynidxi);
  
  -- Single-driven assignments
  r <= r;
  
  -- Multi-driven assignments
  szaqynidxi <= czizdhcd;
  elgvpoiw <= czizdhcd;
  czizdhcd <= "";
  elgvpoiw <= "";
end asxy;



-- Seed after: 14352565245737164657,17219977430298397867
