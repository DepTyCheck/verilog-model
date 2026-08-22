-- Seed: 5524025581341167954,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity uzoh is
  port (fijudzg : buffer std_logic_vector(3 to 1); aqbaqmo : in time_vector(2 downto 0));
end uzoh;

architecture p of uzoh is
  
begin
  -- Multi-driven assignments
  fijudzg <= (others => '0');
end p;

library ieee;
use ieee.std_logic_1164.all;

entity elqxx is
  port (rgjtgag : linkage std_logic_vector(4 to 1); itrd : linkage std_logic; cafs : out real);
end elqxx;

library ieee;
use ieee.std_logic_1164.all;

architecture oryng of elqxx is
  signal elktguf : time_vector(2 downto 0);
  signal tnkiw : std_logic_vector(3 to 1);
begin
  i : entity work.uzoh
    port map (fijudzg => tnkiw, aqbaqmo => elktguf);
  
  -- Single-driven assignments
  cafs <= cafs;
  elktguf <= (4 ns, 14100 ns, 8#53116# ns);
  
  -- Multi-driven assignments
  tnkiw <= "";
  tnkiw <= "";
  tnkiw <= tnkiw;
end oryng;



-- Seed after: 696645570638811087,5805648483995786113
