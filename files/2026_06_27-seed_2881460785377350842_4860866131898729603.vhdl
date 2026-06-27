-- Seed: 2881460785377350842,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (sigrws : in std_logic_vector(2 downto 0));
end o;

architecture bqx of o is
  
begin
  
end bqx;

entity ihrefsv is
  port (pmwvkqwp : linkage boolean);
end ihrefsv;

library ieee;
use ieee.std_logic_1164.all;

architecture tkbuholkn of ihrefsv is
  signal remgncbrm : std_logic_vector(2 downto 0);
  signal wkarhvyyqk : std_logic_vector(2 downto 0);
begin
  lsdbewidi : entity work.o
    port map (sigrws => wkarhvyyqk);
  nx : entity work.o
    port map (sigrws => remgncbrm);
  
  -- Multi-driven assignments
  remgncbrm <= "UUU";
end tkbuholkn;



-- Seed after: 9037184323686951609,4860866131898729603
