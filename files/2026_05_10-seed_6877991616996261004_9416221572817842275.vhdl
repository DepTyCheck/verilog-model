-- Seed: 6877991616996261004,9416221572817842275

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (pmtxmml : linkage std_logic);
end l;



architecture p of l is
  
begin
  
end p;

library ieee;
use ieee.std_logic_1164.all;

entity gfmt is
  port (br : buffer std_logic);
end gfmt;

library ieee;
use ieee.std_logic_1164.all;

architecture lyk of gfmt is
  signal pwltwfs : std_logic;
begin
  nwk : entity work.l
    port map (pmtxmml => pwltwfs);
  jxw : entity work.l
    port map (pmtxmml => pwltwfs);
  czbm : entity work.l
    port map (pmtxmml => br);
  x : entity work.l
    port map (pmtxmml => br);
end lyk;



-- Seed after: 9634992911691409932,9416221572817842275
