-- Seed: 9886787808211399036,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity tmcxlwt is
  port (z : in std_logic_vector(1 downto 0));
end tmcxlwt;

architecture onmafita of tmcxlwt is
  
begin
  
end onmafita;

entity gtntsikty is
  port (vhrenqlvw : buffer real; qyxicfqbz : inout integer);
end gtntsikty;

library ieee;
use ieee.std_logic_1164.all;

architecture lhu of gtntsikty is
  signal suvmtmkso : std_logic_vector(1 downto 0);
  signal iouaf : std_logic_vector(1 downto 0);
begin
  irncmg : entity work.tmcxlwt
    port map (z => iouaf);
  jjwgipmfgk : entity work.tmcxlwt
    port map (z => suvmtmkso);
  
  -- Multi-driven assignments
  suvmtmkso <= iouaf;
end lhu;



-- Seed after: 8634784933513659912,6140041381800297705
