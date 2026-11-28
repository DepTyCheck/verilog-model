-- Seed: 10214142302445802158,15306146742274979323

entity v is
  port (spbju : linkage bit);
end v;

architecture wp of v is
  
begin
  
end wp;

entity osna is
  port (qzl : in real; x : linkage boolean_vector(2 to 1));
end osna;

architecture sofha of osna is
  signal bmkm : bit;
begin
  pruojlxqm : entity work.v
    port map (spbju => bmkm);
end sofha;

library ieee;
use ieee.std_logic_1164.all;

entity sxe is
  port (ipitfumlfr : inout std_logic_vector(0 downto 2); yatyh : in std_logic);
end sxe;

architecture fhqyjrq of sxe is
  signal tpq : bit;
begin
  qfnarsnyty : entity work.v
    port map (spbju => tpq);
  
  -- Multi-driven assignments
  ipitfumlfr <= ipitfumlfr;
  ipitfumlfr <= "";
end fhqyjrq;

entity kznb is
  port (pnpj : in character);
end kznb;

architecture ypmzsspyw of kznb is
  signal zvn : boolean_vector(2 to 1);
  signal o : real;
begin
  kravalgo : entity work.osna
    port map (qzl => o, x => zvn);
  
  -- Single-driven assignments
  o <= 8#4_5_5_6_2.3_4#;
end ypmzsspyw;



-- Seed after: 9256351277558870212,15306146742274979323
