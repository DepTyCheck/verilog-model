-- Seed: 5565439962032453587,9900838946149614537

entity cikc is
  port (gcfto : in time; wkpbn : buffer string(3 to 4));
end cikc;

architecture nb of cikc is
  
begin
  
end nb;

library ieee;
use ieee.std_logic_1164.all;

entity ejiwdxiunu is
  port (iimsmkn : in std_logic; gun : linkage boolean_vector(1 to 2));
end ejiwdxiunu;

architecture fntjfownu of ejiwdxiunu is
  signal duoxeovls : string(3 to 4);
  signal dmagij : string(3 to 4);
  signal fubbjzgvcd : time;
begin
  xqg : entity work.cikc
    port map (gcfto => fubbjzgvcd, wkpbn => dmagij);
  bxifn : entity work.cikc
    port map (gcfto => fubbjzgvcd, wkpbn => duoxeovls);
  
  -- Single-driven assignments
  fubbjzgvcd <= 16#3E.8_F_3_C# ps;
end fntjfownu;

library ieee;
use ieee.std_logic_1164.all;

entity qn is
  port (vrdf : inout time; wtiyohdeuk : out integer_vector(1 to 1); xaawlolor : linkage std_logic; giiwpojct : inout std_logic);
end qn;

architecture k of qn is
  signal djtvwidbo : string(3 to 4);
  signal yrnfoswwk : string(3 to 4);
  signal ndmom : boolean_vector(1 to 2);
  signal j : string(3 to 4);
begin
  dqtilv : entity work.cikc
    port map (gcfto => vrdf, wkpbn => j);
  fyw : entity work.ejiwdxiunu
    port map (iimsmkn => giiwpojct, gun => ndmom);
  pkly : entity work.cikc
    port map (gcfto => vrdf, wkpbn => yrnfoswwk);
  bypzqrmjyk : entity work.cikc
    port map (gcfto => vrdf, wkpbn => djtvwidbo);
  
  -- Single-driven assignments
  wtiyohdeuk <= (others => 8#1_4_6_7#);
  vrdf <= vrdf;
  
  -- Multi-driven assignments
  giiwpojct <= giiwpojct;
end k;



-- Seed after: 13218272526430659066,9900838946149614537
