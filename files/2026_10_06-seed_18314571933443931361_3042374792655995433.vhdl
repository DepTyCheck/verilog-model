-- Seed: 18314571933443931361,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity ztruwaeomv is
  port (jenvsbxttj : out boolean_vector(1 to 0); nylryei : inout std_logic_vector(4 to 2));
end ztruwaeomv;

architecture abmjbq of ztruwaeomv is
  
begin
  -- Multi-driven assignments
  nylryei <= nylryei;
  nylryei <= (others => '0');
  nylryei <= "";
  nylryei <= nylryei;
end abmjbq;

library ieee;
use ieee.std_logic_1164.all;

entity vrna is
  port (ka : in time; ivqihiaolg : inout std_logic_vector(2 downto 4); xqxiit : in real; qfdcsw : inout integer);
end vrna;

library ieee;
use ieee.std_logic_1164.all;

architecture arhmuybb of vrna is
  signal xfmll : std_logic_vector(4 to 2);
  signal qwuwhesz : boolean_vector(1 to 0);
begin
  ngcrplqri : entity work.ztruwaeomv
    port map (jenvsbxttj => qwuwhesz, nylryei => xfmll);
  
  -- Single-driven assignments
  qfdcsw <= qfdcsw;
  
  -- Multi-driven assignments
  ivqihiaolg <= ivqihiaolg;
  ivqihiaolg <= ivqihiaolg;
end arhmuybb;

library ieee;
use ieee.std_logic_1164.all;

entity vejiei is
  port (z : in std_logic; jqfbq : out character);
end vejiei;

library ieee;
use ieee.std_logic_1164.all;

architecture ofgrew of vejiei is
  signal n : integer;
  signal ygxqknx : std_logic_vector(2 downto 4);
  signal qrnela : time;
  signal lnvqzccio : integer;
  signal ubhmse : std_logic_vector(2 downto 4);
  signal fwxmoxk : time;
  signal pfeklumqs : integer;
  signal kovzkpkobg : real;
  signal frwkm : std_logic_vector(2 downto 4);
  signal ogt : time;
begin
  suicvfq : entity work.vrna
    port map (ka => ogt, ivqihiaolg => frwkm, xqxiit => kovzkpkobg, qfdcsw => pfeklumqs);
  ymbjmq : entity work.vrna
    port map (ka => fwxmoxk, ivqihiaolg => ubhmse, xqxiit => kovzkpkobg, qfdcsw => lnvqzccio);
  wwmm : entity work.vrna
    port map (ka => qrnela, ivqihiaolg => ygxqknx, xqxiit => kovzkpkobg, qfdcsw => n);
  
  -- Single-driven assignments
  jqfbq <= jqfbq;
  qrnela <= ogt;
  fwxmoxk <= ogt;
  kovzkpkobg <= kovzkpkobg;
  ogt <= 01 us;
  
  -- Multi-driven assignments
  ygxqknx <= frwkm;
end ofgrew;



-- Seed after: 11756300714873758698,3042374792655995433
