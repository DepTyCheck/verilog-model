-- Seed: 7344467398477317230,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity ozinzjm is
  port (tscanh : inout std_logic; p : inout boolean; x : in integer);
end ozinzjm;

architecture wpelps of ozinzjm is
  
begin
  -- Single-driven assignments
  p <= FALSE;
  
  -- Multi-driven assignments
  tscanh <= 'W';
end wpelps;

entity crf is
  port (dammsgli : in integer_vector(3 downto 3));
end crf;

library ieee;
use ieee.std_logic_1164.all;

architecture y of crf is
  signal kn : boolean;
  signal fjete : boolean;
  signal zl : integer;
  signal qfnvl : boolean;
  signal solr : std_logic;
begin
  lqsm : entity work.ozinzjm
    port map (tscanh => solr, p => qfnvl, x => zl);
  agbogs : entity work.ozinzjm
    port map (tscanh => solr, p => fjete, x => zl);
  zly : entity work.ozinzjm
    port map (tscanh => solr, p => kn, x => zl);
  
  -- Single-driven assignments
  zl <= 1_3_4_1_3;
  
  -- Multi-driven assignments
  solr <= 'H';
  solr <= '-';
end y;



-- Seed after: 741973144985632710,10557070023141912087
