-- Seed: 14771988759309207952,8437298063418820479

entity hj is
  port (oyzoezbkwa : out bit_vector(2 to 4));
end hj;

architecture s of hj is
  
begin
  
end s;

entity kpxebnu is
  port (a : buffer time);
end kpxebnu;

architecture q of kpxebnu is
  signal qcyxawfw : bit_vector(2 to 4);
begin
  cuhjnyw : entity work.hj
    port map (oyzoezbkwa => qcyxawfw);
  
  -- Single-driven assignments
  a <= 16#B_D# fs;
end q;

library ieee;
use ieee.std_logic_1164.all;

entity ahyli is
  port (qydcocixdw : inout std_logic);
end ahyli;

architecture vkywuhkre of ahyli is
  signal xvrpq : bit_vector(2 to 4);
  signal nqsjrrzl : bit_vector(2 to 4);
  signal h : bit_vector(2 to 4);
begin
  xc : entity work.hj
    port map (oyzoezbkwa => h);
  oq : entity work.hj
    port map (oyzoezbkwa => nqsjrrzl);
  zosnjkg : entity work.hj
    port map (oyzoezbkwa => xvrpq);
  
  -- Multi-driven assignments
  qydcocixdw <= qydcocixdw;
  qydcocixdw <= qydcocixdw;
end vkywuhkre;

library ieee;
use ieee.std_logic_1164.all;

entity szphjjc is
  port (qydjrtf : in std_logic_vector(3 to 2); wfaa : out std_logic_vector(0 downto 2); japvmi : in std_logic);
end szphjjc;

architecture daz of szphjjc is
  signal fqhyquouaw : bit_vector(2 to 4);
  signal hhbyog : time;
begin
  wfegodoqa : entity work.kpxebnu
    port map (a => hhbyog);
  xthtzobtpy : entity work.hj
    port map (oyzoezbkwa => fqhyquouaw);
end daz;



-- Seed after: 10417435842110234276,8437298063418820479
