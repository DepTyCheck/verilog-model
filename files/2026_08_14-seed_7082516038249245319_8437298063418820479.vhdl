-- Seed: 7082516038249245319,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity hudiwscqu is
  port (pmbgtdi : linkage std_logic; haasczuomu : buffer std_logic; y : linkage time; chapkora : inout time);
end hudiwscqu;

architecture vqqahxdi of hudiwscqu is
  
begin
  -- Single-driven assignments
  chapkora <= 3_1_0_0_2 fs;
  
  -- Multi-driven assignments
  haasczuomu <= 'Z';
  haasczuomu <= 'W';
end vqqahxdi;

entity sr is
  port (gukjt : buffer real; m : linkage bit);
end sr;

library ieee;
use ieee.std_logic_1164.all;

architecture glzjcwu of sr is
  signal klhrboa : time;
  signal lyj : time;
  signal xtkfizv : time;
  signal klib : time;
  signal hazqwdaci : time;
  signal sqfv : time;
  signal wnonpfcvcm : time;
  signal lpntzm : time;
  signal iqio : std_logic;
begin
  ivtuzgzb : entity work.hudiwscqu
    port map (pmbgtdi => iqio, haasczuomu => iqio, y => lpntzm, chapkora => wnonpfcvcm);
  mhvqbehqy : entity work.hudiwscqu
    port map (pmbgtdi => iqio, haasczuomu => iqio, y => sqfv, chapkora => hazqwdaci);
  ott : entity work.hudiwscqu
    port map (pmbgtdi => iqio, haasczuomu => iqio, y => klib, chapkora => xtkfizv);
  snjsjokazx : entity work.hudiwscqu
    port map (pmbgtdi => iqio, haasczuomu => iqio, y => lyj, chapkora => klhrboa);
  
  -- Single-driven assignments
  gukjt <= gukjt;
  
  -- Multi-driven assignments
  iqio <= 'L';
  iqio <= 'W';
end glzjcwu;

entity dqpw is
  port (tkn : buffer integer_vector(1 downto 2); wimwyrb : inout time; gtxwoifvhy : buffer time);
end dqpw;

architecture c of dqpw is
  
begin
  -- Single-driven assignments
  gtxwoifvhy <= 33 fs;
  tkn <= (others => 0);
  wimwyrb <= 8#0532.4_0_0_1_7# fs;
end c;



-- Seed after: 8943428433823471,8437298063418820479
