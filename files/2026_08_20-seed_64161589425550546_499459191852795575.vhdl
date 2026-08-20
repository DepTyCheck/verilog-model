-- Seed: 64161589425550546,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity yjw is
  port (rdmp : out std_logic; f : buffer time_vector(4 to 2));
end yjw;

architecture gorrq of yjw is
  
begin
  -- Single-driven assignments
  f <= f;
  
  -- Multi-driven assignments
  rdmp <= 'H';
  rdmp <= 'X';
  rdmp <= rdmp;
end gorrq;

entity kneza is
  port (zikb : linkage time);
end kneza;

library ieee;
use ieee.std_logic_1164.all;

architecture vqmitj of kneza is
  signal fbcizmw : time_vector(4 to 2);
  signal tplpjvd : time_vector(4 to 2);
  signal pdp : std_logic;
  signal iojmgh : time_vector(4 to 2);
  signal gatwzes : std_logic;
begin
  nfupdr : entity work.yjw
    port map (rdmp => gatwzes, f => iojmgh);
  bupb : entity work.yjw
    port map (rdmp => pdp, f => tplpjvd);
  qqy : entity work.yjw
    port map (rdmp => gatwzes, f => fbcizmw);
  
  -- Multi-driven assignments
  pdp <= 'L';
  pdp <= '1';
end vqmitj;

library ieee;
use ieee.std_logic_1164.all;

entity obaobxfu is
  port (rrqlasnwjm : buffer time; rqcaetb : out std_logic);
end obaobxfu;

library ieee;
use ieee.std_logic_1164.all;

architecture gmi of obaobxfu is
  signal kmwk : time_vector(4 to 2);
  signal jqumptn : std_logic;
  signal vlkqafbndq : time_vector(4 to 2);
  signal pjrko : std_logic;
begin
  syop : entity work.yjw
    port map (rdmp => pjrko, f => vlkqafbndq);
  dgbdynnb : entity work.yjw
    port map (rdmp => jqumptn, f => kmwk);
  
  -- Single-driven assignments
  rrqlasnwjm <= rrqlasnwjm;
  
  -- Multi-driven assignments
  jqumptn <= rqcaetb;
  jqumptn <= '-';
  jqumptn <= 'H';
  rqcaetb <= jqumptn;
end gmi;



-- Seed after: 10611608476076770039,499459191852795575
