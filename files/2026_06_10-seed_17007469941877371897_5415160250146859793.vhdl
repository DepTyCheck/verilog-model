-- Seed: 17007469941877371897,5415160250146859793



entity fvbfgnm is
  port (bkzin : linkage boolean_vector(2 to 0));
end fvbfgnm;



architecture z of fvbfgnm is
  
begin
  
end z;

library ieee;
use ieee.std_logic_1164.all;

entity fatyzho is
  port (wttxqu : inout std_logic_vector(0 to 3); zxllfdln : linkage character; dvmtuarnj : out boolean_vector(0 downto 3));
end fatyzho;



architecture qfdtpz of fatyzho is
  signal pxhir : boolean_vector(2 to 0);
begin
  tceetnjny : entity work.fvbfgnm
    port map (bkzin => pxhir);
  paohdl : entity work.fvbfgnm
    port map (bkzin => dvmtuarnj);
  jrdpz : entity work.fvbfgnm
    port map (bkzin => pxhir);
  cjguwockqy : entity work.fvbfgnm
    port map (bkzin => dvmtuarnj);
end qfdtpz;

library ieee;
use ieee.std_logic_1164.all;

entity ik is
  port (uqmpea : linkage time; bzdvs : linkage std_logic_vector(3 downto 2); mcjknqdhc : in std_logic);
end ik;

library ieee;
use ieee.std_logic_1164.all;

architecture gh of ik is
  signal xlei : boolean_vector(0 downto 3);
  signal tm : character;
  signal jutczvs : std_logic_vector(0 to 3);
  signal lqa : boolean_vector(2 to 0);
begin
  e : entity work.fvbfgnm
    port map (bkzin => lqa);
  ybqczsog : entity work.fvbfgnm
    port map (bkzin => lqa);
  gvcxbrksq : entity work.fatyzho
    port map (wttxqu => jutczvs, zxllfdln => tm, dvmtuarnj => xlei);
  b : entity work.fvbfgnm
    port map (bkzin => lqa);
end gh;



entity wywhf is
  port (wbpck : buffer boolean);
end wywhf;

library ieee;
use ieee.std_logic_1164.all;

architecture aqx of wywhf is
  signal xdg : std_logic;
  signal oyfb : std_logic_vector(3 downto 2);
  signal colgnttce : time;
  signal jrtvhbhbjf : boolean_vector(0 downto 3);
  signal wtgjkw : character;
  signal sgtgp : std_logic_vector(0 to 3);
begin
  tuwanjyp : entity work.fatyzho
    port map (wttxqu => sgtgp, zxllfdln => wtgjkw, dvmtuarnj => jrtvhbhbjf);
  lmt : entity work.ik
    port map (uqmpea => colgnttce, bzdvs => oyfb, mcjknqdhc => xdg);
end aqx;



-- Seed after: 16200954228884989874,5415160250146859793
