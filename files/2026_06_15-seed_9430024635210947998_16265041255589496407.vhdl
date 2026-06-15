-- Seed: 9430024635210947998,16265041255589496407

library ieee;
use ieee.std_logic_1164.all;

entity eleny is
  port (qpobah : in bit_vector(2 to 2); zm : linkage boolean; ge : inout std_logic; aofoppnyj : linkage std_logic_vector(0 downto 2));
end eleny;



architecture uprox of eleny is
  
begin
  
end uprox;

library ieee;
use ieee.std_logic_1164.all;

entity nhlynvbgm is
  port (iglm : out std_logic; xwhnb : buffer time; kbeyl : in std_logic);
end nhlynvbgm;

library ieee;
use ieee.std_logic_1164.all;

architecture tlj of nhlynvbgm is
  signal yncgb : std_logic_vector(0 downto 2);
  signal jbsa : std_logic;
  signal epwvl : boolean;
  signal wnwbtw : bit_vector(2 to 2);
  signal itp : std_logic_vector(0 downto 2);
  signal jxf : std_logic;
  signal f : boolean;
  signal qumvuzpmoz : bit_vector(2 to 2);
  signal ygkval : std_logic_vector(0 downto 2);
  signal akhca : boolean;
  signal tfutvajl : bit_vector(2 to 2);
begin
  r : entity work.eleny
    port map (qpobah => tfutvajl, zm => akhca, ge => iglm, aofoppnyj => ygkval);
  ctvg : entity work.eleny
    port map (qpobah => qumvuzpmoz, zm => f, ge => jxf, aofoppnyj => itp);
  ezukbl : entity work.eleny
    port map (qpobah => wnwbtw, zm => epwvl, ge => jbsa, aofoppnyj => yncgb);
end tlj;



entity y is
  port (oxxux : buffer boolean_vector(3 to 3); trv : linkage character; wqthsz : linkage time);
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture c of y is
  signal qgozrpn : std_logic;
  signal sijcgvumeb : time;
  signal tspwdv : std_logic;
  signal m : std_logic_vector(0 downto 2);
  signal imccvom : std_logic;
  signal b : boolean;
  signal xlguhymrwc : bit_vector(2 to 2);
begin
  kqugo : entity work.eleny
    port map (qpobah => xlguhymrwc, zm => b, ge => imccvom, aofoppnyj => m);
  nobwatu : entity work.nhlynvbgm
    port map (iglm => tspwdv, xwhnb => sijcgvumeb, kbeyl => qgozrpn);
  ylrj : entity work.eleny
    port map (qpobah => xlguhymrwc, zm => b, ge => tspwdv, aofoppnyj => m);
end c;



-- Seed after: 8198580242489666282,16265041255589496407
