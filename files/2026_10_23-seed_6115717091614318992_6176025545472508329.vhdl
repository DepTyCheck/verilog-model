-- Seed: 6115717091614318992,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity hpnhxyclo is
  port (ypellddem : buffer std_logic_vector(4 to 2); iybtevvfc : linkage std_logic);
end hpnhxyclo;

architecture rqupkqid of hpnhxyclo is
  
begin
  
end rqupkqid;

library ieee;
use ieee.std_logic_1164.all;

entity eltnsdqb is
  port ( qwotecpezq : linkage boolean_vector(2 to 2)
  ; cwljj : buffer std_logic_vector(0 downto 2)
  ; bo : linkage real
  ; nqjxl : inout std_logic_vector(0 downto 4)
  );
end eltnsdqb;

library ieee;
use ieee.std_logic_1164.all;

architecture v of eltnsdqb is
  signal blmmn : std_logic;
  signal etvwsyjsk : std_logic_vector(4 to 2);
begin
  py : entity work.hpnhxyclo
    port map (ypellddem => etvwsyjsk, iybtevvfc => blmmn);
  qweaj : entity work.hpnhxyclo
    port map (ypellddem => nqjxl, iybtevvfc => blmmn);
  
  -- Multi-driven assignments
  nqjxl <= (others => '0');
  etvwsyjsk <= "";
  nqjxl <= (others => '0');
  blmmn <= '-';
end v;

library ieee;
use ieee.std_logic_1164.all;

entity ftfjklngg is
  port (wdjepbr : linkage std_logic_vector(3 to 2); gq : in std_logic; wjim : in std_logic);
end ftfjklngg;

library ieee;
use ieee.std_logic_1164.all;

architecture bpyrdkobj of ftfjklngg is
  signal dpchjj : std_logic;
  signal vimokim : std_logic_vector(4 to 2);
  signal o : real;
  signal mvosicr : boolean_vector(2 to 2);
  signal xuljvivpfr : std_logic_vector(4 to 2);
begin
  pqdfhergu : entity work.hpnhxyclo
    port map (ypellddem => xuljvivpfr, iybtevvfc => wjim);
  ojfdne : entity work.eltnsdqb
    port map (qwotecpezq => mvosicr, cwljj => xuljvivpfr, bo => o, nqjxl => xuljvivpfr);
  tm : entity work.hpnhxyclo
    port map (ypellddem => xuljvivpfr, iybtevvfc => wjim);
  lskqbzsbi : entity work.hpnhxyclo
    port map (ypellddem => vimokim, iybtevvfc => dpchjj);
end bpyrdkobj;



-- Seed after: 15920273341866378949,6176025545472508329
