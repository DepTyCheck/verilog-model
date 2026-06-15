-- Seed: 230732866091478155,16265041255589496407

library ieee;
use ieee.std_logic_1164.all;

entity gczjjj is
  port (wbhttoae : linkage std_logic; hlucybr : buffer integer; zohgpfzv : in character);
end gczjjj;



architecture srcgxis of gczjjj is
  
begin
  
end srcgxis;

library ieee;
use ieee.std_logic_1164.all;

entity fizyt is
  port (nk : inout bit; itspg : linkage std_logic_vector(1 to 3));
end fizyt;

library ieee;
use ieee.std_logic_1164.all;

architecture asrlblaj of fizyt is
  signal yithd : character;
  signal cwigrshbnz : integer;
  signal jpsdxi : std_logic;
begin
  eepormj : entity work.gczjjj
    port map (wbhttoae => jpsdxi, hlucybr => cwigrshbnz, zohgpfzv => yithd);
end asrlblaj;



entity cvgyerttc is
  port (vsar : inout time; txdrvjzyvw : buffer time);
end cvgyerttc;

library ieee;
use ieee.std_logic_1164.all;

architecture kqombzvgm of cvgyerttc is
  signal gazwhuaq : std_logic_vector(1 to 3);
  signal r : bit;
begin
  kc : entity work.fizyt
    port map (nk => r, itspg => gazwhuaq);
end kqombzvgm;



entity wnsg is
  port (bpla : out bit; bmju : inout real; xwcwun : out time_vector(0 downto 0));
end wnsg;



architecture qfebuaaujn of wnsg is
  signal bssiar : time;
  signal xfqhrwz : time;
begin
  vzlpkmaj : entity work.cvgyerttc
    port map (vsar => xfqhrwz, txdrvjzyvw => bssiar);
end qfebuaaujn;



-- Seed after: 14362434367483492523,16265041255589496407
