-- Seed: 7367574153654817273,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity huqen is
  port (ruaxb : inout std_logic; w : linkage std_logic);
end huqen;

architecture q of huqen is
  
begin
  
end q;

library ieee;
use ieee.std_logic_1164.all;

entity vgjx is
  port (durvdug : linkage std_logic; ul : buffer std_logic; txorjyvyc : in bit; pcksjbkq : out std_logic);
end vgjx;

library ieee;
use ieee.std_logic_1164.all;

architecture p of vgjx is
  signal abtk : std_logic;
  signal qj : std_logic;
begin
  noconsv : entity work.huqen
    port map (ruaxb => qj, w => abtk);
  
  -- Multi-driven assignments
  qj <= '0';
end p;

library ieee;
use ieee.std_logic_1164.all;

entity eolde is
  port (siaprjy : in integer; wdz : linkage character; ltkiyc : linkage std_logic);
end eolde;

library ieee;
use ieee.std_logic_1164.all;

architecture bozimx of eolde is
  signal io : std_logic;
  signal saktf : std_logic;
  signal myfite : bit;
  signal tknxgh : std_logic;
  signal x : std_logic;
begin
  qgtmgr : entity work.huqen
    port map (ruaxb => x, w => x);
  xafm : entity work.huqen
    port map (ruaxb => x, w => tknxgh);
  dv : entity work.vgjx
    port map (durvdug => ltkiyc, ul => x, txorjyvyc => myfite, pcksjbkq => saktf);
  ihenwvskfx : entity work.huqen
    port map (ruaxb => io, w => ltkiyc);
  
  -- Single-driven assignments
  myfite <= '1';
end bozimx;



-- Seed after: 17045087510129150698,5805648483995786113
