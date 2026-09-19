-- Seed: 6069242732169123986,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity twsalm is
  port (pe : out time; gpci : buffer time; bc : in std_logic_vector(2 downto 3));
end twsalm;

architecture fykjyhdly of twsalm is
  
begin
  -- Single-driven assignments
  gpci <= 8#244.361# ps;
  pe <= pe;
end fykjyhdly;

library ieee;
use ieee.std_logic_1164.all;

entity okfphk is
  port (ncyhcb : in bit_vector(4 to 0); jjerujlwtp : buffer std_logic; ihpx : buffer boolean; j : in std_logic_vector(0 downto 3));
end okfphk;

library ieee;
use ieee.std_logic_1164.all;

architecture zzzmhkhav of okfphk is
  signal llsq : std_logic_vector(2 downto 3);
  signal pzslkyxux : time;
  signal gtfzew : time;
  signal pb : time;
  signal jxkb : time;
  signal pjblepjisd : time;
  signal vhiaz : time;
  signal qkqr : time;
  signal uswdyftyv : time;
begin
  yibf : entity work.twsalm
    port map (pe => uswdyftyv, gpci => qkqr, bc => j);
  eaussi : entity work.twsalm
    port map (pe => vhiaz, gpci => pjblepjisd, bc => j);
  ywpfohf : entity work.twsalm
    port map (pe => jxkb, gpci => pb, bc => j);
  ourqur : entity work.twsalm
    port map (pe => gtfzew, gpci => pzslkyxux, bc => llsq);
  
  -- Single-driven assignments
  ihpx <= TRUE;
  
  -- Multi-driven assignments
  jjerujlwtp <= 'Z';
  llsq <= llsq;
end zzzmhkhav;

library ieee;
use ieee.std_logic_1164.all;

entity iulgbrze is
  port (dyidqff : buffer severity_level; tjorxc : in std_logic);
end iulgbrze;

library ieee;
use ieee.std_logic_1164.all;

architecture gfvb of iulgbrze is
  signal ihfrz : time;
  signal fzucky : time;
  signal urt : std_logic_vector(2 downto 3);
  signal ltf : time;
  signal izzkxrv : time;
begin
  snrnyldbwy : entity work.twsalm
    port map (pe => izzkxrv, gpci => ltf, bc => urt);
  pevt : entity work.twsalm
    port map (pe => fzucky, gpci => ihfrz, bc => urt);
  
  -- Single-driven assignments
  dyidqff <= ERROR;
  
  -- Multi-driven assignments
  urt <= "";
end gfvb;



-- Seed after: 5882435983559778307,14141408946471626091
