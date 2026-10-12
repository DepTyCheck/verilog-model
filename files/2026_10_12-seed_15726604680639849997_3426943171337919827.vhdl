-- Seed: 15726604680639849997,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity tcutgxr is
  port (aashfxbn : buffer std_logic; ciry : in bit);
end tcutgxr;

architecture evgcpxrud of tcutgxr is
  
begin
  -- Multi-driven assignments
  aashfxbn <= 'U';
  aashfxbn <= aashfxbn;
end evgcpxrud;

entity wcbwkonx is
  port (tburtktuc : out time);
end wcbwkonx;

library ieee;
use ieee.std_logic_1164.all;

architecture mfwoqteqc of wcbwkonx is
  signal ok : bit;
  signal ttjizbnyvz : bit;
  signal mjnqz : bit;
  signal tck : std_logic;
begin
  vmgpbxp : entity work.tcutgxr
    port map (aashfxbn => tck, ciry => mjnqz);
  thr : entity work.tcutgxr
    port map (aashfxbn => tck, ciry => ttjizbnyvz);
  zjyitnyjs : entity work.tcutgxr
    port map (aashfxbn => tck, ciry => mjnqz);
  zuks : entity work.tcutgxr
    port map (aashfxbn => tck, ciry => ok);
  
  -- Single-driven assignments
  tburtktuc <= tburtktuc;
  ok <= '0';
  ttjizbnyvz <= '0';
  mjnqz <= '1';
  
  -- Multi-driven assignments
  tck <= '1';
end mfwoqteqc;

entity pfic is
  port (fhfofc : inout boolean);
end pfic;

library ieee;
use ieee.std_logic_1164.all;

architecture gq of pfic is
  signal zwifkmylxz : bit;
  signal hhbcpkw : std_logic;
  signal qwxrnjpua : time;
begin
  d : entity work.wcbwkonx
    port map (tburtktuc => qwxrnjpua);
  fgtlmb : entity work.tcutgxr
    port map (aashfxbn => hhbcpkw, ciry => zwifkmylxz);
  
  -- Single-driven assignments
  zwifkmylxz <= zwifkmylxz;
  fhfofc <= TRUE;
end gq;



-- Seed after: 9676877581006746378,3426943171337919827
