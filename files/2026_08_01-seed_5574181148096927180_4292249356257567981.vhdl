-- Seed: 5574181148096927180,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity epseipbiot is
  port (qyj : in std_logic_vector(4 to 2); zgvnzysrt : buffer real; xkbivf : buffer bit_vector(4 to 3); wa : out time);
end epseipbiot;

architecture rmnly of epseipbiot is
  
begin
  -- Single-driven assignments
  wa <= 4010.22 ms;
  zgvnzysrt <= zgvnzysrt;
  xkbivf <= (others => '0');
end rmnly;

entity xmuktuyh is
  port (ikoozvz : out integer; q : in integer; hhvrff : linkage time);
end xmuktuyh;

library ieee;
use ieee.std_logic_1164.all;

architecture zv of xmuktuyh is
  signal riz : time;
  signal ocmrpqdifj : bit_vector(4 to 3);
  signal tjvmqyh : real;
  signal fbg : std_logic_vector(4 to 2);
begin
  kzelcltvg : entity work.epseipbiot
    port map (qyj => fbg, zgvnzysrt => tjvmqyh, xkbivf => ocmrpqdifj, wa => riz);
  
  -- Single-driven assignments
  ikoozvz <= q;
  
  -- Multi-driven assignments
  fbg <= fbg;
  fbg <= (others => '0');
end zv;

entity ydxhrya is
  port (zbduzjnil : buffer integer; ep : buffer real);
end ydxhrya;

library ieee;
use ieee.std_logic_1164.all;

architecture ojkt of ydxhrya is
  signal oloubzfq : time;
  signal aaiqqewm : time;
  signal c : bit_vector(4 to 3);
  signal xc : std_logic_vector(4 to 2);
  signal hhwuga : time;
  signal oxmtrmzemr : integer;
  signal vgdkblvvt : time;
  signal hpbpngukca : bit_vector(4 to 3);
  signal bjr : real;
  signal f : std_logic_vector(4 to 2);
begin
  o : entity work.epseipbiot
    port map (qyj => f, zgvnzysrt => bjr, xkbivf => hpbpngukca, wa => vgdkblvvt);
  lgqkv : entity work.xmuktuyh
    port map (ikoozvz => oxmtrmzemr, q => zbduzjnil, hhvrff => hhwuga);
  xcug : entity work.epseipbiot
    port map (qyj => xc, zgvnzysrt => ep, xkbivf => c, wa => aaiqqewm);
  nouyx : entity work.xmuktuyh
    port map (ikoozvz => zbduzjnil, q => zbduzjnil, hhvrff => oloubzfq);
  
  -- Multi-driven assignments
  xc <= (others => '0');
  xc <= xc;
end ojkt;



-- Seed after: 13569706626226264059,4292249356257567981
