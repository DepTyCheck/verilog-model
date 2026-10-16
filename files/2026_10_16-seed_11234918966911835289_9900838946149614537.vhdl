-- Seed: 11234918966911835289,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity vut is
  port (ohwlsowv : out boolean_vector(0 downto 1); rasjhgt : buffer std_logic_vector(1 to 0); bwmt : linkage bit_vector(4 to 0));
end vut;

architecture j of vut is
  
begin
  -- Multi-driven assignments
  rasjhgt <= rasjhgt;
  rasjhgt <= "";
end j;

entity ui is
  port (abbytfl : linkage time; akxlrdul : out boolean);
end ui;

library ieee;
use ieee.std_logic_1164.all;

architecture fqewl of ui is
  signal oc : bit_vector(4 to 0);
  signal vwf : boolean_vector(0 downto 1);
  signal wpgczdjcq : bit_vector(4 to 0);
  signal xh : std_logic_vector(1 to 0);
  signal mproj : boolean_vector(0 downto 1);
begin
  lfahzve : entity work.vut
    port map (ohwlsowv => mproj, rasjhgt => xh, bwmt => wpgczdjcq);
  chzfjnknub : entity work.vut
    port map (ohwlsowv => vwf, rasjhgt => xh, bwmt => oc);
  
  -- Single-driven assignments
  akxlrdul <= FALSE;
  
  -- Multi-driven assignments
  xh <= (others => '0');
  xh <= xh;
  xh <= xh;
end fqewl;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (cbt : linkage integer; ndxxcc : out std_logic_vector(3 downto 2));
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture vniqsj of r is
  signal ip : bit_vector(4 to 0);
  signal fj : boolean_vector(0 downto 1);
  signal hkqjtj : bit_vector(4 to 0);
  signal quszks : boolean_vector(0 downto 1);
  signal oqsy : bit_vector(4 to 0);
  signal omix : std_logic_vector(1 to 0);
  signal plan : boolean_vector(0 downto 1);
  signal bg : boolean;
  signal nevjond : time;
begin
  l : entity work.ui
    port map (abbytfl => nevjond, akxlrdul => bg);
  vrixthgsbe : entity work.vut
    port map (ohwlsowv => plan, rasjhgt => omix, bwmt => oqsy);
  psvec : entity work.vut
    port map (ohwlsowv => quszks, rasjhgt => omix, bwmt => hkqjtj);
  ahxnybq : entity work.vut
    port map (ohwlsowv => fj, rasjhgt => omix, bwmt => ip);
end vniqsj;

entity dripkhwp is
  port (asjtmgqxhv : in time);
end dripkhwp;

library ieee;
use ieee.std_logic_1164.all;

architecture ycjmtfe of dripkhwp is
  signal tgods : bit_vector(4 to 0);
  signal jdqknuufs : boolean_vector(0 downto 1);
  signal iqckiy : bit_vector(4 to 0);
  signal g : std_logic_vector(1 to 0);
  signal l : boolean_vector(0 downto 1);
  signal grpeeuy : std_logic_vector(3 downto 2);
  signal cbe : integer;
begin
  zgdfvt : entity work.r
    port map (cbt => cbe, ndxxcc => grpeeuy);
  tgyfdjk : entity work.vut
    port map (ohwlsowv => l, rasjhgt => g, bwmt => iqckiy);
  nyg : entity work.vut
    port map (ohwlsowv => jdqknuufs, rasjhgt => g, bwmt => tgods);
  
  -- Multi-driven assignments
  grpeeuy <= grpeeuy;
  grpeeuy <= grpeeuy;
  grpeeuy <= ('W', 'X');
end ycjmtfe;



-- Seed after: 400695370188497860,9900838946149614537
