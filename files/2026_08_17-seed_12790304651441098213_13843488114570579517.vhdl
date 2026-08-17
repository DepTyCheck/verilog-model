-- Seed: 12790304651441098213,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity tyrdpghf is
  port (kekom : in string(1 to 3); dhcitaildw : in boolean; qpcuipi : out std_logic);
end tyrdpghf;

architecture hxwvhidpmp of tyrdpghf is
  
begin
  -- Multi-driven assignments
  qpcuipi <= 'Z';
  qpcuipi <= qpcuipi;
  qpcuipi <= qpcuipi;
end hxwvhidpmp;

library ieee;
use ieee.std_logic_1164.all;

entity gnhgsbt is
  port (rxqalqz : out std_logic; lf : inout bit; efayowftjw : in integer);
end gnhgsbt;

library ieee;
use ieee.std_logic_1164.all;

architecture nldfcu of gnhgsbt is
  signal llb : std_logic;
  signal mzkpuqa : boolean;
  signal uie : string(1 to 3);
  signal hv : string(1 to 3);
  signal ia : std_logic;
  signal ahfpbzm : boolean;
  signal uu : string(1 to 3);
begin
  gpfdyn : entity work.tyrdpghf
    port map (kekom => uu, dhcitaildw => ahfpbzm, qpcuipi => ia);
  rzbp : entity work.tyrdpghf
    port map (kekom => hv, dhcitaildw => ahfpbzm, qpcuipi => rxqalqz);
  huk : entity work.tyrdpghf
    port map (kekom => uie, dhcitaildw => mzkpuqa, qpcuipi => llb);
  atnbb : entity work.tyrdpghf
    port map (kekom => uu, dhcitaildw => ahfpbzm, qpcuipi => llb);
  
  -- Single-driven assignments
  lf <= '1';
  hv <= uu;
  
  -- Multi-driven assignments
  rxqalqz <= 'U';
  rxqalqz <= rxqalqz;
  rxqalqz <= rxqalqz;
end nldfcu;



-- Seed after: 18233017838110110260,13843488114570579517
