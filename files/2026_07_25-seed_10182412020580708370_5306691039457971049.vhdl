-- Seed: 10182412020580708370,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity lwfuf is
  port (lytyhf : inout std_logic; ueatnmbqm : linkage integer_vector(2 to 1));
end lwfuf;

architecture hduzwbee of lwfuf is
  
begin
  
end hduzwbee;

entity p is
  port (ksopf : in integer);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture qmibwzs of p is
  signal kct : integer_vector(2 to 1);
  signal gxc : integer_vector(2 to 1);
  signal yvurdn : std_logic;
begin
  vhstue : entity work.lwfuf
    port map (lytyhf => yvurdn, ueatnmbqm => gxc);
  mcvxioql : entity work.lwfuf
    port map (lytyhf => yvurdn, ueatnmbqm => kct);
  
  -- Multi-driven assignments
  yvurdn <= 'Z';
end qmibwzs;

library ieee;
use ieee.std_logic_1164.all;

entity dlitcd is
  port (metp : linkage std_logic; oyzkw : out integer_vector(1 downto 2));
end dlitcd;

architecture pmd of dlitcd is
  
begin
  -- Single-driven assignments
  oyzkw <= oyzkw;
end pmd;



-- Seed after: 4196520760009790286,5306691039457971049
