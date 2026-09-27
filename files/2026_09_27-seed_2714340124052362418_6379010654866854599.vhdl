-- Seed: 2714340124052362418,6379010654866854599

entity sm is
  port (aq : in time);
end sm;

architecture hm of sm is
  
begin
  
end hm;

library ieee;
use ieee.std_logic_1164.all;

entity qkm is
  port (gcxrhrwt : buffer time; lscqpijmkp : linkage boolean; rwnim : in std_logic_vector(4 downto 1));
end qkm;

architecture fp of qkm is
  
begin
  -- Single-driven assignments
  gcxrhrwt <= 2#000.0101# ns;
end fp;

library ieee;
use ieee.std_logic_1164.all;

entity qzxhdf is
  port (rxts : inout std_logic; wveaiy : inout time; fln : inout bit);
end qzxhdf;

library ieee;
use ieee.std_logic_1164.all;

architecture t of qzxhdf is
  signal vb : std_logic_vector(4 downto 1);
  signal sqslqtejsv : boolean;
  signal uauisavnmk : time;
begin
  pspcabgo : entity work.sm
    port map (aq => wveaiy);
  mmca : entity work.qkm
    port map (gcxrhrwt => uauisavnmk, lscqpijmkp => sqslqtejsv, rwnim => vb);
  hvlg : entity work.sm
    port map (aq => wveaiy);
  ngxrzsix : entity work.sm
    port map (aq => wveaiy);
  
  -- Single-driven assignments
  fln <= '1';
  wveaiy <= 202.41433 ps;
  
  -- Multi-driven assignments
  rxts <= '1';
end t;



-- Seed after: 16481344517287395957,6379010654866854599
