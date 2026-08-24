-- Seed: 13131546956405463824,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity vg is
  port (pft : out boolean_vector(0 downto 0); lj : buffer std_logic; pygsj : in time_vector(0 downto 2));
end vg;

architecture gru of vg is
  
begin
  -- Single-driven assignments
  pft <= (others => FALSE);
  
  -- Multi-driven assignments
  lj <= lj;
  lj <= 'H';
  lj <= 'L';
  lj <= 'H';
end gru;



-- Seed after: 7068573705194517829,16159265764638711791
