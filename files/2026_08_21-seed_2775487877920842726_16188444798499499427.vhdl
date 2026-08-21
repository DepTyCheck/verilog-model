-- Seed: 2775487877920842726,16188444798499499427

entity nldlgjd is
  port (uldsrc : buffer time);
end nldlgjd;

architecture dim of nldlgjd is
  
begin
  -- Single-driven assignments
  uldsrc <= uldsrc;
end dim;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (u : in real; vnz : in time; jimktkku : out std_logic_vector(1 downto 4));
end j;

architecture yqbbdi of j is
  signal lj : time;
  signal t : time;
  signal mhkcka : time;
begin
  mlcbxsgoz : entity work.nldlgjd
    port map (uldsrc => mhkcka);
  nmofawaqxk : entity work.nldlgjd
    port map (uldsrc => t);
  zhrn : entity work.nldlgjd
    port map (uldsrc => lj);
  
  -- Multi-driven assignments
  jimktkku <= (others => '0');
  jimktkku <= jimktkku;
  jimktkku <= jimktkku;
end yqbbdi;

entity dqfbjfu is
  port (dc : buffer integer_vector(0 to 4));
end dqfbjfu;

architecture sqj of dqfbjfu is
  
begin
  -- Single-driven assignments
  dc <= (2#1100#, 2#0#, 3_3_1_0, 4, 4_0_0);
end sqj;



-- Seed after: 4283424515431438395,16188444798499499427
