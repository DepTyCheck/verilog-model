-- Seed: 16582792171608015354,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity yaswp is
  port (z : inout integer; noz : inout std_logic_vector(0 to 1));
end yaswp;

architecture lnik of yaswp is
  
begin
  -- Single-driven assignments
  z <= 16#6_B_E_2_3#;
  
  -- Multi-driven assignments
  noz <= "WW";
  noz <= ('-', 'W');
  noz <= "0X";
end lnik;

library ieee;
use ieee.std_logic_1164.all;

entity zubk is
  port (qzpdvtzz : linkage std_logic; cjvoxtswzy : in real);
end zubk;

library ieee;
use ieee.std_logic_1164.all;

architecture fsitaalv of zubk is
  signal tn : integer;
  signal jjrgj : std_logic_vector(0 to 1);
  signal r : integer;
  signal bvatfcwor : integer;
  signal fngjt : std_logic_vector(0 to 1);
  signal sbh : integer;
begin
  jvzlp : entity work.yaswp
    port map (z => sbh, noz => fngjt);
  mpvlpe : entity work.yaswp
    port map (z => bvatfcwor, noz => fngjt);
  qfy : entity work.yaswp
    port map (z => r, noz => jjrgj);
  krdb : entity work.yaswp
    port map (z => tn, noz => fngjt);
  
  -- Multi-driven assignments
  fngjt <= "XZ";
  fngjt <= ('W', '1');
end fsitaalv;



-- Seed after: 13601749062315262033,8421704836678237495
