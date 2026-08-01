-- Seed: 16372876797278662370,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity jnci is
  port (jpwspdt : linkage std_logic; xsjnjahdp : buffer real);
end jnci;

architecture fw of jnci is
  
begin
  -- Single-driven assignments
  xsjnjahdp <= 0_1_3_1_3.3;
end fw;

library ieee;
use ieee.std_logic_1164.all;

entity ltzb is
  port (izhqvcun : out bit_vector(1 to 3); w : linkage std_logic);
end ltzb;

architecture uxfauinf of ltzb is
  signal ytwupf : real;
begin
  ob : entity work.jnci
    port map (jpwspdt => w, xsjnjahdp => ytwupf);
  
  -- Single-driven assignments
  izhqvcun <= izhqvcun;
end uxfauinf;

entity ttaqycf is
  port (cunde : linkage time; wdy : in time);
end ttaqycf;

library ieee;
use ieee.std_logic_1164.all;

architecture h of ttaqycf is
  signal wmy : bit_vector(1 to 3);
  signal pzbxslx : real;
  signal jmnr : real;
  signal dymev : std_logic;
begin
  yvogctdnws : entity work.jnci
    port map (jpwspdt => dymev, xsjnjahdp => jmnr);
  rouailh : entity work.jnci
    port map (jpwspdt => dymev, xsjnjahdp => pzbxslx);
  sazdc : entity work.ltzb
    port map (izhqvcun => wmy, w => dymev);
  
  -- Multi-driven assignments
  dymev <= 'L';
end h;



-- Seed after: 15979780223855220111,4292249356257567981
