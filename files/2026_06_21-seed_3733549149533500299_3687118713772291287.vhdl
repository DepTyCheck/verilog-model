-- Seed: 3733549149533500299,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity ilkhd is
  port (vakv : out std_logic_vector(1 downto 4));
end ilkhd;

architecture upu of ilkhd is
  
begin
  -- Multi-driven assignments
  vakv <= "";
  vakv <= "";
  vakv <= (others => '0');
end upu;

library ieee;
use ieee.std_logic_1164.all;

entity vrsyw is
  port (es : inout real; chnuuzqa : out integer; khaxvjka : linkage std_logic);
end vrsyw;

library ieee;
use ieee.std_logic_1164.all;

architecture buxunc of vrsyw is
  signal i : std_logic_vector(1 downto 4);
begin
  zqvxi : entity work.ilkhd
    port map (vakv => i);
  fe : entity work.ilkhd
    port map (vakv => i);
  
  -- Single-driven assignments
  es <= 23424.3311;
  chnuuzqa <= 4_2_1_4;
  
  -- Multi-driven assignments
  i <= (others => '0');
end buxunc;

library ieee;
use ieee.std_logic_1164.all;

entity dggnxgcx is
  port (msvp : out std_logic; j : in time; vxzvoyeodo : out real);
end dggnxgcx;

architecture w of dggnxgcx is
  signal ytu : integer;
  signal tnmgwy : real;
begin
  m : entity work.vrsyw
    port map (es => tnmgwy, chnuuzqa => ytu, khaxvjka => msvp);
  
  -- Single-driven assignments
  vxzvoyeodo <= 0_4_4_1_4.4_3_3;
  
  -- Multi-driven assignments
  msvp <= 'X';
  msvp <= 'H';
end w;

library ieee;
use ieee.std_logic_1164.all;

entity txavvsg is
  port (nvmfy : buffer integer; tluyiybey : linkage std_logic; bj : linkage std_logic; nwf : out std_logic_vector(3 downto 4));
end txavvsg;

library ieee;
use ieee.std_logic_1164.all;

architecture wvunr of txavvsg is
  signal yjkoxqn : std_logic_vector(1 downto 4);
begin
  iohqqgfw : entity work.ilkhd
    port map (vakv => yjkoxqn);
end wvunr;



-- Seed after: 815925934292685119,3687118713772291287
