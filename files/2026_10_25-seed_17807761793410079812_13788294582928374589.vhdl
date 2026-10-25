-- Seed: 17807761793410079812,13788294582928374589

entity mwteie is
  port (pifz : buffer real; dxvbdfz : buffer integer; w : inout bit; v : linkage integer);
end mwteie;

architecture dgika of mwteie is
  
begin
  
end dgika;

library ieee;
use ieee.std_logic_1164.all;

entity dtaj is
  port (qpszkadze : in std_logic_vector(4 downto 1));
end dtaj;

architecture wjyolbag of dtaj is
  signal tosmqjai : integer;
  signal dzozfr : bit;
  signal h : integer;
  signal ze : real;
  signal hpljki : integer;
  signal lld : bit;
  signal yvcfwl : integer;
  signal tvbfnnersv : real;
begin
  xtelv : entity work.mwteie
    port map (pifz => tvbfnnersv, dxvbdfz => yvcfwl, w => lld, v => hpljki);
  ow : entity work.mwteie
    port map (pifz => ze, dxvbdfz => h, w => dzozfr, v => tosmqjai);
end wjyolbag;

library ieee;
use ieee.std_logic_1164.all;

entity kbolsk is
  port (nvmtkr : buffer bit_vector(4 downto 4); y : linkage std_logic);
end kbolsk;

library ieee;
use ieee.std_logic_1164.all;

architecture zeva of kbolsk is
  signal h : std_logic_vector(4 downto 1);
  signal bfp : integer;
  signal bbjlc : bit;
  signal tebtqvvxxe : integer;
  signal x : real;
begin
  beblaxzl : entity work.mwteie
    port map (pifz => x, dxvbdfz => tebtqvvxxe, w => bbjlc, v => bfp);
  pdkoxbiot : entity work.dtaj
    port map (qpszkadze => h);
  
  -- Single-driven assignments
  nvmtkr <= (others => '0');
  
  -- Multi-driven assignments
  h <= h;
  h <= h;
  h <= h;
  h <= ('0', 'X', 'Z', '0');
end zeva;



-- Seed after: 8636886638901028457,13788294582928374589
