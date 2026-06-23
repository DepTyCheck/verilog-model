-- Seed: 1909388271978576067,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity xtd is
  port (ltjjcytcn : out std_logic; ftumg : linkage real; w : in bit_vector(4 downto 3); bwffrwhsmv : in severity_level);
end xtd;

architecture wrzj of xtd is
  
begin
  -- Multi-driven assignments
  ltjjcytcn <= 'H';
  ltjjcytcn <= 'H';
end wrzj;

library ieee;
use ieee.std_logic_1164.all;

entity axgv is
  port (kxfdp : inout time; udmlwwx : inout severity_level; orjfi : linkage std_logic_vector(4 downto 3));
end axgv;

library ieee;
use ieee.std_logic_1164.all;

architecture atncow of axgv is
  signal stdklezr : severity_level;
  signal sqwoh : real;
  signal egbx : real;
  signal sz : std_logic;
  signal dohs : severity_level;
  signal tuqdxfikep : bit_vector(4 downto 3);
  signal tyi : real;
  signal jstic : bit_vector(4 downto 3);
  signal kcig : real;
  signal jlkfgoatq : std_logic;
begin
  wqlhcg : entity work.xtd
    port map (ltjjcytcn => jlkfgoatq, ftumg => kcig, w => jstic, bwffrwhsmv => udmlwwx);
  vtvgja : entity work.xtd
    port map (ltjjcytcn => jlkfgoatq, ftumg => tyi, w => tuqdxfikep, bwffrwhsmv => dohs);
  rfc : entity work.xtd
    port map (ltjjcytcn => sz, ftumg => egbx, w => jstic, bwffrwhsmv => udmlwwx);
  ji : entity work.xtd
    port map (ltjjcytcn => jlkfgoatq, ftumg => sqwoh, w => jstic, bwffrwhsmv => stdklezr);
  
  -- Single-driven assignments
  udmlwwx <= ERROR;
  stdklezr <= NOTE;
  dohs <= ERROR;
  kxfdp <= 2#1_1.1# ps;
  
  -- Multi-driven assignments
  jlkfgoatq <= 'W';
  jlkfgoatq <= 'H';
  jlkfgoatq <= 'L';
  sz <= '1';
end atncow;



-- Seed after: 17206760156936237715,8421704836678237495
