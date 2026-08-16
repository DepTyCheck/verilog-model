-- Seed: 1414193952584626724,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity psbpm is
  port (cistwomjb : inout std_logic; ru : out std_logic; s : inout bit_vector(0 downto 0); drjyrlllb : inout bit);
end psbpm;

architecture ndvx of psbpm is
  
begin
  -- Multi-driven assignments
  ru <= 'W';
  cistwomjb <= ru;
  cistwomjb <= ru;
end ndvx;

entity yaj is
  port (ke : linkage integer; xnzoyyy : out time; eyup : buffer integer; axmy : inout string(3 downto 1));
end yaj;

library ieee;
use ieee.std_logic_1164.all;

architecture gozjog of yaj is
  signal cipllife : bit;
  signal tg : bit_vector(0 downto 0);
  signal mu : bit;
  signal ktttb : bit_vector(0 downto 0);
  signal tdygmnkc : std_logic;
  signal zkdjwsb : bit;
  signal fefspjt : bit_vector(0 downto 0);
  signal rkfzpfls : std_logic;
begin
  ioux : entity work.psbpm
    port map (cistwomjb => rkfzpfls, ru => rkfzpfls, s => fefspjt, drjyrlllb => zkdjwsb);
  tvs : entity work.psbpm
    port map (cistwomjb => rkfzpfls, ru => tdygmnkc, s => ktttb, drjyrlllb => mu);
  ewqomsyrl : entity work.psbpm
    port map (cistwomjb => rkfzpfls, ru => rkfzpfls, s => tg, drjyrlllb => cipllife);
  
  -- Single-driven assignments
  xnzoyyy <= 8#1356.1_3# ms;
  
  -- Multi-driven assignments
  rkfzpfls <= 'L';
  rkfzpfls <= rkfzpfls;
end gozjog;



-- Seed after: 2616333151815523251,13857275728440271305
