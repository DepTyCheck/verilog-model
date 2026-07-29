-- Seed: 9019146060492923592,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity mgrmzklh is
  port (zpix : in time_vector(2 to 0); j : inout severity_level; hliy : out std_logic; avpkl : out integer);
end mgrmzklh;

architecture xep of mgrmzklh is
  
begin
  -- Single-driven assignments
  avpkl <= 2#1_1#;
  j <= FAILURE;
  
  -- Multi-driven assignments
  hliy <= hliy;
  hliy <= hliy;
  hliy <= hliy;
  hliy <= hliy;
end xep;

entity yqa is
  port (qtv : inout time; otlsv : buffer integer_vector(1 downto 3));
end yqa;

library ieee;
use ieee.std_logic_1164.all;

architecture jreloljv of yqa is
  signal uby : integer;
  signal whgcdwyocq : severity_level;
  signal wpkwr : integer;
  signal phrfkhe : std_logic;
  signal owzfqlcbxu : severity_level;
  signal locw : time_vector(2 to 0);
begin
  aez : entity work.mgrmzklh
    port map (zpix => locw, j => owzfqlcbxu, hliy => phrfkhe, avpkl => wpkwr);
  fvmhlb : entity work.mgrmzklh
    port map (zpix => locw, j => whgcdwyocq, hliy => phrfkhe, avpkl => uby);
  
  -- Single-driven assignments
  qtv <= 4 sec;
  locw <= (others => 0 ns);
  otlsv <= (others => 0);
  
  -- Multi-driven assignments
  phrfkhe <= 'H';
  phrfkhe <= 'W';
end jreloljv;



-- Seed after: 13885799880107637005,14641901754878719179
