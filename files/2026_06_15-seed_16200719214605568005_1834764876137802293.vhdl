-- Seed: 16200719214605568005,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity soomze is
  port (zomez : out severity_level; nfqqccyow : in integer; dmkveptvf : in std_logic_vector(2 downto 4));
end soomze;

architecture gnjwo of soomze is
  
begin
  
end gnjwo;

library ieee;
use ieee.std_logic_1164.all;

entity sepxp is
  port (ecrtvlafvc : out real_vector(3 to 2); zh : out std_logic; lnjrzeh : linkage std_logic; hatyiz : inout std_logic);
end sepxp;

library ieee;
use ieee.std_logic_1164.all;

architecture ddkfxwaaz of sepxp is
  signal bp : std_logic_vector(2 downto 4);
  signal l : integer;
  signal czljex : severity_level;
begin
  pfud : entity work.soomze
    port map (zomez => czljex, nfqqccyow => l, dmkveptvf => bp);
  
  -- Single-driven assignments
  ecrtvlafvc <= (others => 0.0);
  l <= 22;
  
  -- Multi-driven assignments
  hatyiz <= 'W';
  hatyiz <= 'U';
  zh <= '0';
end ddkfxwaaz;

library ieee;
use ieee.std_logic_1164.all;

entity fibkzqgtb is
  port (kenplke : inout bit; aeqfvlom : linkage boolean; q : in std_logic_vector(4 to 2); dm : in time_vector(2 downto 3));
end fibkzqgtb;

library ieee;
use ieee.std_logic_1164.all;

architecture okm of fibkzqgtb is
  signal kxau : std_logic_vector(2 downto 4);
  signal eymafq : integer;
  signal muvgpfo : severity_level;
begin
  kcw : entity work.soomze
    port map (zomez => muvgpfo, nfqqccyow => eymafq, dmkveptvf => kxau);
  
  -- Single-driven assignments
  kenplke <= '1';
  eymafq <= 3;
  
  -- Multi-driven assignments
  kxau <= "";
end okm;

library ieee;
use ieee.std_logic_1164.all;

entity eqidblby is
  port (ul : out bit; wrgc : linkage time; tuki : linkage std_logic);
end eqidblby;

library ieee;
use ieee.std_logic_1164.all;

architecture d of eqidblby is
  signal aqboo : integer;
  signal emfvc : severity_level;
  signal fj : std_logic;
  signal ifewnqnlt : std_logic;
  signal uag : real_vector(3 to 2);
  signal qsce : time_vector(2 downto 3);
  signal obnl : std_logic_vector(2 downto 4);
  signal wkl : boolean;
begin
  z : entity work.fibkzqgtb
    port map (kenplke => ul, aeqfvlom => wkl, q => obnl, dm => qsce);
  brbz : entity work.sepxp
    port map (ecrtvlafvc => uag, zh => ifewnqnlt, lnjrzeh => tuki, hatyiz => fj);
  vdtjqb : entity work.soomze
    port map (zomez => emfvc, nfqqccyow => aqboo, dmkveptvf => obnl);
  
  -- Single-driven assignments
  aqboo <= 2#1_1_0_0_1#;
  qsce <= (others => 0 ns);
  
  -- Multi-driven assignments
  obnl <= (others => '0');
  obnl <= "";
end d;



-- Seed after: 11312023910228084193,1834764876137802293
