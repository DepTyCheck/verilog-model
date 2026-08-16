-- Seed: 3385286586239695269,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity wrtcuy is
  port (qbtcgfly : in integer_vector(3 downto 1); xkcjv : out std_logic_vector(3 to 4); bamq : buffer real; fkppoaz : out integer);
end wrtcuy;

architecture lsd of wrtcuy is
  
begin
  -- Single-driven assignments
  fkppoaz <= 3_1_1;
  bamq <= bamq;
  
  -- Multi-driven assignments
  xkcjv <= ('H', '0');
end lsd;

entity vqmdm is
  port (j : in time; pf : buffer severity_level);
end vqmdm;

library ieee;
use ieee.std_logic_1164.all;

architecture x of vqmdm is
  signal fjcm : integer;
  signal bxssc : real;
  signal jmamry : std_logic_vector(3 to 4);
  signal qxyvncnxd : integer_vector(3 downto 1);
  signal yqayrsj : integer;
  signal itgoywg : real;
  signal ftomi : std_logic_vector(3 to 4);
  signal ub : integer_vector(3 downto 1);
begin
  fcaqeoayeu : entity work.wrtcuy
    port map (qbtcgfly => ub, xkcjv => ftomi, bamq => itgoywg, fkppoaz => yqayrsj);
  ooun : entity work.wrtcuy
    port map (qbtcgfly => qxyvncnxd, xkcjv => jmamry, bamq => bxssc, fkppoaz => fjcm);
  
  -- Multi-driven assignments
  ftomi <= ('1', '-');
  jmamry <= ('-', '1');
  ftomi <= ('X', 'U');
  ftomi <= "-X";
end x;



-- Seed after: 17435316223337029976,13857275728440271305
