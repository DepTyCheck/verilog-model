-- Seed: 6327512031513912446,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (erlw : buffer severity_level; olmulpdicr : out bit_vector(3 downto 3); cxa : linkage std_logic_vector(3 downto 3); evhiwqeouy : in std_logic);
end l;

architecture bg of l is
  
begin
  -- Single-driven assignments
  olmulpdicr <= (others => '0');
  erlw <= ERROR;
end bg;

entity p is
  port (azgnkgeic : inout real);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture yjrvedwmnn of p is
  signal spzp : std_logic;
  signal ugbwoin : std_logic_vector(3 downto 3);
  signal macknvyi : bit_vector(3 downto 3);
  signal htniffan : severity_level;
  signal zthpjim : std_logic_vector(3 downto 3);
  signal j : bit_vector(3 downto 3);
  signal mengsnlglc : severity_level;
  signal wlq : std_logic;
  signal kpfv : std_logic_vector(3 downto 3);
  signal gll : bit_vector(3 downto 3);
  signal o : severity_level;
  signal s : std_logic;
  signal vpbns : std_logic_vector(3 downto 3);
  signal hprbk : bit_vector(3 downto 3);
  signal f : severity_level;
begin
  rchnz : entity work.l
    port map (erlw => f, olmulpdicr => hprbk, cxa => vpbns, evhiwqeouy => s);
  blhvstaxvn : entity work.l
    port map (erlw => o, olmulpdicr => gll, cxa => kpfv, evhiwqeouy => wlq);
  daumbanb : entity work.l
    port map (erlw => mengsnlglc, olmulpdicr => j, cxa => zthpjim, evhiwqeouy => wlq);
  rlzomnc : entity work.l
    port map (erlw => htniffan, olmulpdicr => macknvyi, cxa => ugbwoin, evhiwqeouy => spzp);
  
  -- Multi-driven assignments
  vpbns <= "-";
  vpbns <= "H";
end yjrvedwmnn;

library ieee;
use ieee.std_logic_1164.all;

entity nqwbmksp is
  port (vai : buffer std_logic_vector(1 to 1); ztb : buffer string(2 to 3));
end nqwbmksp;

library ieee;
use ieee.std_logic_1164.all;

architecture jmu of nqwbmksp is
  signal ebvxl : std_logic;
  signal wxyoc : bit_vector(3 downto 3);
  signal zlcfszt : severity_level;
  signal cnfjriqmh : real;
begin
  tryqq : entity work.p
    port map (azgnkgeic => cnfjriqmh);
  mazwwtiu : entity work.l
    port map (erlw => zlcfszt, olmulpdicr => wxyoc, cxa => vai, evhiwqeouy => ebvxl);
  
  -- Multi-driven assignments
  vai <= (others => 'W');
  vai <= (others => '1');
  vai <= (others => '1');
  vai <= (others => 'L');
end jmu;



-- Seed after: 2227541059789793311,6882842853887419669
