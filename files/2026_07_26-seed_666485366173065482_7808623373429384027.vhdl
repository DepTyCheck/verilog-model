-- Seed: 666485366173065482,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (h : inout string(1 downto 2); xrvokblp : in std_logic_vector(0 downto 2));
end t;

architecture iqcy of t is
  
begin
  -- Single-driven assignments
  h <= h;
end iqcy;

library ieee;
use ieee.std_logic_1164.all;

entity zi is
  port (cno : inout std_logic_vector(2 to 1); e : buffer std_logic; um : buffer severity_level; gtzlk : in std_logic);
end zi;

library ieee;
use ieee.std_logic_1164.all;

architecture hdkdgoy of zi is
  signal gcdxzgoa : string(1 downto 2);
  signal kbicfddtq : string(1 downto 2);
  signal xbwr : std_logic_vector(0 downto 2);
  signal sediuc : string(1 downto 2);
begin
  awsbjwz : entity work.t
    port map (h => sediuc, xrvokblp => xbwr);
  ymt : entity work.t
    port map (h => kbicfddtq, xrvokblp => xbwr);
  qcpgmnl : entity work.t
    port map (h => gcdxzgoa, xrvokblp => cno);
  
  -- Single-driven assignments
  um <= ERROR;
end hdkdgoy;



-- Seed after: 16659268062154949958,7808623373429384027
