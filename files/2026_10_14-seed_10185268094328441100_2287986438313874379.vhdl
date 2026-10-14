-- Seed: 10185268094328441100,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity fhditlc is
  port (iexhdtcb : out time; odsuyig : in time; tuvbujjruf : in std_logic);
end fhditlc;

architecture wwrpqjaj of fhditlc is
  
begin
  -- Single-driven assignments
  iexhdtcb <= 2#0000.1# ps;
end wwrpqjaj;

library ieee;
use ieee.std_logic_1164.all;

entity jtvp is
  port (ozjrr : in bit; lqtolcb : inout real; iwq : inout real; yousnlrjxy : in std_logic);
end jtvp;

architecture rzizzttqfb of jtvp is
  signal fj : time;
  signal fwt : time;
begin
  sikgyn : entity work.fhditlc
    port map (iexhdtcb => fwt, odsuyig => fj, tuvbujjruf => yousnlrjxy);
end rzizzttqfb;

library ieee;
use ieee.std_logic_1164.all;

entity bsggezynf is
  port (sob : inout integer; pgkmqwris : out real_vector(2 to 1); dlcs : inout std_logic_vector(3 downto 2));
end bsggezynf;

library ieee;
use ieee.std_logic_1164.all;

architecture oqfui of bsggezynf is
  signal tkgbdrwwt : std_logic;
  signal aw : real;
  signal rzitzqu : real;
  signal n : bit;
  signal urgqrzfu : std_logic;
  signal gao : time;
  signal wbcppmfcti : time;
begin
  hxamjvuzmc : entity work.fhditlc
    port map (iexhdtcb => wbcppmfcti, odsuyig => gao, tuvbujjruf => urgqrzfu);
  f : entity work.jtvp
    port map (ozjrr => n, lqtolcb => rzitzqu, iwq => aw, yousnlrjxy => tkgbdrwwt);
  
  -- Single-driven assignments
  pgkmqwris <= (others => 0.0);
  
  -- Multi-driven assignments
  dlcs <= "Z1";
end oqfui;



-- Seed after: 12942112575879567928,2287986438313874379
