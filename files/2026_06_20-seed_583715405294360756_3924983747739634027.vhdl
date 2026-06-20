-- Seed: 583715405294360756,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity cfmzhwllyk is
  port (rhbomyjszb : out std_logic_vector(2 to 2); ixsayclq : linkage std_logic);
end cfmzhwllyk;

architecture talypddqcd of cfmzhwllyk is
  
begin
  -- Multi-driven assignments
  rhbomyjszb <= "H";
  rhbomyjszb <= (others => 'Z');
  rhbomyjszb <= (others => '1');
  rhbomyjszb <= "U";
end talypddqcd;

library ieee;
use ieee.std_logic_1164.all;

entity uambpwoe is
  port (dewrjak : inout severity_level; sogaypggp : out std_logic; sgehrvzg : linkage string(3 downto 5));
end uambpwoe;

library ieee;
use ieee.std_logic_1164.all;

architecture ocrkctnf of uambpwoe is
  signal idkn : std_logic;
  signal okendt : std_logic_vector(2 to 2);
  signal mtpf : std_logic;
  signal spgvx : std_logic_vector(2 to 2);
begin
  zbpn : entity work.cfmzhwllyk
    port map (rhbomyjszb => spgvx, ixsayclq => mtpf);
  otloh : entity work.cfmzhwllyk
    port map (rhbomyjszb => okendt, ixsayclq => idkn);
  wrbc : entity work.cfmzhwllyk
    port map (rhbomyjszb => spgvx, ixsayclq => sogaypggp);
  aqvmlsyjk : entity work.cfmzhwllyk
    port map (rhbomyjszb => spgvx, ixsayclq => sogaypggp);
  
  -- Single-driven assignments
  dewrjak <= WARNING;
  
  -- Multi-driven assignments
  okendt <= (others => 'L');
  mtpf <= 'X';
  sogaypggp <= '1';
end ocrkctnf;

library ieee;
use ieee.std_logic_1164.all;

entity kcktwqu is
  port (el : out std_logic_vector(3 to 0); ktiqi : buffer bit_vector(4 downto 3); c : in std_logic);
end kcktwqu;

library ieee;
use ieee.std_logic_1164.all;

architecture dm of kcktwqu is
  signal awquxagokq : std_logic_vector(2 to 2);
begin
  nxhhruswdv : entity work.cfmzhwllyk
    port map (rhbomyjszb => awquxagokq, ixsayclq => c);
  
  -- Single-driven assignments
  ktiqi <= ('0', '1');
  
  -- Multi-driven assignments
  el <= "";
  el <= (others => '0');
end dm;



-- Seed after: 12702110815806060139,3924983747739634027
