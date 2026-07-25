-- Seed: 12407730694813215781,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity kje is
  port (ifsqruda : buffer std_logic_vector(3 downto 2); e : in integer);
end kje;

architecture f of kje is
  
begin
  -- Multi-driven assignments
  ifsqruda <= "LU";
  ifsqruda <= ifsqruda;
  ifsqruda <= "UU";
end f;

library ieee;
use ieee.std_logic_1164.all;

entity tc is
  port (v : in severity_level; bmwbt : inout std_logic; olkjptwlk : linkage time);
end tc;

library ieee;
use ieee.std_logic_1164.all;

architecture rngqh of tc is
  signal rqmxktq : integer;
  signal xjgzuq : std_logic_vector(3 downto 2);
  signal viy : std_logic_vector(3 downto 2);
  signal jfctuzhlt : integer;
  signal tr : std_logic_vector(3 downto 2);
  signal wltdquepxa : integer;
  signal eufxulp : std_logic_vector(3 downto 2);
begin
  ifa : entity work.kje
    port map (ifsqruda => eufxulp, e => wltdquepxa);
  co : entity work.kje
    port map (ifsqruda => tr, e => jfctuzhlt);
  pbr : entity work.kje
    port map (ifsqruda => viy, e => wltdquepxa);
  noggzw : entity work.kje
    port map (ifsqruda => xjgzuq, e => rqmxktq);
  
  -- Single-driven assignments
  wltdquepxa <= 16#9#;
  rqmxktq <= 2#1101#;
  jfctuzhlt <= 3223;
end rngqh;



-- Seed after: 10523167822548521960,5306691039457971049
