-- Seed: 1151982258792801422,13479070923501788437

entity eulduaocg is
  port (bgihqg : out real);
end eulduaocg;

architecture wiuxpn of eulduaocg is
  
begin
  -- Single-driven assignments
  bgihqg <= 2#11.1_1#;
end wiuxpn;

library ieee;
use ieee.std_logic_1164.all;

entity mzdg is
  port (btmujmoc : out std_logic; qdwnanu : linkage integer; xu : out severity_level);
end mzdg;

architecture so of mzdg is
  
begin
  -- Single-driven assignments
  xu <= FAILURE;
end so;

entity qdcjezyrr is
  port (datuldmja : buffer real; naqkrx : inout bit_vector(1 downto 1));
end qdcjezyrr;

library ieee;
use ieee.std_logic_1164.all;

architecture eqt of qdcjezyrr is
  signal cmqoryect : real;
  signal ewcs : severity_level;
  signal qwmde : integer;
  signal i : severity_level;
  signal uh : integer;
  signal rv : std_logic;
  signal epsfjuwq : real;
begin
  pqcutdrky : entity work.eulduaocg
    port map (bgihqg => epsfjuwq);
  buiw : entity work.mzdg
    port map (btmujmoc => rv, qdwnanu => uh, xu => i);
  dka : entity work.mzdg
    port map (btmujmoc => rv, qdwnanu => qwmde, xu => ewcs);
  z : entity work.eulduaocg
    port map (bgihqg => cmqoryect);
  
  -- Single-driven assignments
  naqkrx <= (others => '1');
  datuldmja <= 4_0.2_4_3_4;
  
  -- Multi-driven assignments
  rv <= 'H';
  rv <= 'L';
end eqt;



-- Seed after: 12476514266097806101,13479070923501788437
