-- Seed: 10073567307531556226,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity acggzynf is
  port (jlduzxrcz : in std_logic; ykqalbkvvh : buffer std_logic_vector(3 downto 3));
end acggzynf;

architecture ojafbda of acggzynf is
  
begin
  -- Multi-driven assignments
  ykqalbkvvh <= "L";
  ykqalbkvvh <= (others => 'X');
  ykqalbkvvh <= (others => 'L');
end ojafbda;

library ieee;
use ieee.std_logic_1164.all;

entity qdyynwcht is
  port (id : inout real; fpwrdgspn : buffer std_logic);
end qdyynwcht;

library ieee;
use ieee.std_logic_1164.all;

architecture zgc of qdyynwcht is
  signal gp : std_logic_vector(3 downto 3);
  signal qelkmvqv : std_logic;
  signal ulybs : std_logic_vector(3 downto 3);
  signal wtuvuzd : std_logic;
begin
  dhqjpgkrp : entity work.acggzynf
    port map (jlduzxrcz => wtuvuzd, ykqalbkvvh => ulybs);
  cheyhmh : entity work.acggzynf
    port map (jlduzxrcz => qelkmvqv, ykqalbkvvh => gp);
  u : entity work.acggzynf
    port map (jlduzxrcz => fpwrdgspn, ykqalbkvvh => ulybs);
  
  -- Single-driven assignments
  id <= 101.22;
  
  -- Multi-driven assignments
  ulybs <= (others => 'Z');
  fpwrdgspn <= 'H';
  fpwrdgspn <= 'U';
end zgc;

entity ctv is
  port (yh : buffer severity_level; bezidq : buffer integer);
end ctv;

architecture eyga of ctv is
  
begin
  -- Single-driven assignments
  bezidq <= 8#1164#;
  yh <= WARNING;
end eyga;



-- Seed after: 17289110511077969047,13694093582652240945
