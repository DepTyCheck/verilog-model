-- Seed: 7701540097017035191,16159265764638711791

entity jywklo is
  port (qvvho : inout boolean_vector(4 downto 4));
end jywklo;

architecture nekwz of jywklo is
  
begin
  -- Single-driven assignments
  qvvho <= (others => TRUE);
end nekwz;

library ieee;
use ieee.std_logic_1164.all;

entity celiz is
  port ( lvzltqf : out std_logic_vector(3 downto 0)
  ; nnmjxt : in time
  ; pt : in std_logic_vector(1 downto 3)
  ; lhfk : linkage std_logic_vector(4 downto 1)
  );
end celiz;

architecture gimx of celiz is
  signal ynlwt : boolean_vector(4 downto 4);
  signal dtdhuozj : boolean_vector(4 downto 4);
begin
  nqjzxgibbc : entity work.jywklo
    port map (qvvho => dtdhuozj);
  nwoknsh : entity work.jywklo
    port map (qvvho => ynlwt);
  
  -- Multi-driven assignments
  lvzltqf <= ('0', 'H', 'Z', 'X');
end gimx;



-- Seed after: 6177512581738816445,16159265764638711791
