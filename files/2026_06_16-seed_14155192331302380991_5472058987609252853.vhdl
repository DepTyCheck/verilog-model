-- Seed: 14155192331302380991,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity pqbrueds is
  port (sv : linkage std_logic_vector(4 downto 3); nyirzdtzhn : in boolean_vector(3 to 2); abydhupbpf : inout integer);
end pqbrueds;

architecture nkudgzwqq of pqbrueds is
  
begin
  -- Single-driven assignments
  abydhupbpf <= 2_2_3_0;
end nkudgzwqq;

entity fqdsolz is
  port (ttxxdpkvl : linkage severity_level);
end fqdsolz;

library ieee;
use ieee.std_logic_1164.all;

architecture rmkss of fqdsolz is
  signal txzsqsuqoa : integer;
  signal yn : boolean_vector(3 to 2);
  signal akqzhu : std_logic_vector(4 downto 3);
begin
  cotlvjw : entity work.pqbrueds
    port map (sv => akqzhu, nyirzdtzhn => yn, abydhupbpf => txzsqsuqoa);
  
  -- Single-driven assignments
  yn <= (others => TRUE);
  
  -- Multi-driven assignments
  akqzhu <= ('H', '0');
  akqzhu <= "LZ";
  akqzhu <= "X1";
end rmkss;



-- Seed after: 18129422940474189745,5472058987609252853
