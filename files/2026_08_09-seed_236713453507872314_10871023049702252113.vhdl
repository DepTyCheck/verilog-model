-- Seed: 236713453507872314,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity jwuzp is
  port (pqio : linkage std_logic; ongzl : buffer time; u : out std_logic_vector(0 to 3));
end jwuzp;

architecture qbzmdko of jwuzp is
  
begin
  -- Single-driven assignments
  ongzl <= ongzl;
end qbzmdko;

library ieee;
use ieee.std_logic_1164.all;

entity gm is
  port ( adizmuzopo : out boolean_vector(0 downto 4)
  ; taxxsvyj : buffer bit_vector(4 downto 0)
  ; yns : linkage std_logic_vector(2 downto 0)
  ; pehaqhyfl : inout std_logic_vector(3 to 1)
  );
end gm;

library ieee;
use ieee.std_logic_1164.all;

architecture l of gm is
  signal ldmrgzkzu : std_logic_vector(0 to 3);
  signal wego : time;
  signal vrv : std_logic;
  signal ihzw : std_logic_vector(0 to 3);
  signal sz : time;
  signal rgh : std_logic;
begin
  wlrjhk : entity work.jwuzp
    port map (pqio => rgh, ongzl => sz, u => ihzw);
  azlmisazcf : entity work.jwuzp
    port map (pqio => vrv, ongzl => wego, u => ldmrgzkzu);
  
  -- Single-driven assignments
  taxxsvyj <= taxxsvyj;
  adizmuzopo <= (others => TRUE);
  
  -- Multi-driven assignments
  vrv <= rgh;
  pehaqhyfl <= pehaqhyfl;
end l;



-- Seed after: 14864349147434338471,10871023049702252113
