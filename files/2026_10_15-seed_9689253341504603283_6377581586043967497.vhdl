-- Seed: 9689253341504603283,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity ipbfn is
  port (erxbppnw : inout std_logic_vector(1 downto 4));
end ipbfn;

architecture fy of ipbfn is
  
begin
  -- Multi-driven assignments
  erxbppnw <= erxbppnw;
  erxbppnw <= erxbppnw;
end fy;

library ieee;
use ieee.std_logic_1164.all;

entity jiexzj is
  port (zjyuui : in bit; gsbchokgma : inout bit; s : buffer std_logic);
end jiexzj;

library ieee;
use ieee.std_logic_1164.all;

architecture cti of jiexzj is
  signal byi : std_logic_vector(1 downto 4);
  signal uszwns : std_logic_vector(1 downto 4);
begin
  psfd : entity work.ipbfn
    port map (erxbppnw => uszwns);
  tgwhvwsjtk : entity work.ipbfn
    port map (erxbppnw => uszwns);
  gmqyf : entity work.ipbfn
    port map (erxbppnw => uszwns);
  rexw : entity work.ipbfn
    port map (erxbppnw => byi);
  
  -- Single-driven assignments
  gsbchokgma <= '0';
end cti;



-- Seed after: 803863683822699947,6377581586043967497
