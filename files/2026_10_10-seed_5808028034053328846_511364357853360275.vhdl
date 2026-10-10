-- Seed: 5808028034053328846,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity ilvx is
  port (hr : out std_logic_vector(3 downto 0); wsffm : buffer time; zlryjjwpe : out std_logic);
end ilvx;

architecture krowtgqvn of ilvx is
  
begin
  -- Single-driven assignments
  wsffm <= wsffm;
  
  -- Multi-driven assignments
  zlryjjwpe <= zlryjjwpe;
  zlryjjwpe <= zlryjjwpe;
end krowtgqvn;

library ieee;
use ieee.std_logic_1164.all;

entity vh is
  port (xoow : buffer std_logic; purwbop : in string(3 to 3); qu : in time; je : inout real_vector(0 to 1));
end vh;

library ieee;
use ieee.std_logic_1164.all;

architecture xig of vh is
  signal dcov : time;
  signal oey : time;
  signal elg : std_logic_vector(3 downto 0);
begin
  l : entity work.ilvx
    port map (hr => elg, wsffm => oey, zlryjjwpe => xoow);
  exu : entity work.ilvx
    port map (hr => elg, wsffm => dcov, zlryjjwpe => xoow);
  
  -- Multi-driven assignments
  xoow <= '-';
end xig;



-- Seed after: 14220588266696842447,511364357853360275
