-- Seed: 14534688547945618350,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity ropv is
  port (z : in std_logic; zhsahng : linkage real);
end ropv;

architecture jbbivlo of ropv is
  
begin
  
end jbbivlo;

entity vy is
  port (gmsnvgh : inout real; qka : inout boolean);
end vy;

library ieee;
use ieee.std_logic_1164.all;

architecture qkeh of vy is
  signal gfrm : real;
  signal jphngmxqch : std_logic;
  signal rzpdavp : real;
  signal jdhm : std_logic;
begin
  kkywzjvv : entity work.ropv
    port map (z => jdhm, zhsahng => gmsnvgh);
  nnxiwqyvqi : entity work.ropv
    port map (z => jdhm, zhsahng => rzpdavp);
  nytr : entity work.ropv
    port map (z => jphngmxqch, zhsahng => gfrm);
  
  -- Single-driven assignments
  qka <= FALSE;
  
  -- Multi-driven assignments
  jphngmxqch <= jdhm;
end qkeh;



-- Seed after: 48408284198410418,5906004015519833893
