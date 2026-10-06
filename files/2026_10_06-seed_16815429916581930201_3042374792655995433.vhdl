-- Seed: 16815429916581930201,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity dffhywcsq is
  port (pr : buffer std_logic_vector(1 to 1); v : out std_logic);
end dffhywcsq;

architecture uozspzdz of dffhywcsq is
  
begin
  -- Multi-driven assignments
  v <= '0';
  v <= v;
  pr <= pr;
  v <= 'L';
end uozspzdz;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (igmyxju : out std_logic; qjxirq : buffer time_vector(1 to 1));
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture gkeijlz of o is
  signal wibsh : std_logic;
  signal ulmlckuowe : std_logic_vector(1 to 1);
begin
  sbzamg : entity work.dffhywcsq
    port map (pr => ulmlckuowe, v => wibsh);
  
  -- Single-driven assignments
  qjxirq <= (others => 1.342 ns);
end gkeijlz;



-- Seed after: 16802400083930053239,3042374792655995433
