-- Seed: 8643542386785772650,2338584220606314193

library ieee;
use ieee.std_logic_1164.all;

entity urj is
  port (em : in std_logic_vector(2 to 1));
end urj;

architecture teedikmxt of urj is
  
begin
  
end teedikmxt;

library ieee;
use ieee.std_logic_1164.all;

entity vajnr is
  port (vnnmbmj : in std_logic_vector(3 downto 0); w : buffer time);
end vajnr;

library ieee;
use ieee.std_logic_1164.all;

architecture m of vajnr is
  signal xzwiy : std_logic_vector(2 to 1);
begin
  biar : entity work.urj
    port map (em => xzwiy);
  xlltnoe : entity work.urj
    port map (em => xzwiy);
  jk : entity work.urj
    port map (em => xzwiy);
  
  -- Single-driven assignments
  w <= 1 hr;
  
  -- Multi-driven assignments
  xzwiy <= (others => '0');
  xzwiy <= xzwiy;
  xzwiy <= (others => '0');
end m;

library ieee;
use ieee.std_logic_1164.all;

entity jyevw is
  port (txnsrwfg : buffer std_logic; e : buffer std_logic_vector(4 to 1));
end jyevw;

library ieee;
use ieee.std_logic_1164.all;

architecture qtpyfom of jyevw is
  signal xvnw : std_logic_vector(2 to 1);
begin
  jpzg : entity work.urj
    port map (em => xvnw);
end qtpyfom;



-- Seed after: 17365451105482415804,2338584220606314193
