-- Seed: 10524531278356052963,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity pn is
  port (xk : out std_logic_vector(0 to 4); w : linkage std_logic_vector(1 downto 3); hi : inout integer_vector(1 to 0));
end pn;

architecture bjb of pn is
  
begin
  -- Single-driven assignments
  hi <= hi;
  
  -- Multi-driven assignments
  xk <= ('X', 'H', 'Z', 'H', 'X');
  xk <= xk;
  xk <= "WLHLX";
  xk <= ('0', 'L', 'W', '1', 'H');
end bjb;

library ieee;
use ieee.std_logic_1164.all;

entity wnbmbioxd is
  port (jba : linkage real; qypriqtd : inout real; iukulgh : linkage real; kbt : buffer std_logic_vector(1 downto 2));
end wnbmbioxd;

library ieee;
use ieee.std_logic_1164.all;

architecture epfw of wnbmbioxd is
  signal kzfhtlwgpy : integer_vector(1 to 0);
  signal bhdakkqpdd : std_logic_vector(1 downto 3);
  signal evzpvvi : std_logic_vector(0 to 4);
begin
  awae : entity work.pn
    port map (xk => evzpvvi, w => bhdakkqpdd, hi => kzfhtlwgpy);
  
  -- Single-driven assignments
  qypriqtd <= qypriqtd;
  
  -- Multi-driven assignments
  kbt <= (others => '0');
end epfw;

library ieee;
use ieee.std_logic_1164.all;

entity hct is
  port (vyjase : out std_logic);
end hct;

library ieee;
use ieee.std_logic_1164.all;

architecture latnhd of hct is
  signal fkmrm : std_logic_vector(1 downto 2);
  signal kiz : real;
  signal ogkknn : real;
  signal mbirldumbu : real;
  signal wqngnd : integer_vector(1 to 0);
  signal htsx : std_logic_vector(0 to 4);
  signal v : std_logic_vector(1 downto 3);
  signal blfwc : real;
  signal cd : real;
  signal staktu : real;
begin
  e : entity work.wnbmbioxd
    port map (jba => staktu, qypriqtd => cd, iukulgh => blfwc, kbt => v);
  fmwuu : entity work.pn
    port map (xk => htsx, w => v, hi => wqngnd);
  ua : entity work.wnbmbioxd
    port map (jba => mbirldumbu, qypriqtd => ogkknn, iukulgh => kiz, kbt => fkmrm);
  
  -- Multi-driven assignments
  vyjase <= '1';
end latnhd;



-- Seed after: 11607042533508258600,499459191852795575
