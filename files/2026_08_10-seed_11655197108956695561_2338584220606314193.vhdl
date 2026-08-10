-- Seed: 11655197108956695561,2338584220606314193

library ieee;
use ieee.std_logic_1164.all;

entity puuuplvdf is
  port (ssdwnstqgf : out std_logic);
end puuuplvdf;

architecture zyuvxcpc of puuuplvdf is
  
begin
  -- Multi-driven assignments
  ssdwnstqgf <= '0';
end zyuvxcpc;

entity khmmi is
  port (fo : inout real_vector(4 downto 2); xhstt : inout time);
end khmmi;

architecture gccysyi of khmmi is
  
begin
  -- Single-driven assignments
  fo <= fo;
  xhstt <= xhstt;
end gccysyi;

entity f is
  port (qi : in boolean_vector(3 downto 1));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture vrjlhbbnya of f is
  signal dzrnqz : std_logic;
  signal spymwq : std_logic;
  signal ergnmph : time;
  signal grxtxgccox : real_vector(4 downto 2);
  signal kilaemzcvf : time;
  signal r : real_vector(4 downto 2);
begin
  pk : entity work.khmmi
    port map (fo => r, xhstt => kilaemzcvf);
  fa : entity work.khmmi
    port map (fo => grxtxgccox, xhstt => ergnmph);
  oeorovxrt : entity work.puuuplvdf
    port map (ssdwnstqgf => spymwq);
  gydypyd : entity work.puuuplvdf
    port map (ssdwnstqgf => dzrnqz);
  
  -- Multi-driven assignments
  spymwq <= '1';
end vrjlhbbnya;



-- Seed after: 17683709221726189097,2338584220606314193
