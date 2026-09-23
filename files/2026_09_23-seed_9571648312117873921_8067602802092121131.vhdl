-- Seed: 9571648312117873921,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity oqyyy is
  port (qwpzy : buffer std_logic; fpbrntqipi : out std_logic);
end oqyyy;

architecture atsq of oqyyy is
  
begin
  -- Multi-driven assignments
  qwpzy <= fpbrntqipi;
end atsq;

entity uuvsrjuo is
  port (qxsjobj : out integer);
end uuvsrjuo;

library ieee;
use ieee.std_logic_1164.all;

architecture dyuiicxzuo of uuvsrjuo is
  signal g : std_logic;
  signal r : std_logic;
  signal da : std_logic;
  signal lhjywerqm : std_logic;
begin
  kghtg : entity work.oqyyy
    port map (qwpzy => lhjywerqm, fpbrntqipi => da);
  exxz : entity work.oqyyy
    port map (qwpzy => r, fpbrntqipi => g);
  
  -- Multi-driven assignments
  g <= lhjywerqm;
  lhjywerqm <= 'Z';
  lhjywerqm <= '0';
end dyuiicxzuo;

entity yne is
  port (l : buffer string(3 downto 3); vjvtooqdbe : in integer; fm : buffer integer);
end yne;

library ieee;
use ieee.std_logic_1164.all;

architecture ikqlimgwa of yne is
  signal avm : integer;
  signal knzhduwsz : integer;
  signal gljmfuw : std_logic;
  signal qbbx : std_logic;
begin
  imxrrkldp : entity work.oqyyy
    port map (qwpzy => qbbx, fpbrntqipi => gljmfuw);
  xau : entity work.uuvsrjuo
    port map (qxsjobj => fm);
  pnhvt : entity work.uuvsrjuo
    port map (qxsjobj => knzhduwsz);
  tuzltxjj : entity work.uuvsrjuo
    port map (qxsjobj => avm);
  
  -- Multi-driven assignments
  qbbx <= qbbx;
  qbbx <= qbbx;
  gljmfuw <= qbbx;
end ikqlimgwa;

library ieee;
use ieee.std_logic_1164.all;

entity golapyapxu is
  port (xxoky : inout time; b : in std_logic; qongcyeq : buffer std_logic_vector(3 to 3));
end golapyapxu;

architecture uwyder of golapyapxu is
  signal pkkkgav : integer;
begin
  sj : entity work.uuvsrjuo
    port map (qxsjobj => pkkkgav);
  
  -- Single-driven assignments
  xxoky <= 3 ps;
  
  -- Multi-driven assignments
  qongcyeq <= "U";
  qongcyeq <= "H";
  qongcyeq <= "-";
end uwyder;



-- Seed after: 306737760478510979,8067602802092121131
