-- Seed: 2086591082795499518,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (twz : inout std_logic_vector(4 to 3); recjpe : linkage integer; qbwipfld : buffer time; epqgk : out std_logic);
end u;

architecture d of u is
  
begin
  -- Single-driven assignments
  qbwipfld <= 2#11111# ps;
  
  -- Multi-driven assignments
  twz <= "";
  epqgk <= 'X';
end d;

library ieee;
use ieee.std_logic_1164.all;

entity ewjmlt is
  port (as : in severity_level; ykjrvkqnkc : linkage time_vector(0 to 0); kz : in std_logic_vector(4 to 0));
end ewjmlt;

architecture tivayms of ewjmlt is
  
begin
  
end tivayms;

entity boebikvlmt is
  port (m : in time; pevaa : inout real_vector(4 to 3));
end boebikvlmt;

library ieee;
use ieee.std_logic_1164.all;

architecture frlud of boebikvlmt is
  signal bues : std_logic_vector(4 to 0);
  signal l : time_vector(0 to 0);
  signal ayw : severity_level;
begin
  lnyvrihha : entity work.ewjmlt
    port map (as => ayw, ykjrvkqnkc => l, kz => bues);
  
  -- Single-driven assignments
  pevaa <= (others => 0.0);
  
  -- Multi-driven assignments
  bues <= "";
  bues <= "";
  bues <= (others => '0');
  bues <= "";
end frlud;

entity xrguab is
  port (e : buffer integer_vector(0 downto 0); xxya : in integer; kiqemzaiv : inout time);
end xrguab;

library ieee;
use ieee.std_logic_1164.all;

architecture wrc of xrguab is
  signal oivntoyl : real_vector(4 to 3);
  signal jarhghwj : std_logic;
  signal ju : integer;
  signal gpzy : std_logic_vector(4 to 3);
begin
  vmdn : entity work.u
    port map (twz => gpzy, recjpe => ju, qbwipfld => kiqemzaiv, epqgk => jarhghwj);
  vplg : entity work.boebikvlmt
    port map (m => kiqemzaiv, pevaa => oivntoyl);
  
  -- Multi-driven assignments
  gpzy <= "";
  gpzy <= "";
end wrc;



-- Seed after: 9206903679588285633,13479070923501788437
