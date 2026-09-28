-- Seed: 11062313108132052458,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity jeetzikllt is
  port (di : buffer std_logic_vector(2 to 3));
end jeetzikllt;

architecture i of jeetzikllt is
  
begin
  -- Multi-driven assignments
  di <= di;
  di <= "UU";
  di <= di;
end i;

library ieee;
use ieee.std_logic_1164.all;

entity cqtrteos is
  port (yagv : linkage integer; avouie : out std_logic_vector(0 downto 2));
end cqtrteos;

library ieee;
use ieee.std_logic_1164.all;

architecture rh of cqtrteos is
  signal smoggjizcz : std_logic_vector(2 to 3);
  signal hy : std_logic_vector(2 to 3);
  signal kiaem : std_logic_vector(2 to 3);
begin
  i : entity work.jeetzikllt
    port map (di => kiaem);
  dxvy : entity work.jeetzikllt
    port map (di => hy);
  brzyrixbcg : entity work.jeetzikllt
    port map (di => smoggjizcz);
  
  -- Multi-driven assignments
  avouie <= (others => '0');
  avouie <= (others => '0');
end rh;

entity bmlpqtk is
  port (pglia : buffer time; fvbqq : linkage real_vector(0 to 3));
end bmlpqtk;

library ieee;
use ieee.std_logic_1164.all;

architecture bx of bmlpqtk is
  signal si : std_logic_vector(0 downto 2);
  signal xzyhib : integer;
  signal v : std_logic_vector(2 to 3);
begin
  koobe : entity work.jeetzikllt
    port map (di => v);
  sdvkfashiy : entity work.cqtrteos
    port map (yagv => xzyhib, avouie => si);
  
  -- Single-driven assignments
  pglia <= 4 min;
  
  -- Multi-driven assignments
  si <= (others => '0');
  v <= ('Z', 'H');
  v <= ('1', 'X');
  v <= v;
end bx;



-- Seed after: 8621171018050694988,7311216359267151659
