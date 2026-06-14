-- Seed: 17989000694512547295,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity nn is
  port (bjyh : buffer std_logic; yrhylablbl : inout integer_vector(3 downto 2));
end nn;

architecture muuivvljpq of nn is
  
begin
  -- Single-driven assignments
  yrhylablbl <= (342, 16#7_2#);
  
  -- Multi-driven assignments
  bjyh <= 'W';
end muuivvljpq;

library ieee;
use ieee.std_logic_1164.all;

entity cykcz is
  port (hmrwzl : buffer std_logic_vector(3 downto 3); dp : out bit);
end cykcz;

library ieee;
use ieee.std_logic_1164.all;

architecture ujiq of cykcz is
  signal ikz : integer_vector(3 downto 2);
  signal clvifqt : std_logic;
begin
  arbx : entity work.nn
    port map (bjyh => clvifqt, yrhylablbl => ikz);
  
  -- Single-driven assignments
  dp <= '1';
  
  -- Multi-driven assignments
  hmrwzl <= (others => 'Z');
  hmrwzl <= "L";
  hmrwzl <= (others => '0');
  clvifqt <= 'X';
end ujiq;

library ieee;
use ieee.std_logic_1164.all;

entity nwwoe is
  port (argr : inout integer; paahp : in std_logic_vector(1 to 2));
end nwwoe;

library ieee;
use ieee.std_logic_1164.all;

architecture dhlbyxfzf of nwwoe is
  signal lhefbhoup : integer_vector(3 downto 2);
  signal vgroaeyc : std_logic;
  signal k : bit;
  signal rxjf : std_logic_vector(3 downto 3);
  signal ohv : integer_vector(3 downto 2);
  signal lznzduq : std_logic;
  signal emv : integer_vector(3 downto 2);
  signal asehhlix : std_logic;
begin
  xkff : entity work.nn
    port map (bjyh => asehhlix, yrhylablbl => emv);
  ohauo : entity work.nn
    port map (bjyh => lznzduq, yrhylablbl => ohv);
  oxotge : entity work.cykcz
    port map (hmrwzl => rxjf, dp => k);
  gq : entity work.nn
    port map (bjyh => vgroaeyc, yrhylablbl => lhefbhoup);
  
  -- Single-driven assignments
  argr <= 03;
end dhlbyxfzf;



-- Seed after: 15501962893022634460,14652815260262078753
