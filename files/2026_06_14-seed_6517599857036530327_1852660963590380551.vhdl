-- Seed: 6517599857036530327,1852660963590380551

library ieee;
use ieee.std_logic_1164.all;

entity fehwz is
  port (axqgfvqeuj : inout integer; ekqdkyk : in std_logic; sos : linkage std_logic);
end fehwz;



architecture k of fehwz is
  
begin
  
end k;



entity wsn is
  port (gsffzgkti : inout boolean_vector(2 to 2));
end wsn;

library ieee;
use ieee.std_logic_1164.all;

architecture hhfkte of wsn is
  signal vgrafjyxy : std_logic;
  signal ifeipja : integer;
begin
  ns : entity work.fehwz
    port map (axqgfvqeuj => ifeipja, ekqdkyk => vgrafjyxy, sos => vgrafjyxy);
end hhfkte;

library ieee;
use ieee.std_logic_1164.all;

entity mpsq is
  port (isrnw : linkage std_logic_vector(4 downto 4); e : out integer; bl : in boolean_vector(4 to 3));
end mpsq;

library ieee;
use ieee.std_logic_1164.all;

architecture x of mpsq is
  signal ak : boolean_vector(2 to 2);
  signal ql : boolean_vector(2 to 2);
  signal ydflygmkpv : boolean_vector(2 to 2);
  signal piat : std_logic;
begin
  foanzqx : entity work.fehwz
    port map (axqgfvqeuj => e, ekqdkyk => piat, sos => piat);
  poyxct : entity work.wsn
    port map (gsffzgkti => ydflygmkpv);
  imhmcyxlg : entity work.wsn
    port map (gsffzgkti => ql);
  n : entity work.wsn
    port map (gsffzgkti => ak);
end x;



-- Seed after: 17569105628526245025,1852660963590380551
