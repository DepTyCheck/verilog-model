-- Seed: 1293785000252629794,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity yu is
  port (sxko : inout std_logic_vector(4 downto 4); uihdzqwxq : out boolean; cxtbnni : inout std_logic);
end yu;

architecture a of yu is
  
begin
  -- Single-driven assignments
  uihdzqwxq <= TRUE;
  
  -- Multi-driven assignments
  cxtbnni <= 'L';
  cxtbnni <= cxtbnni;
end a;

library ieee;
use ieee.std_logic_1164.all;

entity xoey is
  port (law : inout std_logic_vector(0 to 2); qcbxsjqru : linkage time; bw : in real);
end xoey;

library ieee;
use ieee.std_logic_1164.all;

architecture u of xoey is
  signal sqajenumdv : std_logic;
  signal s : boolean;
  signal qbwgesjm : std_logic_vector(4 downto 4);
begin
  dhnvq : entity work.yu
    port map (sxko => qbwgesjm, uihdzqwxq => s, cxtbnni => sqajenumdv);
  
  -- Multi-driven assignments
  law <= ('1', 'Z', 'U');
  sqajenumdv <= 'L';
  law <= law;
  law <= ('Z', '1', '0');
end u;

entity bgarxbsfvz is
  port (bien : inout bit; ppzvex : in time);
end bgarxbsfvz;

architecture kpgdluov of bgarxbsfvz is
  
begin
  
end kpgdluov;

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (crjzi : linkage std_logic_vector(0 downto 0));
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture dd of i is
  signal xnfzbwtr : std_logic;
  signal klcyrgy : boolean;
  signal lhinvzq : time;
  signal xuvzb : bit;
  signal oujorvre : std_logic;
  signal nw : boolean;
  signal kklvm : std_logic_vector(4 downto 4);
begin
  qcgqt : entity work.yu
    port map (sxko => kklvm, uihdzqwxq => nw, cxtbnni => oujorvre);
  somtthdskc : entity work.bgarxbsfvz
    port map (bien => xuvzb, ppzvex => lhinvzq);
  cuw : entity work.yu
    port map (sxko => kklvm, uihdzqwxq => klcyrgy, cxtbnni => xnfzbwtr);
  
  -- Single-driven assignments
  lhinvzq <= 1_3_0.0200 ps;
  
  -- Multi-driven assignments
  kklvm <= (others => '1');
  kklvm <= "L";
  kklvm <= kklvm;
end dd;



-- Seed after: 7963359782857193820,7808623373429384027
