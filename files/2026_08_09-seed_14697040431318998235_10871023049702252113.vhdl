-- Seed: 14697040431318998235,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity ayv is
  port (psigt : linkage real_vector(3 to 0); csdpwtdal : in integer; zdno : inout std_logic_vector(4 downto 3); pdxmz : buffer boolean_vector(3 to 1));
end ayv;

architecture n of ayv is
  
begin
  -- Single-driven assignments
  pdxmz <= (others => TRUE);
  
  -- Multi-driven assignments
  zdno <= zdno;
  zdno <= zdno;
  zdno <= ('H', '0');
  zdno <= ('0', '0');
end n;

library ieee;
use ieee.std_logic_1164.all;

entity fgaigwo is
  port (fjkfqa : out std_logic_vector(2 to 4); x : linkage boolean);
end fgaigwo;

library ieee;
use ieee.std_logic_1164.all;

architecture vfodl of fgaigwo is
  signal vptfgadylt : boolean_vector(3 to 1);
  signal khj : std_logic_vector(4 downto 3);
  signal h : integer;
  signal zoiptfluiu : real_vector(3 to 0);
  signal tmjxomieg : boolean_vector(3 to 1);
  signal elgzobo : std_logic_vector(4 downto 3);
  signal zmfxeuz : real_vector(3 to 0);
  signal pnqd : boolean_vector(3 to 1);
  signal tlisv : real_vector(3 to 0);
  signal d : boolean_vector(3 to 1);
  signal mciitaq : std_logic_vector(4 downto 3);
  signal ygnlpg : integer;
  signal rj : real_vector(3 to 0);
begin
  yzqa : entity work.ayv
    port map (psigt => rj, csdpwtdal => ygnlpg, zdno => mciitaq, pdxmz => d);
  sirb : entity work.ayv
    port map (psigt => tlisv, csdpwtdal => ygnlpg, zdno => mciitaq, pdxmz => pnqd);
  ivwx : entity work.ayv
    port map (psigt => zmfxeuz, csdpwtdal => ygnlpg, zdno => elgzobo, pdxmz => tmjxomieg);
  bkrix : entity work.ayv
    port map (psigt => zoiptfluiu, csdpwtdal => h, zdno => khj, pdxmz => vptfgadylt);
  
  -- Single-driven assignments
  h <= 16#6_B_4#;
  ygnlpg <= h;
  
  -- Multi-driven assignments
  elgzobo <= khj;
  fjkfqa <= "Z1H";
  fjkfqa <= ('L', '1', 'W');
end vfodl;



-- Seed after: 8319505796236555405,10871023049702252113
