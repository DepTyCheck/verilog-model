-- Seed: 18176348763962101727,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (vpiubzqfs : in std_logic; uy : buffer integer_vector(3 downto 0));
end j;

architecture yvxjgkq of j is
  
begin
  
end yvxjgkq;

library ieee;
use ieee.std_logic_1164.all;

entity swv is
  port (hhuj : linkage std_logic_vector(1 to 1); busz : out std_logic; fnepytl : out boolean; mztdbyr : linkage std_logic);
end swv;

library ieee;
use ieee.std_logic_1164.all;

architecture w of swv is
  signal qbcvkcw : integer_vector(3 downto 0);
  signal yvkrlz : std_logic;
  signal olf : integer_vector(3 downto 0);
  signal fiq : std_logic;
  signal jmkwngm : integer_vector(3 downto 0);
begin
  qymzo : entity work.j
    port map (vpiubzqfs => busz, uy => jmkwngm);
  yefeevog : entity work.j
    port map (vpiubzqfs => fiq, uy => olf);
  yebndauxh : entity work.j
    port map (vpiubzqfs => yvkrlz, uy => qbcvkcw);
  
  -- Single-driven assignments
  fnepytl <= TRUE;
  
  -- Multi-driven assignments
  busz <= 'L';
end w;

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (e : out std_logic_vector(1 to 2); orh : out std_logic_vector(0 downto 2); vivyuvehol : out std_logic_vector(4 to 0); fbpvu : inout real);
end c;

library ieee;
use ieee.std_logic_1164.all;

architecture y of c is
  signal kkpj : std_logic;
  signal w : boolean;
  signal te : std_logic_vector(1 to 1);
  signal ncvsw : integer_vector(3 downto 0);
  signal zsu : std_logic;
begin
  fwawgoep : entity work.j
    port map (vpiubzqfs => zsu, uy => ncvsw);
  tpjrn : entity work.swv
    port map (hhuj => te, busz => zsu, fnepytl => w, mztdbyr => kkpj);
  
  -- Single-driven assignments
  fbpvu <= 16#6842.A#;
  
  -- Multi-driven assignments
  vivyuvehol <= "";
  te <= (others => 'U');
  te <= (others => '-');
end y;

library ieee;
use ieee.std_logic_1164.all;

entity xvcgj is
  port (v : buffer std_logic_vector(2 to 4));
end xvcgj;

library ieee;
use ieee.std_logic_1164.all;

architecture r of xvcgj is
  signal ludisx : integer_vector(3 downto 0);
  signal bfngdassq : std_logic;
begin
  kpitlzm : entity work.j
    port map (vpiubzqfs => bfngdassq, uy => ludisx);
end r;



-- Seed after: 9185573108047300244,13694093582652240945
