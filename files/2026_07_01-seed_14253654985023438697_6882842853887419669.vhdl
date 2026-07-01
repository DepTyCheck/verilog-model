-- Seed: 14253654985023438697,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity vs is
  port (bj : out std_logic; f : buffer time; ziqwk : out severity_level; zdqpdaxh : buffer std_logic_vector(3 to 4));
end vs;

architecture bfz of vs is
  
begin
  -- Single-driven assignments
  ziqwk <= WARNING;
  f <= 8#4# ms;
  
  -- Multi-driven assignments
  zdqpdaxh <= "WW";
  zdqpdaxh <= "XU";
end bfz;

library ieee;
use ieee.std_logic_1164.all;

entity ilfvqqvs is
  port (ee : buffer std_logic; ldqvxijm : out std_logic; omknqknst : in integer);
end ilfvqqvs;

library ieee;
use ieee.std_logic_1164.all;

architecture euyyvxejb of ilfvqqvs is
  signal cxz : severity_level;
  signal r : time;
  signal gezm : severity_level;
  signal anbggxbe : time;
  signal gjidqsl : severity_level;
  signal zvti : time;
  signal zutbsbecod : std_logic;
  signal nquexghljb : std_logic_vector(3 to 4);
  signal esnkp : severity_level;
  signal g : time;
  signal o : std_logic;
begin
  nex : entity work.vs
    port map (bj => o, f => g, ziqwk => esnkp, zdqpdaxh => nquexghljb);
  hcvjcn : entity work.vs
    port map (bj => zutbsbecod, f => zvti, ziqwk => gjidqsl, zdqpdaxh => nquexghljb);
  q : entity work.vs
    port map (bj => o, f => anbggxbe, ziqwk => gezm, zdqpdaxh => nquexghljb);
  fdh : entity work.vs
    port map (bj => zutbsbecod, f => r, ziqwk => cxz, zdqpdaxh => nquexghljb);
  
  -- Multi-driven assignments
  ldqvxijm <= 'W';
end euyyvxejb;

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (d : out std_logic_vector(0 downto 1); fq : out time_vector(1 to 3); bamhuyfi : buffer boolean_vector(4 downto 2); hvhwg : linkage time);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture phqe of i is
  signal jpkq : integer;
  signal rz : std_logic;
begin
  osr : entity work.ilfvqqvs
    port map (ee => rz, ldqvxijm => rz, omknqknst => jpkq);
  
  -- Single-driven assignments
  bamhuyfi <= (TRUE, FALSE, TRUE);
  fq <= (0114.0_2_3_1_4 ps, 16#7_7_0_7.C_2_4# ps, 2#1_1_1.101# ns);
  
  -- Multi-driven assignments
  d <= (others => '0');
  d <= "";
  d <= "";
  d <= (others => '0');
end phqe;

library ieee;
use ieee.std_logic_1164.all;

entity jwqgm is
  port (brpyembhk : buffer integer; spwfw : in std_logic);
end jwqgm;

library ieee;
use ieee.std_logic_1164.all;

architecture pavgl of jwqgm is
  signal wsjx : std_logic_vector(3 to 4);
  signal env : severity_level;
  signal nsqebxv : time;
  signal dqqeertdcs : severity_level;
  signal bndjfaray : time;
  signal prkvskjy : std_logic;
  signal xdte : std_logic;
  signal sa : std_logic;
  signal k : std_logic_vector(3 to 4);
  signal dlw : severity_level;
  signal qhygghfmho : time;
  signal t : std_logic;
begin
  xkihxsuqm : entity work.vs
    port map (bj => t, f => qhygghfmho, ziqwk => dlw, zdqpdaxh => k);
  artgjiawr : entity work.ilfvqqvs
    port map (ee => sa, ldqvxijm => xdte, omknqknst => brpyembhk);
  niypck : entity work.vs
    port map (bj => prkvskjy, f => bndjfaray, ziqwk => dqqeertdcs, zdqpdaxh => k);
  edzsoqeydz : entity work.vs
    port map (bj => xdte, f => nsqebxv, ziqwk => env, zdqpdaxh => wsjx);
  
  -- Single-driven assignments
  brpyembhk <= 3_4;
end pavgl;



-- Seed after: 16503667710344979603,6882842853887419669
