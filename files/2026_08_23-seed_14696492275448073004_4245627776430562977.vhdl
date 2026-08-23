-- Seed: 14696492275448073004,4245627776430562977

entity te is
  port (ei : inout bit; r : inout integer_vector(0 downto 4); xjwi : linkage time; nk : inout severity_level);
end te;

architecture qtce of te is
  
begin
  -- Single-driven assignments
  nk <= nk;
end qtce;

library ieee;
use ieee.std_logic_1164.all;

entity drt is
  port (mgudxbcv : buffer std_logic; qcjw : linkage real; jsmmupjz : buffer real_vector(4 to 4));
end drt;

architecture ictvzyft of drt is
  signal rwyenfguj : severity_level;
  signal lihj : time;
  signal fpo : integer_vector(0 downto 4);
  signal lkvqc : bit;
  signal ozxxaafma : severity_level;
  signal zrpovptp : time;
  signal zmqltgwe : integer_vector(0 downto 4);
  signal soougtv : bit;
  signal bxsy : severity_level;
  signal sgo : time;
  signal l : integer_vector(0 downto 4);
  signal zm : bit;
begin
  t : entity work.te
    port map (ei => zm, r => l, xjwi => sgo, nk => bxsy);
  tizsgnjz : entity work.te
    port map (ei => soougtv, r => zmqltgwe, xjwi => zrpovptp, nk => ozxxaafma);
  bbgin : entity work.te
    port map (ei => lkvqc, r => fpo, xjwi => lihj, nk => rwyenfguj);
  
  -- Single-driven assignments
  jsmmupjz <= (others => 16#6.31C67#);
  
  -- Multi-driven assignments
  mgudxbcv <= mgudxbcv;
  mgudxbcv <= '-';
end ictvzyft;

library ieee;
use ieee.std_logic_1164.all;

entity lsdnk is
  port (ztysbvx : inout std_logic; j : inout std_logic_vector(4 downto 0); fvlwtp : in std_logic_vector(4 to 0));
end lsdnk;

architecture bkppc of lsdnk is
  signal lgdg : severity_level;
  signal plnnffotx : time;
  signal nofouu : integer_vector(0 downto 4);
  signal zfocfpeme : bit;
  signal fsgbms : severity_level;
  signal ymemz : time;
  signal ryouvku : integer_vector(0 downto 4);
  signal ptqiqyq : bit;
  signal itdpu : severity_level;
  signal wniiigsm : time;
  signal w : integer_vector(0 downto 4);
  signal tmfhobagc : bit;
begin
  dcn : entity work.te
    port map (ei => tmfhobagc, r => w, xjwi => wniiigsm, nk => itdpu);
  zu : entity work.te
    port map (ei => ptqiqyq, r => ryouvku, xjwi => ymemz, nk => fsgbms);
  pjsuzj : entity work.te
    port map (ei => zfocfpeme, r => nofouu, xjwi => plnnffotx, nk => lgdg);
  
  -- Multi-driven assignments
  j <= ('U', '0', 'W', 'X', '1');
  j <= ('L', 'H', 'W', '0', 'L');
  j <= ('U', '0', 'H', '0', '1');
  ztysbvx <= 'W';
end bkppc;

library ieee;
use ieee.std_logic_1164.all;

entity opswwus is
  port (wnojuj : out std_logic_vector(0 downto 2); a : buffer std_logic);
end opswwus;

library ieee;
use ieee.std_logic_1164.all;

architecture y of opswwus is
  signal jcvek : real_vector(4 to 4);
  signal ktlhbbfqk : real;
  signal rlgpk : std_logic;
  signal dtegnz : severity_level;
  signal fsb : time;
  signal se : integer_vector(0 downto 4);
  signal yjnviher : bit;
  signal uwthf : severity_level;
  signal hfrepgtcr : time;
  signal rcrorktlqs : integer_vector(0 downto 4);
  signal npykmwyvh : bit;
  signal uweyvr : std_logic_vector(4 to 0);
  signal w : std_logic_vector(4 downto 0);
  signal uwsacgp : std_logic;
begin
  wjxsgvg : entity work.lsdnk
    port map (ztysbvx => uwsacgp, j => w, fvlwtp => uweyvr);
  rthueglv : entity work.te
    port map (ei => npykmwyvh, r => rcrorktlqs, xjwi => hfrepgtcr, nk => uwthf);
  bulfrekl : entity work.te
    port map (ei => yjnviher, r => se, xjwi => fsb, nk => dtegnz);
  iqlbgn : entity work.drt
    port map (mgudxbcv => rlgpk, qcjw => ktlhbbfqk, jsmmupjz => jcvek);
end y;



-- Seed after: 3317017599412335456,4245627776430562977
