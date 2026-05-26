-- Seed: 8910065776497624192,8089241273282434469

library ieee;
use ieee.std_logic_1164.all;

entity nhn is
  port (fq : buffer std_logic_vector(4 downto 3); yjspwn : out severity_level; bbptwgvnv : inout time);
end nhn;



architecture auca of nhn is
  
begin
  
end auca;



entity mpqp is
  port (ysoelqqb : out real_vector(1 to 4); skymzjoz : buffer integer_vector(4 to 3); clyhath : out real);
end mpqp;

library ieee;
use ieee.std_logic_1164.all;

architecture o of mpqp is
  signal f : time;
  signal u : severity_level;
  signal ivvdfw : time;
  signal fqqqbrdu : severity_level;
  signal geegw : std_logic_vector(4 downto 3);
  signal nexwywx : time;
  signal mqmbpjn : severity_level;
  signal tev : std_logic_vector(4 downto 3);
begin
  rh : entity work.nhn
    port map (fq => tev, yjspwn => mqmbpjn, bbptwgvnv => nexwywx);
  zh : entity work.nhn
    port map (fq => geegw, yjspwn => fqqqbrdu, bbptwgvnv => ivvdfw);
  coidver : entity work.nhn
    port map (fq => geegw, yjspwn => u, bbptwgvnv => f);
end o;



entity qzhici is
  port (ytgcn : linkage time_vector(0 to 0));
end qzhici;

library ieee;
use ieee.std_logic_1164.all;

architecture exlvkkxtj of qzhici is
  signal hfkopul : time;
  signal zqazbgjmp : severity_level;
  signal qolswatzt : std_logic_vector(4 downto 3);
  signal yvkgrrykuh : time;
  signal bfcodlriv : severity_level;
  signal njjmxax : std_logic_vector(4 downto 3);
  signal ptedo : real;
  signal zobhpux : integer_vector(4 to 3);
  signal vrpgbrceo : real_vector(1 to 4);
begin
  yzqxnjv : entity work.mpqp
    port map (ysoelqqb => vrpgbrceo, skymzjoz => zobhpux, clyhath => ptedo);
  hwsakeld : entity work.nhn
    port map (fq => njjmxax, yjspwn => bfcodlriv, bbptwgvnv => yvkgrrykuh);
  rwerw : entity work.nhn
    port map (fq => qolswatzt, yjspwn => zqazbgjmp, bbptwgvnv => hfkopul);
end exlvkkxtj;



entity cynxnce is
  port (ivnvkof : out real);
end cynxnce;

library ieee;
use ieee.std_logic_1164.all;

architecture cankfrcmqw of cynxnce is
  signal xtwhb : time_vector(0 to 0);
  signal gbkgauuee : time;
  signal h : severity_level;
  signal bodlc : time;
  signal smfvvw : severity_level;
  signal bqgvtfpuj : std_logic_vector(4 downto 3);
  signal uyvirazw : integer_vector(4 to 3);
  signal ddaarnrlex : real_vector(1 to 4);
begin
  gaagfp : entity work.mpqp
    port map (ysoelqqb => ddaarnrlex, skymzjoz => uyvirazw, clyhath => ivnvkof);
  nzph : entity work.nhn
    port map (fq => bqgvtfpuj, yjspwn => smfvvw, bbptwgvnv => bodlc);
  dj : entity work.nhn
    port map (fq => bqgvtfpuj, yjspwn => h, bbptwgvnv => gbkgauuee);
  n : entity work.qzhici
    port map (ytgcn => xtwhb);
end cankfrcmqw;



-- Seed after: 14452984732707121137,8089241273282434469
