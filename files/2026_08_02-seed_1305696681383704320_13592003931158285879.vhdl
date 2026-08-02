-- Seed: 1305696681383704320,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity tp is
  port (qrdk : linkage std_logic_vector(1 to 3); uaveklgc : out real; ix : in real);
end tp;

architecture bvknkxbns of tp is
  
begin
  
end bvknkxbns;

library ieee;
use ieee.std_logic_1164.all;

entity akkvg is
  port (gtkapvcyqz : inout std_logic_vector(3 downto 2));
end akkvg;

library ieee;
use ieee.std_logic_1164.all;

architecture tipnqq of akkvg is
  signal xilhd : real;
  signal nfoy : real;
  signal wxbupb : std_logic_vector(1 to 3);
  signal ivcyba : real;
  signal fjifljedk : std_logic_vector(1 to 3);
  signal kqtbxfkgee : real;
  signal icqzsavn : real;
  signal yimhjtot : std_logic_vector(1 to 3);
begin
  vpjiycnnm : entity work.tp
    port map (qrdk => yimhjtot, uaveklgc => icqzsavn, ix => kqtbxfkgee);
  wnixmrp : entity work.tp
    port map (qrdk => fjifljedk, uaveklgc => ivcyba, ix => icqzsavn);
  hxb : entity work.tp
    port map (qrdk => wxbupb, uaveklgc => nfoy, ix => xilhd);
  
  -- Single-driven assignments
  xilhd <= 0_0_1_2.1;
  kqtbxfkgee <= nfoy;
  
  -- Multi-driven assignments
  gtkapvcyqz <= gtkapvcyqz;
  yimhjtot <= "0ZU";
end tipnqq;

library ieee;
use ieee.std_logic_1164.all;

entity emn is
  port (hpjby : linkage boolean; mppw : in std_logic_vector(4 to 1); axqzxfjsbf : linkage std_logic_vector(3 downto 1); jmykvatedk : in real);
end emn;

library ieee;
use ieee.std_logic_1164.all;

architecture qs of emn is
  signal gasta : std_logic_vector(3 downto 2);
  signal vw : std_logic_vector(3 downto 2);
  signal ocgchjjcq : real;
  signal ogerluw : std_logic_vector(1 to 3);
begin
  hdruulax : entity work.tp
    port map (qrdk => ogerluw, uaveklgc => ocgchjjcq, ix => jmykvatedk);
  ukhy : entity work.akkvg
    port map (gtkapvcyqz => vw);
  cdxerqahs : entity work.akkvg
    port map (gtkapvcyqz => gasta);
end qs;



-- Seed after: 17532621780891432886,13592003931158285879
