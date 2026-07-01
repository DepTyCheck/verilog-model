-- Seed: 4681799953720181263,6882842853887419669

entity jm is
  port (ebuy : out time; der : linkage real; lpglbzpo : in boolean; bzmypunyq : buffer time);
end jm;

architecture ng of jm is
  
begin
  -- Single-driven assignments
  bzmypunyq <= 430 ps;
  ebuy <= 8#65074# ps;
end ng;

entity pksg is
  port (hsgbdxohw : out time_vector(1 to 1); mroeumd : in bit_vector(3 to 3); kpmeaqmh : buffer severity_level; usfurrwe : linkage bit);
end pksg;

architecture askv of pksg is
  signal xspgrzw : time;
  signal opjjfidt : boolean;
  signal lac : real;
  signal nqfegoe : time;
  signal tjuu : time;
  signal jy : real;
  signal w : time;
  signal prykckabrx : time;
  signal g : boolean;
  signal obpyoxil : real;
  signal osw : time;
begin
  wgkkls : entity work.jm
    port map (ebuy => osw, der => obpyoxil, lpglbzpo => g, bzmypunyq => prykckabrx);
  d : entity work.jm
    port map (ebuy => w, der => jy, lpglbzpo => g, bzmypunyq => tjuu);
  rwk : entity work.jm
    port map (ebuy => nqfegoe, der => lac, lpglbzpo => opjjfidt, bzmypunyq => xspgrzw);
  
  -- Single-driven assignments
  kpmeaqmh <= FAILURE;
  opjjfidt <= TRUE;
end askv;

library ieee;
use ieee.std_logic_1164.all;

entity aawmmekor is
  port (zlh : in std_logic_vector(1 to 2); t : buffer character);
end aawmmekor;

architecture hfuqhmext of aawmmekor is
  signal mnucp : time;
  signal yuqfd : boolean;
  signal zmrxtd : real;
  signal xc : time;
  signal xgwqvtdh : bit;
  signal egstsgwi : severity_level;
  signal ryczhmlzt : bit_vector(3 to 3);
  signal a : time_vector(1 to 1);
begin
  ciaeaerp : entity work.pksg
    port map (hsgbdxohw => a, mroeumd => ryczhmlzt, kpmeaqmh => egstsgwi, usfurrwe => xgwqvtdh);
  anevrbulim : entity work.jm
    port map (ebuy => xc, der => zmrxtd, lpglbzpo => yuqfd, bzmypunyq => mnucp);
  
  -- Single-driven assignments
  t <= 'b';
  yuqfd <= FALSE;
  ryczhmlzt <= (others => '0');
end hfuqhmext;

library ieee;
use ieee.std_logic_1164.all;

entity wsptzp is
  port (badwqz : buffer bit_vector(4 downto 2); enuqkqt : linkage real; igmy : in time; kwacouua : buffer std_logic);
end wsptzp;

architecture qjhdheno of wsptzp is
  signal uwnpn : bit;
  signal pwmol : severity_level;
  signal rzdbasfjym : bit_vector(3 to 3);
  signal url : time_vector(1 to 1);
begin
  ekseli : entity work.pksg
    port map (hsgbdxohw => url, mroeumd => rzdbasfjym, kpmeaqmh => pwmol, usfurrwe => uwnpn);
  
  -- Single-driven assignments
  rzdbasfjym <= (others => '1');
  badwqz <= ('1', '0', '1');
  
  -- Multi-driven assignments
  kwacouua <= '-';
  kwacouua <= 'W';
  kwacouua <= 'H';
  kwacouua <= 'U';
end qjhdheno;



-- Seed after: 3113953841954038692,6882842853887419669
