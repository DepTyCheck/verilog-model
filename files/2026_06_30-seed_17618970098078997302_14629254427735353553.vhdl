-- Seed: 17618970098078997302,14629254427735353553

entity fjfpkyx is
  port (bn : in integer; shnprxvx : in real; im : inout integer; llwcso : buffer bit_vector(2 downto 3));
end fjfpkyx;

architecture tpmzncpc of fjfpkyx is
  
begin
  -- Single-driven assignments
  llwcso <= (others => '0');
  im <= 3_4_3_1;
end tpmzncpc;

library ieee;
use ieee.std_logic_1164.all;

entity yxbmil is
  port (aq : buffer severity_level; yujqa : buffer std_logic_vector(2 downto 1));
end yxbmil;

architecture mtf of yxbmil is
  signal zyzf : bit_vector(2 downto 3);
  signal ph : real;
  signal xmsvphpot : integer;
  signal jorhkf : bit_vector(2 downto 3);
  signal hzehgi : integer;
  signal tge : real;
  signal ciervw : integer;
begin
  rkgxezon : entity work.fjfpkyx
    port map (bn => ciervw, shnprxvx => tge, im => hzehgi, llwcso => jorhkf);
  gogrci : entity work.fjfpkyx
    port map (bn => xmsvphpot, shnprxvx => ph, im => ciervw, llwcso => zyzf);
  
  -- Single-driven assignments
  xmsvphpot <= 2;
  aq <= FAILURE;
  
  -- Multi-driven assignments
  yujqa <= ('X', 'X');
  yujqa <= ('H', '0');
end mtf;

entity xlxcq is
  port (oqpqfugq : buffer real; otxnw : in severity_level; xgjygh : linkage integer; mnlhtgsv : buffer time);
end xlxcq;

library ieee;
use ieee.std_logic_1164.all;

architecture dxv of xlxcq is
  signal c : severity_level;
  signal qeioykab : bit_vector(2 downto 3);
  signal ix : integer;
  signal jqweny : real;
  signal tdedj : std_logic_vector(2 downto 1);
  signal djw : severity_level;
  signal pkzppm : bit_vector(2 downto 3);
  signal g : integer;
  signal tdfnaz : integer;
begin
  hbdppk : entity work.fjfpkyx
    port map (bn => tdfnaz, shnprxvx => oqpqfugq, im => g, llwcso => pkzppm);
  iyqmzw : entity work.yxbmil
    port map (aq => djw, yujqa => tdedj);
  odgdc : entity work.fjfpkyx
    port map (bn => tdfnaz, shnprxvx => jqweny, im => ix, llwcso => qeioykab);
  qc : entity work.yxbmil
    port map (aq => c, yujqa => tdedj);
  
  -- Single-driven assignments
  tdfnaz <= 0;
  oqpqfugq <= 2#1.000#;
  mnlhtgsv <= 4201 us;
  
  -- Multi-driven assignments
  tdedj <= "XH";
  tdedj <= ('W', 'H');
end dxv;

entity uck is
  port (qklmj : buffer severity_level; bsyckul : inout real; bwqknj : out boolean);
end uck;

library ieee;
use ieee.std_logic_1164.all;

architecture wrjrtewfx of uck is
  signal kjgf : bit_vector(2 downto 3);
  signal yrtsigyxkt : integer;
  signal pjkyqm : integer;
  signal fxtk : std_logic_vector(2 downto 1);
begin
  jzbaefutpv : entity work.yxbmil
    port map (aq => qklmj, yujqa => fxtk);
  mrib : entity work.fjfpkyx
    port map (bn => pjkyqm, shnprxvx => bsyckul, im => yrtsigyxkt, llwcso => kjgf);
  
  -- Single-driven assignments
  bwqknj <= FALSE;
  bsyckul <= 0211.2_3_2_2_0;
  
  -- Multi-driven assignments
  fxtk <= ('U', 'W');
  fxtk <= "X0";
  fxtk <= "W1";
  fxtk <= "-L";
end wrjrtewfx;



-- Seed after: 10796756755973391672,14629254427735353553
